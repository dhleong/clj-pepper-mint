(ns clj-pepper-mint.core
  (:require [clj-http.client :as client]
            [clj-http.cookies :as cookies]
            [cheshire.core :refer [generate-string]])
  (:import (org.apache.http.impl.cookie BasicClientCookie))
  )

(def url-base "https://wwws.mint.com/")
(def user-agent "Mozilla/5.0  Macintosh; Intel Mac OS X 10_9_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/35.0.1916.153 Safari/537.36")
(def browser "chrome")
(def browser-version 35)
(def os-name "mac")
(def referrer (str "https://wwws.mint.com/login.event" 
                   "?task=L&messageId=1&country=US&nextPage=overview.event"))

(def json-form-url "bundledServiceController.xevent?legacy=false&token=")

(defn- pget [creds url & data]
  (client/get (str url-base url) {:cookie-store (:cookies @creds)}))

(defn- pform [creds url data]
  (client/post (str url-base url) 
    {:accept :json
     :headers {"User-Agent" user-agent
               "X-Request-With" "XMLHttpRequest"
               "X-NewRelic-ID" "UA4OVVFWGwEGV1VaBwc="
               "Referrer" referrer}
     :form-params data
     :cookie-store (:cookies @creds)
     :as :json
     }))

(defn next-request [creds] 
  (dosync 
    (let [id (:request-id @creds)]
      (alter creds assoc :request-id (inc id))
      (str id)))) 

(defn pform-json [creds json]
  (let [req-id (next-request creds)
        url (str json-form-url (:token @creds))
        json-with-id (assoc json :id req-id)
        form-data {:input (generate-string [json-with-id])}
        ]
    (when-let [response (:response (:body (pform creds url form-data)))]
      (:response (get response (keyword req-id)))
      )))

(defn- add-cookie 
  [creds ckey cval]
  (let [store (:cookies @creds)]
    (.addCookie store (BasicClientCookie. ckey (String/valueOf cval)))))

(defn new-credentials 
  "Create a new, empty credentials object"
  [] 
  (ref {:request-id 42 ; magic number? random number?
        :cookies (cookies/cookie-store)
        }))

(defn login
  "Login to Mint. Returns a credentials object for use with other functions"
  [user pass]

  (let [creds (new-credentials)
             _ (pget creds "login.event?task=L") 
             pod (pform creds "getUserPod.xevent" {:username user})]
    (add-cookie creds "mintPN" (:mintPN pod))
    (when-let [result (pform creds "loginUserSubmit.xevent"
                         {:username user
                          :password pass
                          :task "L"
                          :browser browser
                          :browserVersion browser-version
                          :os os-name
                          })]
      (when-let [error (:error (:body result))]
        (throw (Exception. (:copy (:vError error)))))
      (when-let [json (:body result)]
        (dosync 
          (alter creds assoc :login json)
          (alter creds assoc :token (:token (:sUser json))))
        creds))))

(defn get-accounts
  "List accounts associated with the credentials"
  [creds]
  (pform-json creds {:args {:types ["BANK"
                                    "CREDIT"
                                    "INVESTMENT"
                                    "LOAN"
                                    "MORTGAGE"
                                    "OTHER_PROPERTY"
                                    "REAL_ESTATE"
                                    "VEHICLE"
                                    "UNCLASSIFIED"]}
                     :service "MintAccountService"
                     :task "getAccountsSorted"
                     }))

