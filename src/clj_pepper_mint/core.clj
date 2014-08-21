(ns clj-pepper-mint.core
  (:require [clj-http.client :as client]
            [clj-http.cookies :as cookies]
            [cheshire.core :refer [generate-string]])
  (:import (org.apache.http.impl.cookie BasicClientCookie))
  )

(def mint-domain "mint.com")
(def url-base "https://wwws.mint.com/")

;; (def url-base "http://localhost:9090/")
;; (def mint-domain "localhost")

(def user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/35.0.1916.153 Safari/537.36")
(def browser "chrome")
(def browser-version 35)
(def os-name "mac")
(def referrer (str "https://wwws.mint.com/login.event" 
                   "?task=L&messageId=1&country=US&nextPage=overview.event"))
(def static-headers {"User-Agent" user-agent
                     "X-Request-With" "XMLHttpRequest"
                     "X-NewRelic-ID" "UA4OVVFWGwEGV1VaBwc="
                     "Referrer" referrer})

(def json-form-url "bundledServiceController.xevent?legacy=false&token=")

(def conn-pool (clj-http.conn-mgr/make-reusable-conn-manager {:timeout 10 :threads 1}))

(defn- pget [creds url & [params options]]
  (client/get (str url-base url) 
              (merge {:cookie-store (:cookies @creds)
                      :query-params params
                      :headers static-headers
                      :connection-manager conn-pool
                      } options)))

(defn- pform [creds url data]
  (client/post (str url-base url) 
    {:accept :json
     :headers static-headers
     :character-encoding "utf-8"
     :form-params data
     :cookie-store (:cookies @creds)
     :as :json
     :connection-manager conn-pool
     :decompress-body false
     }))

(defn- next-request [creds] 
  (dosync 
    (let [id (:request-id @creds)]
      (alter creds assoc :request-id (inc id))
      (str id)))) 

(defn- pform-json [creds json]
  (let [req-id (next-request creds)
        url (str json-form-url (:token @creds))
        json-with-id (assoc json :id req-id)
        form-data {:input (generate-string [json-with-id])}
        ]
    (when-let [response (-> (pform creds url form-data) :body :response)]
      (:response (get response (keyword req-id)))
      )))

(defn- pjson [creds args]
  (when-let [resp (pget creds "getJsonData.xevent"
                    (assoc args :rnd (System/currentTimeMillis))
                    {:as :json})]
    (-> resp :body :set first :data)))

(defn- add-cookie 
  [creds ckey cval]
  (let [store (:cookies @creds)]
    (.addCookie store (doto (BasicClientCookie. ckey (str cval))
                              (.setDomain mint-domain)
                              (.setPath "/")
                              ))))

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
    (add-cookie creds "mintPN" (-> pod :body :mintPN))
    (when-let [result (pform creds "loginUserSubmit.xevent"
                         {:username user
                          :password pass
                          :task "L"
                          :browser browser
                          :browserVersion browser-version
                          :os os-name
                          })]
      (when-let [error (-> result :body :error)]
        (throw (Exception. (-> error :vError :copy))))
      (when-let [json (:body result)]
        (dosync 
          (alter creds assoc :login json)
          (alter creds assoc :token (-> json :sUser :token)))
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

(defn get-categories
  "Last valid categories"
  [creds]
  (pjson creds {:task "categories"}))

(defn get-tags
  "Last defined tags"
  [creds]
  (pjson creds {:task "tags"}))

(defn get-transactions
  "List transactions, optionally filtered with an options map:
    :accountId Filter by account
    :offset int offset into result set
  "
  [creds & [args]]
  (pjson creds (merge {:offset 0
                       :comparableType 8
                       :acctChanged "T"
                       :task "transactions"
                       } args)))

(defn create-transaction
  "Create a new cash transaction;
   to be used to fake transaction imports.
  NB: There is currently very little arg validation,
   and the server seems to silently reject issues, too :(
  Args should look like: {
   :accountId 1234 ; apparently ignored, but good to have, I guess?
   :amount 4.2
   :category {:id id :name name}
   :date \"MM/DD/YYYY\"
   :isExpense bool
   :isInvestment bool
   :merchant \"Merchant Name\"
   :note \"Note, if any\"
   :tags [1234, 5678] ; set of ids
  }
  :category is Optional; if not provided, will just show
   up as UNCATEGORIZED, it seems
  "
  [creds args]
  (let [form-base {:amount (:amount args)
                   :cashTxnType "on"
                   :date (:date args)
                   :isInvestment (:isInvestment args false)
                   :merchant (:merchant args)
                   :mtAccount (:accountId args)
                   :mtCashSplitPref 2          ;; ?
                   :mtCheckNo ""
                   :mtIsExpense (:isExpense args true)
                   :mtType "cash"
                   :note (:note args)
                   :task "txnadd"
                   :txnId ":0"                  ;; might be required
                   ;
                   :token (:token @creds)
                   }
        form2 (if (:tags args)
               (apply assoc form-base (interleave 
                                        (map (partial str "tag") (:tags args)) 
                                        (repeat 2)))
               form-base)

        form (if (:category args)
               (assoc form2 
                   :catId (:id (:category args))
                   :category (:name (:category args)))
               form2
               )
        ]
    (when-not (:amount form)
      (throw (Exception. ":amount is required")))
    (when-not (:date form)
      (throw (Exception. ":date is required")))
    (when-not (:merchant form)
      (throw (Exception. ":merchant is required")))
    (:body (pform creds "updateTransaction.xevent" form))
    ))

(defn delete-transaction
  "Delete a transaction by its id"
  [creds transaction-id]
  (:body (pform creds "updateTransaction.xevent" {:task "delete"
                                                  :txnId transaction-id
                                                  :token (:token @creds)
                                                  })))
