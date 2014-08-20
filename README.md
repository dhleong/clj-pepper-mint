# clj-pepper-mint

A Clojure library for mint.com, ported from my
[pepper-mint](https://github.com/dhleong/pepper-mint) library.
This is my first Clojure project, so it's liable to be rough
around the edges, not follow best practices, and/or experience
breaking changes frequently.

## Usage

```clojure
(if-let [creds (login "user" "pass")]
    (get-transactions creds))
```

## License

Copyright © 2014 Daniel Leong

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
