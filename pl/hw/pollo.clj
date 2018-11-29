(use '[clojure.test :rename {is test-is}])
(use '[clojure.core.logic :rename {== ===}])
(require '[clojure.core.logic.fd :as fd])

(defn counto
  "This logic function unifies result with the number of elements
  contained in lst."
  [lst result]
  (conde
    [(=== lst [])
     (=== result 0)]

    [(fresh [head tail temp]
            (conso head tail lst)
            (fd/+ temp 1 result)
            (counto tail temp))]))