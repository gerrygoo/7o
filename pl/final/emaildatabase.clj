(require '[clojure.string :as strfn] )

(defn count-different-emails [ mail_list ]

    (defn no-dots [ s ] (filter #(not= % \.)  s) )

    (defn clean [ s ] (take-while #(not= % \+) s) )

    (defn mail-transform [ mail ]
        (let [ [name domain] (strfn/split mail #"\@" ) ]
            (str (apply str (no-dots (clean name ))) "@" domain )))

    (->> mail_list (map mail-transform ) set count))