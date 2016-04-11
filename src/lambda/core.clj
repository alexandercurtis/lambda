(ns lambda.core
  (:gen-class))


(defprotocol Expression
  (to-str [exp]))

(defrecord lname [lname]
  Expression
  (to-str
    [n]
    (:lname n)))

(defrecord function [name expression]
  Expression
  (to-str
    [f]
    (str "λ" (to-str (:name f)) "." (to-str (:expression f)))))



(defrecord application [function-expression argument-expression]
  Expression
  (to-str
    [a]
    (str "(" (to-str (:function-expression a)) " " (to-str (:argument-expression a)) ")")))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")

  (let [identity-fn (->function (->lname "x") (->lname "x"))
        app (->application identity-fn (->lname "y"))]

    (prn (to-str identity-fn))
    (prn (to-str app))


    )

  )
