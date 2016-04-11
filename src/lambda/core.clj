(ns lambda.core
  (:gen-class))




(defprotocol Expression
  (to-str [expr])
  (substitute [expr n value])
  (beta-reduce [expr]))

(defrecord lname [lname]
  Expression
  (to-str
    [expr]
    (:lname expr))
  (substitute
    [expr n value]
    (if (= (:lname expr) (:lname n))
      value
      expr))
  (beta-reduce
    [expr]
    expr
    ))

(defrecord function [name body]
  Expression
  (to-str
    [expr]
    (str "λ" (to-str (:name expr)) "." (to-str (:body expr))))
  (substitute
    [expr n value]
    (->function (:name expr) (substitute (:body expr) n value)))
  (beta-reduce
    [expr]
    (->function (:name expr) (beta-reduce (:body expr)))))

(defrecord application [function-expression argument-expression]
  Expression
  (to-str
    [expr]
    (str "("
         (to-str (:function-expression expr))
         " "
         (to-str (:argument-expression expr))
         ")"))
  (substitute
    [expr n value]
    (->application (substitute (:function-expression expr) n value)
                   (substitute (:argument-expression expr) n value)))
  (beta-reduce
    [expr]
    (let [reduced-fn-expr (beta-reduce (:function-expression expr))]
      (if (instance? function reduced-fn-expr)
        (let [reduced-arg-expr (beta-reduce (:argument-expression expr))]
          (substitute (:body reduced-fn-expr) (:name reduced-fn-expr) reduced-arg-expr))
        (->application reduced-fn-expr (:argument-expression expr)))

      )))



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")

  (let [identity-fn (->function (->lname "x") (->lname "x"))
        app (->application identity-fn (->lname "y"))]

    (prn (to-str identity-fn))
    (prn (to-str app))
    (prn (to-str (beta-reduce identity-fn)))
    (prn (substitute (->lname "foo") (->lname "foo") (->lname "bar")))
    (prn (substitute identity-fn (->lname "foo") (->lname "bar")))
    (prn (to-str (substitute app (->lname "y") (->lname "bar"))))
    (prn (to-str (beta-reduce app)))
    )

  )


(def identity-fn (->function (->lname "x") (->lname "x")))
(def app (->application identity-fn (->lname "y")))
