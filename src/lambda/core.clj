(ns lambda.core
  (:gen-class))

(defprotocol Expression
  (to-str [expr])
  (substitute [expr n value])
  (beta-reduce [expr])
  (free? [exp n]))

(defrecord lname [lname]
  Expression
  (free? [exp n] (= (:lname n) lname))
  (to-str
    [expr]
    (:lname expr))
  (substitute
    [expr n value]
    (if (free? expr n)
      value
      expr))
  (beta-reduce
    [expr]
    expr
    ))

(defrecord function [name body]
  Expression
  (free? [expr n]
    (and (not= name n) (free? body n)))
  (to-str
    [expr]
    (str "λ" (to-str (:name expr)) "." (to-str (:body expr))))
  (substitute
    [expr n value]
    (if (free? expr n)
      (->function (:name expr) (substitute (:body expr) n value))
      expr))
  (beta-reduce
    [expr]
    (->function (:name expr) (beta-reduce (:body expr)))))

(defrecord application [function-expression argument-expression]
  Expression
  (free? [expr n]
    (or (free? function-expression n) (free? argument-expression n)))
  (to-str
    [expr]
    (str "("
         (to-str (:function-expression expr))
         " "
         (to-str (:argument-expression expr))
         ")"))
  (substitute
      [expr n value]
    (if (free? expr n)
      (->application (substitute (:function-expression expr) n value)
                   (substitute (:argument-expression expr) n value))
      expr))
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

(defn steps [expr]
  (let [reduced (beta-reduce expr)]
    (if (= reduced expr)
      [expr]
      (concat [expr] (steps reduced)))))

(defn check-path [expr]
  (->> expr
       (steps)
       (map to-str)
       (clojure.string/join " -> ")))

(defn check [expr]
  (prn (str (to-str expr)
            " -> "
            (to-str (beta-reduce expr)))))


(defn func* [args body]
  (if (empty? args)
    body
    (let [[first-arg & rest] args]
      (->function (->lname first-arg) (func* rest body)))))

(defn appl* [fn & args]
  (if (empty? args)
    (println "no thnuk allowed")
    (let [[first-arg & rest] args
          func (->application fn first-arg)]
      (if (seq rest)
        (recur func rest)
        func))))

(defn nm [x] (->lname x))





(def identity-fn (->function (->lname "x") (->lname "x")))
(def self-apply-fn (->function (->lname "s") (->application (->lname "s") (->lname "s"))))
(def tru (->function (->lname "t") (->function (->lname "f") (->lname "t"))))
(def fls (->function (->lname "t") (->function (->lname "f") (->lname "f"))))
(def iff (func* ["c" "then" "else"] (->application (->application (nm "c") (nm "then")) (nm "else"))))
(def and* (func* ["a" "b"] (appl* iff (nm "a") (nm "b") fls)))

    
