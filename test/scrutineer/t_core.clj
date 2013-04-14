(ns scrutineer.t-core
  (:use midje.sweet)
  (:require [scrutineer.core :as core]))

(facts "#'scrutineer.core/clojure-core-symbol?"
  (core/clojure-core-symbol? '+) => truthy
  (core/clojure-core-symbol? 'bananas) => falsey)

(fact "#'scrutineer.core/number-of-symbols"
  (core/number-of-symbols '(+ x (+ y z))) => 5)

(fact "#'scrutineer.core/number-of-core-symbols"
  (core/number-of-core-symbols '(+ x (+ y z))) => 2)

(fact "#'scrutineer.core/how-deep"
  (core/how-deep '(+ x (+ y (+ z)))) => 3
  (core/how-deep '(+ x (apply + [y (+ z (+ z x))]))) => 4)

(fact "#'scrutineer.core/code-length"
  (core/code-length '(+ x (+ y (+ z)))) => 12 
  (core/code-length '(+ x (apply + [y (+ z (+ z x))]))) => 24)

(fact "#'scrutineer.core/remove-trailing-newline"
  (core/remove-trailing-newline "abc\n") => "abc"
  (core/remove-trailing-newline "abc\n\n") => "abc\n")

(fact "#'scrutineer.core/time-with-naive-warming"
  (core/time-with-naive-warming '(+ 1 1) 5) => (contains #"Elapsed time:"))

(fact "#'scrutineer.core/kibit"
  (core/kibit '(+ 1 1)) => (two-of map?)
  (core/kibit '(+ 1 (- 1 1))) => (two-of map?)
  (core/kibit '(if (=  false true) :a nil)) => (two-of map?))

(fact "#'scrutineer.core/contains-symbols?"
  (core/contains-symbols? '(a b c 1 2 3 + - * ->) '-> '* 'b) => true
  (core/contains-symbols? '(a b c 1 2 3 + - * ->) 'bananas) => false)

(fact "#'scrutineer.core/contains-def-or-defn?"
  (core/contains-def-or-defn? '(def x 1)) => true
  (core/contains-def-or-defn? '(+ x (- x 1))) => false)
