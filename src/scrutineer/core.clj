(ns scrutineer.core
  (:require [clojure.string :as string]
            [clojure.tools.cli :refer [cli]]
            [criterium.core :refer [quick-benchmark
                                    report-result
                                    with-progress-reporting]]
            [kibit.check :as kibit]
            [kibit.reporters :refer [pprint-code cli-reporter]]
            [alandipert.interpol8 :refer [interpolating]])
  (:import java.io.File)
  (:gen-class))

(def clojure-core-symbols
  (set (map first (ns-publics 'clojure.core))))

(defn clojure-core-symbol?
  "(clojure-core-symbol? '+)
   ;; => +

   (clojure-core-symbol? 'bananas)
   ;; => nil"
  [sym]
  (get clojure-core-symbols sym nil))

(defn number-of-symbols
  "(number-of-symbols '(+ x (+ y z)))
   ;; => 5"
  [code]
  (->> (flatten code)
       (filter symbol?)
       count))

(defn number-of-core-symbols
  "(number-of-core-symbols '(+ x (+ y z)))
   ;; => 2"
  [code]
  (->> (flatten code)
       (filter clojure-core-symbol?)
       count))

(defn how-deep
  "(how-deep '(+ x (+ y z) (- (a (b (c d))))))
   ;; => 6"
  [code]
  (->> (tree-seq coll? seq code)
       (filter list?)
       count))

(defn whitespace? [c] (Character/isWhitespace c))
(defn comma? [c] (= c \,))

(defn code-length
  "(code-length '(+ 1 2 3 4))
  ;; => 7"
  [code]
  (->> (pr-str code)
       (remove #(or (whitespace? %)
                    (comma? %)))
       count))

(defn kibit [code]
  (remove empty?
          (for [resolution [:toplevel :subform]]
            (kibit/check-expr code
                              :rules kibit/all-rules
                              :guard kibit/unique-alt?
                              :resolution resolution
                              :init-ns 'user))))

(defn contains-symbols? [code & syms]
  (let [seq (flatten code)]
    (not-every? nil?
                (for [sym syms]
                  (some #{sym} seq)))))

(defn contains-def-or-defn? [code]
  (contains-symbols? code 'def 'defn))

(defn log-line [msg & body]
  (if (seq body)
    (println ";;" (str (string/capitalize msg) ":") body)
    (println ";;" (str (string/capitalize msg) ":"))))

(defn --- [msg code-or-value? body & print-pred]
  (when (or (nil? print-pred) print-pred)
    (if (= code-or-value? :code)
      (do (log-line msg)
          (pprint-code body))
      (log-line msg body))))

(defn make-tmp-ns-with-user-form [form]
  (interpolating
   (let [f (File/createTempFile "tmp" ".clj")
         fpath (.getPath f)
         fname (second (re-find #"(.*)\.clj" (.getName f)))]
     (spit f "(ns #{fname}.user) (defn user-fn [] #{form})")
     (load-file fpath)
     (require [(symbol "#{fname}.user") :as (symbol fname)])
     (ns-resolve (symbol "#{fname}.user") 'user-fn))))

(defn embedded-repl []
  (print (str (ns-name *ns*) "~> "))
  (flush)
  (let [expr (read)
        user-fn (make-tmp-ns-with-user-form expr)
        value (eval expr)]
    (when (not= :quit value)
      (do
        (--- "input" :code expr)
        (--- "value" :code value)

        (let [output (with-out-str (user-fn))]
          (--- "output" :code output (not-empty output)))

        (--- "depth" :value (how-deep expr))
        (--- "length" :value (code-length expr))
        (--- "number of core functions used" :value (number-of-core-symbols expr))
        (--- "number of symbols used" :value (number-of-symbols expr))
        (--- "time (without warming)" :value (string/trim-newline
                                              (with-out-str (time (user-fn)))))
        (--- "kibit results"
             :code
             (doseq [check-map (kibit expr)]
               (cli-reporter check-map))
             true)

        (--- "benchmark"
             :code
             (-> (user-fn)
                 (quick-benchmark :verbose true)
                 with-progress-reporting
                 report-result)
             true))
      (recur))))

(defn -main [& args]
  (embedded-repl))
