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

(defn remove-trailing-newline [s]
  (apply str (drop-last s)))

(defn time-with-naive-warming [code num]
  (last
   (for [_ (range num)]
     (remove-trailing-newline
      (with-out-str (time (eval code)))))))

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

(defmacro ---
  "A convenience macro to clean up the body of -main."
  [msg code-or-value? body & print-pred]
  `(when (or ~(nil? print-pred) ~@print-pred)
     (if (= ~code-or-value? :code)
       (do (println ";;" (str (string/capitalize ~msg) ":"))
           (pprint-code ~body))
       (println ";;" (str (string/capitalize ~msg) ":") ~body))))

(defn make-tmp-ns-with-user-form [form]
  (interpolating
   (let [f (File/createTempFile "tmp" ".clj")
         fpath (.getPath f)
         fname (second (re-find #"(.*)\.clj" (.getName f)))]
     (spit f "(ns #{fname}.user) (defn user-fn [] #{form})")
     (println ";; Created namespace with wrapper function:" (slurp f))
     (load-file fpath)
     (require [(symbol "#{fname}.user") :as (symbol fname)])
     (ns-resolve (symbol "#{fname}.user") 'user-fn))))

(defn cli-options [args code]
  (cli args
       ["-h" "--help"        :flag true :default false]
       ["-d" "--def-or-defn" :flag true :default (contains-def-or-defn? code)]
       ["-b" "--benchmark"   :flag true :default true]
       ["-k" "--kibit"       :flag true :default true]
       ["-w" "--warming"     :default 25 :parse-fn #(Integer. %)]))

(defn -main [& args]
  (let [code (read)
        user-fn (make-tmp-ns-with-user-form code)
        [opts extra banner] (cli-options args code)]

    (if (or (-> args empty?) (:help opts))
      (println banner)

      (do
        (println (string/join (repeat 30 ";")))
        (println ";; Results:")
        (println (string/join (repeat 30 ";")))

        (--- "input" :code code)
        (--- "value" :code (user-fn))
        (let [output (with-out-str (user-fn))]
          (--- "output" :code output (not-empty output)))

        (--- "depth" :value (how-deep code))
        (--- "length" :value (code-length code))
        (--- "number of core functions used" :value (number-of-core-symbols code))
        (--- "number of symbols used" :value (number-of-symbols code))
        (--- "time (without warming)" :value (remove-trailing-newline
                                              (with-out-str (time (user-fn)))))
        ;; (let [warming-num (:warming opts)]
        ;;   (--- (str "time (with warming [" warming-num "])")
        ;;        :value
        ;;        (time-with-naive-warming
        ;;         (user-fn) warming-num)))

        (--- "kibit results"
             :code
             (doseq [check-map (kibit code)]
               (cli-reporter check-map))
             (:kibit opts))

        (--- "benchmark"
             :code
             (-> (user-fn)
                 (quick-benchmark :verbose true)
                 with-progress-reporting
                 report-result)
             (:benchmark opts))))))
