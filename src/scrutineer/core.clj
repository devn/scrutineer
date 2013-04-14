(ns scrutineer.core
  (:require [clojure.string :as string]
            [clojure.tools.cli :refer [cli]]
            [criterium.core :refer [quick-benchmark
                                    report-result
                                    with-progress-reporting]]
            [kibit.check :as kibit]
            [kibit.reporters :refer [pprint-code cli-reporter]])
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
  "A simple convenience macro to clean up the body of -main."
  [msg body & print?]
  `(when ~@(seq print?)
     (do (println ";;" ~(str (string/capitalize msg) ":"))
         ~body)))

(defn cli-options [args code]
  (cli args
       ["-h" "--help"          :flag true :default false]
       ["-d" "--def-or-defn"   :flag true :default (contains-def-or-defn? code)]
       ["-b" "--benchmark"     :flag true :default false]
       ["-k" "--kibit"         :flag true :default true]
       ["-w" "--warming"       :flag true :default 25]))

(defn -main [& args]
  (let [code (read)
        [opts extra banner] (cli-options args code)]

    (--- "input" (pprint-code code))
    (--- "value" (pprint-code (eval code)))
    (let [output (with-out-str (eval code))]
      (--- "output" (pprint-code output)
           (not-empty output)))

    (--- "depth" (how-deep code))
    (--- "length" (code-length code))
    (--- "number of core functions used" (number-of-core-symbols code))
    (--- "number of symbols used" (number-of-symbols code))
    (--- "time (without warming)" (remove-trailing-newline
                                   (with-out-str (time (eval code)))))
    (let [warming-num (:warming opts)]
      (--- (str "time (with warming [" warming-num "]")
           (time-with-naive-warming (eval code) warming-num)))

    (--- "kibit results"
         (doseq [check-map (kibit code)]
           (cli-reporter check-map))
         (:kibit opts))

    (--- "benchmark"
         (-> (eval code)
             (quick-benchmark :verbose true)
             with-progress-reporting
             report-result)
         (:benchmark opts))))
