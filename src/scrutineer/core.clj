(ns scrutineer.core
  (:require [clojure.tools.cli :refer [cli]]
            [criterium.core :refer [quick-benchmark
                                    report-result
                                    with-progress-reporting]]))

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

(defn time-with-naive-warming [code]
  (last
   (for [x (range 25)]
     (remove-trailing-newline
      (with-out-str (time (eval code)))))))

;; (def cli-options
;;   [["-h" "--help" "Help" :flag true :default false]
;;    ["-d" "--def-or-defn" :flag true :default false]
;;    ["-b" "--benchmark"   :flag true :default false]])

;; [opts extra banner] (apply #(cli args %) cli-options)

(defn -main [& args]
  (let [code (read)]
    (println ";; Results")
    (println "Input:" code)
    (println "Value:" (eval code))
    (println "Output:" (with-out-str (eval code)))
    (println "Depth:" (how-deep code))
    (println "Length:" (code-length code))
    (println "Number of Core Functions Used:" (number-of-core-symbols code))
    (println "Total number of symbols used:" (number-of-symbols code))
    (println "Time (without warming):" (remove-trailing-newline
                                        (with-out-str (time (eval code)))))
    (println "Time (with warming):" (time-with-naive-warming (eval code)))
    (println)
    (println "Serious Benchmark:") (-> (eval code)
                                       (quick-benchmark :verbose true)
                                       with-progress-reporting
                                       report-result)))
