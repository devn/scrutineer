(defproject scrutineer "0.1.0-SNAPSHOT"
  :description "Get out your magnifying glass and scrutinize your Clojure code."
  :url "http://github.com/devn/scrutineer"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.cli "0.2.2"]
                 [jonase/kibit "0.0.9-SNAPSHOT"]
                 [criterium "0.3.1"]
                 [alandipert/interpol8 "0.0.3"]
                 [me.raynes/conch "0.5.0"]
                 [analyze "0.3.0"]]
  :main scrutineer.core)
