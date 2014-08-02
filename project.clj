(defproject othello "0.1.0-SNAPSHOT"
  :description "Clojure version of Peter Norvig's Othello from PAIP"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [seesaw "1.4.4"]]
  :main othello.command-line
  :plugins [[lein-bikeshed "0.1.7"]
            [lein-kibit "0.0.8"]])
