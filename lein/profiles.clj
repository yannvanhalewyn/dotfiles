{:user {:dependencies [[org.clojure/tools.nrepl "0.2.13"]
                       [vvvvalvalval/scope-capture "0.1.4"]
                       [acyclic/squiggly-clojure "0.1.9-SNAPSHOT"
                        :exclusions [org.clojure/tools.reader]]]
        :plugins [[refactor-nrepl "2.4.0-SNAPSHOT"]
                  [cider/cider-nrepl "0.18.0-SNAPSHOT"]]
        :injections [(require 'sc.api)]
        :env {:squiggly {:checkers [:eastwood :kibit]}}}}
