{:user {:dependencies [[nrepl "0.6.0"]
                       [vvvvalvalval/scope-capture "0.3.2"]
                       [acyclic/squiggly-clojure "0.1.9-SNAPSHOT"
                        :exclusions [org.clojure/tools.reader]]]
        :plugins [[refactor-nrepl "2.4.0"]
                  [cider/cider-nrepl "0.21.1" ]]
        :middleware [refactor-nrepl.plugin/middleware cider-nrepl.plugin/middleware]
        :injections [(require 'sc.api)]
        :env {:squiggly {:checkers [:eastwood :kibit]}}}}
