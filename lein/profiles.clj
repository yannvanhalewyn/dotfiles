{:user {:dependencies [[nrepl "0.6.0"]
                       [vvvvalvalval/scope-capture "0.3.2"]
                       [vvvvalvalval/scope-capture-nrepl "0.3.1"]]
        :plugins [[refactor-nrepl "2.5.0"]
                  [cider/cider-nrepl "0.25.0" ]
                  [lein-ns-dep-graph "0.2.0-SNAPSHOT"]]
        :middleware [refactor-nrepl.plugin/middleware cider-nrepl.plugin/middleware]
        :injections [(require 'sc.api)]}}
