{:user {:dependencies [[nrepl "0.8.3"]
                       [vvvvalvalval/scope-capture "0.3.2"]
                       [vvvvalvalval/scope-capture-nrepl "0.3.1"]]
        :plugins [[refactor-nrepl "3.6.0"]
                  [cider/cider-nrepl "0.27.2" ]]
        :middleware [refactor-nrepl.plugin/middleware cider-nrepl.plugin/middleware]
        :injections [(require 'sc.api)]}}
