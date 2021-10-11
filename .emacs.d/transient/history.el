((magit-blame
  ("-w"))
 (magit-branch nil)
 (magit-cherry-pick
  ("--ff")
  nil)
 (magit-commit nil
               ("--allow-empty"))
 (magit-diff
  (("--" "src/reveal/core.cljs"))
  ("--no-ext-diff" "--stat")
  (("--" "src/bm/utils.cljc")))
 (magit-dispatch nil)
 (magit-ediff nil)
 (magit-fetch nil)
 (magit-gitignore nil)
 (magit-log
  ("-n256" "--graph" "--decorate")
  ("-n256"
   ("--" "tailwind.config.js")
   "--graph" "--decorate")
  (("--" "src/bm/monolith/persistence/universal/import/ezbase.clj"))
  ("-n256" "--author=Yann Vanhalewyn <yann.vanhalewyn@gmail.com>" "--graph" "--decorate")
  (("--" "resources/overdracht_datamapper.org"))
  (("--" "project.clj"))
  ("-n256"
   ("--" "dir-locals")
   "--graph" "--decorate")
  ("-n256"
   ("--" ".dir-locals")
   "--graph" "--decorate")
  (("--" "src/bm/client/date.cljs"))
  (("--" "src/reveal/slides.cljs")))
 (magit-log:--grep "TC")
 (magit-log:-G "gross price" "cis/update")
 (magit-merge nil)
 (magit-notes nil)
 (magit-pull nil
             ("--rebase"))
 (magit-push nil
             ("--force-with-lease"))
 (magit-rebase nil)
 (magit-reset nil)
 (magit-revert
  ("--edit"))
 (magit-stash nil)
 (magit:-- "tailwind.config.js" "" "src/bm/monolith/persistence/universal/import/ezbase.clj" "resources/overdracht_datamapper.org" "src/bm/server/persistence/product/import.clj" "dir-locals" ".dir-locals" "src/bs/core.cljs" "brezan-depot-routes.edn" "brezan/routes.edn")
 (magit:--author
  #("yannyann" 0 8
    (ivy-index 0))
  #("yann" 0 4
    (ivy-index 0))))
