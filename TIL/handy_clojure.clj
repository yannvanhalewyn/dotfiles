;; ===============================
;; Some handy functions in clojure
;; ===============================

(reductions + (range 10))
;; => (0 1 3 6 10 15 21 28 36 45)

(require 'clojure.inspector)
(clojure.inspector/inspect "foo")
(clojure.inspector/inspect-tree {:foo "bar" :deep {:merge "true"}})
