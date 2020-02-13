(define-keyset 'util-ns-users)
(define-keyset 'util-ns-admin)
(ns.write-registry 'util (keyset-ref-guard 'util-ns-admin) true)
(define-namespace 'util
  (keyset-ref-guard 'util-ns-users)
  (keyset-ref-guard 'util-ns-admin))
