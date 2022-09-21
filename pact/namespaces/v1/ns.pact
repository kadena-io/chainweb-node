
(define-keyset 'ns-admin-keyset (read-keyset 'ns-admin-keyset))
(define-keyset 'ns-operate-keyset (read-keyset 'ns-genesis-keyset))

(module ns GOVERNANCE
  "Administers definition of new namespaces in Chainweb."

  (defschema reg-entry
    admin-guard:guard
    active:bool)

  (deftable registry:{reg-entry})

  (defcap GOVERNANCE ()
    (enforce-keyset 'ns-admin-keyset))

  (defcap OPERATE ()
    (enforce-keyset 'ns-operate-keyset))

  (defconst GUARD_SUCCESS (create-user-guard (success)))
  (defconst GUARD_FAILURE (create-user-guard (failure)))

  (defun success ()
    true)
  (defun failure ()
    (enforce false "Disabled"))

  (defun validate-name (name)
    (enforce (!= "" name) "Empty name not allowed")
    (enforce (< (length name) 64) "Name must be less than 64 characters long")
    (enforce (is-charset CHARSET_LATIN1 name)
             "Name must be in latin1 charset"))

  (defun validate:bool
      ( ns-name:string
        ns-admin:guard
        )
    " Manages namespace install for Chainweb. Requires active row in registry \
    \ for NS-NAME with guard matching NS-ADMIN."

    (validate-name ns-name)

    (with-default-read registry ns-name
      { 'admin-guard : ns-admin
      , 'active : false }
      { 'admin-guard := ag
      , 'active := is-active }

        (enforce is-active "Inactive or unregistered namespace")
        (enforce (= ns-admin ag) "Admin guard must match guard in registry")

        true))

  (defun write-registry:string
      ( ns-name:string
        guard:guard
        active:bool
        )
    " Write entry with GUARD and ACTIVE into registry for NAME. \
    \ Guarded by operate keyset. "

    (with-capability (OPERATE)

      (validate-name ns-name)

      (write registry ns-name
        { 'admin-guard: guard
        , 'active: active })

      "Register entry written"))

  (defun query:object{reg-entry}
      ( ns-name:string )
    (read registry ns-name))

  )

(create-table registry)

(write-registry "kadena"
  (keyset-ref-guard 'ns-operate-keyset) true)
(write-registry "user" GUARD_FAILURE true)
(write-registry "free" GUARD_FAILURE true)

(define-namespace "kadena"
  (keyset-ref-guard 'ns-operate-keyset)
  (keyset-ref-guard 'ns-operate-keyset))

(define-namespace "user" GUARD_SUCCESS GUARD_FAILURE)
(define-namespace "free" GUARD_SUCCESS GUARD_FAILURE)
;;rotate to real operate keyset
(define-keyset 'ns-operate-keyset (read-keyset 'ns-operate-keyset))
