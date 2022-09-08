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
    " Manages namespace install for Chainweb. \
    \ Supports principal namespaces for k: and w: \
    \ using # instead of :. \
    \ Non-principal namespaces require active row in registry \
    \ for NS-NAME with guard matching NS-ADMIN."

    (let*
      ((parse-name
         (lambda (n)
           (let
             ((take-two (take 2 n))
              (drop-two (drop 2 n)))

             (cond
              ((= "k#" take-two)
                (+ "k:" drop-two))
              ((= "w#" take-two)
                (+ "w:" (+ (take 43 drop-two) (+ ":" (drop 46 n)))))
              n))))
       (parsed-name (parse-name ns-name)))

      (if (is-principal parsed-name)

        (validate-principal ns-admin parsed-name) ;; valid principal passes

        (with-default-read registry ns-name       ;; otherwise enforce registry
          { 'admin-guard : ns-admin
          , 'active : false }
          { 'admin-guard := ag
          , 'active := is-active }

          (validate-name ns-name)
          (enforce is-active "Inactive or unregistered namespace")
          (enforce (= ns-admin ag) "Admin guard must match guard in registry")

          true))
      ))

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
