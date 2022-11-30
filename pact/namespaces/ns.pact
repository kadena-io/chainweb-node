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

  (defun create-principal-namespace:string
      ( g:guard
        )
    " Format principal namespace as Pact hash (BLAKE2b256) of principal \
    \ in hex truncated to 160 bits (40 characters), prepended with 'n_'.\
    \ Only w: and k: account protocols are supported. "

    (let
      ((ty (typeof-principal (create-principal g))))

      ;; only w: and k: currently supported
      (if (or (= ty "k:") (= ty "w:"))
        (+ "n_" (take 40 (int-to-str 16 (str-to-int 64 (hash g)))))
        (enforce false
          (format "Unsupported guard protocol: {}" [ty]))
        ))
  )

  (defun validate:bool
      ( ns-name:string
        ns-admin:guard
        )
    " Manages namespace install for Chainweb. \
    \ Allows principal namespaces. \
    \ Non-principal namespaces require active row in registry \
    \ for NS-NAME with guard matching NS-ADMIN."

    (if (= (create-principal-namespace ns-admin) ns-name)

      true ;; allow principal namespaces

      (with-default-read registry ns-name       ;; otherwise enforce registry
        { 'admin-guard : ns-admin
        , 'active : false }
        { 'admin-guard := ag
        , 'active := is-active }

        (enforce is-active "Inactive or unregistered namespace")
        (enforce (= ns-admin ag) "Admin guard must match guard in registry")

        true)
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
