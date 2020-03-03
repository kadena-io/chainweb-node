;; guards.pact

(namespace 'util)

(module guards AUTONOMOUS

  "Functions for implementing various user guards."

  (defcap AUTONOMOUS ()
    (enforce false "Non-upgradeable"))

  (defun after-date:guard (date:time)
    "Guard to enforce chain time is after DATE."
    (create-user-guard (enforce-after-date date)))

  (defun enforce-after-date:bool (date:time)
    (enforce-time date "after"
                  (> (chain-time) date)))


  (defun at-after-date:guard (date:time)
    "Guard to enforce chain time is at or after DATE."
    (create-user-guard (enforce-at-after-date date)))

  (defun enforce-at-after-date:bool (date:time)
    (enforce-time date "at or after"
                  (>= (chain-time) date)))


  (defun before-date:guard (date:time)
    "Guard to enforce chain time is before DATE."
    (create-user-guard (enforce-before-date date)))

  (defun enforce-before-date:bool (date:time)
    (enforce-time date "before"
                  (< (chain-time) date)))


  (defun at-before-date:guard (date:time)
    "Guard to enforce chain time is at or before DATE."
    (create-user-guard (enforce-at-before-date date)))

  (defun enforce-at-before-date:bool (date:time)
    (enforce-time date "at or before"
                  (<= (chain-time) date)))


  (defun enforce-time:bool (date:time msg:string test:bool)
    (enforce test
             (format "Chain time must be {} {}" [msg date])))

  (defun chain-time:time ()
    (at 'block-time (chain-data)))

  (defun guard-and:guard (a:guard b:guard)
    "Guard to enforce both A and B."
    (create-user-guard (enforce-and a b)))

  (defun enforce-and:bool (a:guard b:guard)
    (enforce-guard a)
    (enforce-guard b))

  (defun guard-or:guard (a:guard b:guard)
    "Guard to enforce A or B."
    (create-user-guard (enforce-or a b)))

  (defun enforce-or:bool (a:guard b:guard)
    (enforce-one
     (format "Enforce {} or {}" [a b])
     [(enforce-guard a)
      (enforce-guard b)]))

  )
