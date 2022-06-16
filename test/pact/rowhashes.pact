;; used in Checkpointer.hs test

(module rowhashes GOV
  (defcap GOV () true)
  (defschema sch a:string)
  (deftable table:{sch})
  (deftable tabl:{sch}))

(create-table table)
(create-table tabl)
