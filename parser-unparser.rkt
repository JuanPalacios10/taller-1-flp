#lang eopl

;; Autores: Juan Miguel Palacios Doncel, 2359321. Yeifer Ronaldo Muñoz Valencia, 227868665. Juan Carlos Rojas Quintero, 2359358

(define-datatype circuito circuito?
  (simple-circuit (in (list-of symbol?)) (out (list-of symbol?)) (chip chip?))
  (complex-circuit (circ circuito?) (lcircs (list-of circuito?)) (in (list-of symbol?)) (out (list-of symbol?)))
)

(define-datatype chip chip?
  (prim-chip (chip-prim chip-prim?))
  (comp-chip (in (list-of symbol?)) (out (list-of symbol?)) (circ circuito?))
)

(define-datatype chip-prim chip-prim?
  (chip_or)
  (chip_and)
  (chip_not)
  (chip_xor)
  (chip_nand)
  (chip_nor)
  (chip_xnor)
)