#lang eopl

;; Autores: Juan Miguel Palacios Doncel, 2359321. Yeifer Ronaldo Muñoz Valencia, 227868665. Juan Carlos Rojas Quintero, 2359358
(require (only-in "representacion-listas.rkt" [simple-circuit simple-circuit-list] [complex-circuit complex-circuit-list] 
[comp-chip comp-chip-list] [chip-or chip-or-list] [chip-and chip-and-list] [chip-not chip-not-list] [chip-xor chip-xor-list] [chip-nand chip-nand-list] 
[chip-nor chip-nor-list] [chip-xnor chip-xnor-list] [prim-chip prim-chip-list]))

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



(define evalCirc
  (lambda (cir)
    (cond
      [(eqv? (car cir) 'simple-circuit )
        (simple-circuit (cadr cir) (caddr cir) (evalChip (cadddr cir)))]
      [(eqv? (car cir) 'complex-circuit )
        (complex-circuit (evalCirc (cadr cir)) (map evalCirc (caddr cir)) (cadddr cir) (cadddr (cdr cir)))]
    )
  )
)

(define evalChip
  (lambda (chip)
    (cond
      [(eqv? (car chip) 'prim-chip )
        (prim-chip (evalPrimChip (cadr chip)))]
      [(eqv? (car chip) 'comp-chip )
        (comp-chip (cadr chip) (caddr chip) (evalCirc (cadddr chip)))]
    )
  )
)

(define evalPrimChip
  (lambda (chip)
    (cond
      [(eqv? (car chip) 'chip_or) (chip_or)]
      [(eqv? (car chip) 'chip_and) (chip_and)]
      [(eqv? (car chip) 'chip_not) (chip_not)]
      [(eqv? (car chip) 'chip_xor) (chip_xor)]
      [(eqv? (car chip) 'chip_nand) (chip_nand)]
      [(eqv? (car chip) 'chip_nor) (chip_nor)]
      [(eqv? (car chip) 'chip_xnor) (chip_xnor)]
    )
  )
)

(define parser
  (lambda (circ)
    (evalCirc circ)
  )
)





