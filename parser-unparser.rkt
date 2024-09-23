#lang eopl

;; Autores: Juan Miguel Palacios Doncel, 2359321. Yeifer Ronaldo Muñoz Valencia, 227868665. Juan Carlos Rojas Quintero, 2359358
(require (only-in "representacion-listas.rkt" [simple-circuit simple-circuit-list] [complex-circuit complex-circuit-list] 
[comp-chip comp-chip-list] [chip-or chip-or-list] [chip-and chip-and-list] [chip-not chip-not-list] [chip-xor chip-xor-list] [chip-nand chip-nand-list] 
[chip-nor chip-nor-list] [chip-xnor chip-xnor-list] [prim-chip prim-chip-list]))

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

(define evaluar-circuito
  (lambda (circ)
    (cases circuito circ
      (simple-circuit (in out ch)
        (simple-circuit-list in out (evaluar-chip ch))
      )
      (complex-circuit (circ lcircs in out)
        (complex-circuit-list (evaluar-circuito circ) (map evaluar-circuito lcircs) in out)
      )
    )
  )
)

(define evaluar-chip
  (lambda (ch)
    (cases chip ch
      (prim-chip (ch-prim)
        (prim-chip-list (evaluar-prim ch-prim))
      )
      (comp-chip (in out circ)
        (comp-chip-list in out (evaluar-circuito circ))
      )
    )
  )
)

(define evaluar-prim
  (lambda (ch-prim)
    (cases chip-prim ch-prim
      (chip_or ()
        (chip-or-list)
      )
      (chip_and ()
        (chip-and-list)
      )
      (chip_not ()
        (chip-not-list)
      )
      (chip_xor ()
        (chip-xor-list)
      )
      (chip_nand ()
        (chip-nand-list)
      )
      (chip_nor ()
        (chip-nor-list)
      )
      (chip_xnor ()
        (chip-xnor-list)
      )
    )
  )
)

(define unparser
  (lambda (comp)
    (evaluar-circuito comp)
  )
)

; (complex-circuit
;  (simple-circuit
;   '(m n o p)
;   '(e f)
;     (comp-chip
;       '(INA INB INC IND)
;       '(OUTE OUTF)
;         (complex-circuit
;           (simple-circuit '(a b) '(e) (prim-chip (chip_and)))
;           (list
;           (simple-circuit '(c d) '(f) (prim-chip (chip_and)))
;           )
;           '(a b c d)
;           '(e f)
;         )
;     )
;   )
;   (list
;     (simple-circuit
;     '(e f)
;     '(z)
;       (comp-chip
;         '(INE INF)
;         '(OUTA)
;         (simple-circuit '(e f) '(g) (prim-chip (chip_or)))
;       )
;     )
;   )
;   '(m n o p)
;   '(z)
; )

