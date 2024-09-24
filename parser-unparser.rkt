#lang eopl

;; Autores: Juan Miguel Palacios Doncel, 2359321. Yeifer Ronaldo Muñoz Valencia, 2278665. Juan Carlos Rojas Quintero, 2359358
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
      [(eqv? (car chip) 'chip-or) (chip_or)]
      [(eqv? (car chip) 'chip-and) (chip_and)]
      [(eqv? (car chip) 'chip-not) (chip_not)]
      [(eqv? (car chip) 'chip-xor) (chip_xor)]
      [(eqv? (car chip) 'chip-nand) (chip_nand)]
      [(eqv? (car chip) 'chip-nor) (chip_nor)]
      [(eqv? (car chip) 'chip-xnor) (chip_xnor)]
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

; (define dataT
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
; )

;; Pruebas para Parser

;;----------------------------------------------------------------------------------------

(define simple-circuit-1 (simple-circuit-list '(a) '(b) (prim-chip-list(chip-and-list))))
(define simple-circuit-2 (simple-circuit-list '(a b c) '(c d) (prim-chip-list(chip-or-list))))
(define simple-circuit-3 (simple-circuit-list '(a) '(b) (prim-chip-list(chip-not-list))))
(define simple-circuit-4 (simple-circuit-list '(a) '(b)  (prim-chip-list(chip-or-list))))
(define simple-circuit-5 (simple-circuit-list '(a b) '(c d) (prim-chip-list(chip-nor-list))))

;;----------------------------------------------------------------------------------------

(define complex-circuit-1 (complex-circuit-list simple-circuit-1 (list simple-circuit-2) '(a) '(b)))
(define complex-circuit-2 (complex-circuit-list simple-circuit-2 (list simple-circuit-1 simple-circuit-2) '(x) '(y)))
(define complex-circuit-3 (complex-circuit-list complex-circuit-1 (list simple-circuit-1 complex-circuit-2) '(w) '(y z)))
(define complex-circuit-4 (complex-circuit-list simple-circuit-4 (list simple-circuit-1)'(a d) '(b)))
(define complex-circuit-5 (complex-circuit-list complex-circuit-4 (list simple-circuit-3 simple-circuit-2) '(x y) '(a b)))

;;--------------------------------------------------------------------------------------------

(define comp-chip-1 (comp-chip-list '(a b c) '(d) simple-circuit-1))
(define comp-chip-2 (comp-chip-list '(a b) '(c d) complex-circuit-2))
(define comp-chip-3 (comp-chip-list '(a) '(b) simple-circuit-4))
(define comp-chip-4 (comp-chip-list '( a b c d) '(e f) complex-circuit-5))
(define comp-chip-5 (comp-chip-list '(a b c) '(d e f) simple-circuit-3))

;;------------------------------------------------------------------------------------------------

(define prim-chip-1(prim-chip-list(chip-and-list)))
(define chip-prim-1 (chip-nand-list))
(define chip-prim-2(chip-or-list))
(define chip-prim-3(chip-xor-list))

;;----------------------------------------------------------------------------------------
;;Pruebas para  Unparser

(define simple-circuit-Unp-1 (simple-circuit '(a) '(b) (prim-chip(chip_and))))
(define simple-circuit-Unp-2 (simple-circuit '(a b c) '(c d) (prim-chip(chip_or))))
(define simple-circuit-Unp-3 (simple-circuit '(a) '(b) (prim-chip(chip_not))))
(define simple-circuit-Unp-4 (simple-circuit '(a) '(b)  (prim-chip(chip_or))))
(define simple-circuit-Unp-5 (simple-circuit '(a b) '(c d) (prim-chip(chip_nor))))

;;----------------------------------------------------------------------------------------

(define complex-circuit-Unp-1 (complex-circuit simple-circuit-Unp-1 (list simple-circuit-Unp-2) '(a) '(b)))
(define complex-circuit-Unp-2 (complex-circuit simple-circuit-Unp-2 (list simple-circuit-Unp-1 simple-circuit-Unp-2) '(x) '(y)))
(define complex-circuit-Unp-3 (complex-circuit complex-circuit-Unp-1 (list simple-circuit-Unp-1 complex-circuit-Unp-2) '(w) '(y z)))
(define complex-circuit-Unp-4 (complex-circuit simple-circuit-Unp-4 (list simple-circuit-Unp-1)'(a d) '(b)))
(define complex-circuit-Unp-5 (complex-circuit complex-circuit-Unp-4 (list simple-circuit-Unp-3 simple-circuit-Unp-2) '(x y) '(a b)))

;;--------------------------------------------------------------------------------------------

(define comp-chip-Unp-1 (comp-chip '(a b c) '(d) simple-circuit-Unp-1))
(define comp-chip-Unp-2 (comp-chip '(a b) '(c d) complex-circuit-Unp-2))
(define comp-chip-Unp-3 (comp-chip '(a) '(b) simple-circuit-Unp-4))
(define comp-chip-Unp-4 (comp-chip '( a b c d) '(e f) complex-circuit-Unp-5))
(define comp-chip-Unp-5 (comp-chip '(a b c) '(d e f) simple-circuit-Unp-3))

;;------------------------------------------------------------------------------------------------

(define prim-chip-Unp-1(prim-chip(chip_and)))
(define chip-prim-Unp-1 (chip_nand))
(define chip-prim-Unp-2(chip_or))
(define chip-prim-Unp-3(chip_xor))