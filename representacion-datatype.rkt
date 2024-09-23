#lang eopl

;; Autores: Juan Miguel Palacios Doncel, 2359321. Yeifer Ronaldo Muñoz Valencia, 2278665. Juan Carlos Rojas Quintero, 2359358

;; <circuito> := circ_simple({cable}∗)
;;               ({cable}∗)
;;               <chip>
;;               simple−circuit ( in out chip )
;;            := circ_comp <circuito> {<circuito>}+
;;                         input {cable}∗
;;                         output {cable}∗
;;               complex-circuit ( circ lcircs in out )

(define-datatype circuito circuito?
  (simple-circuit (in (list-of symbol?)) (out (list-of symbol?)) (chip chip?))
  (complex-circuit (circ circuito?) (lcircs (list-of circuito?)) (in (list-of symbol?)) (out (list-of symbol?)))
)

;; <chip> := <chip_prim>
;;           prim-chip(chip-prim)
;;        := chip (-> {(port)}*)
;;                (<- {(port)}*)
;;                <circuito>
;;           comp-chip(in,out,circ)

(define-datatype chip chip?
  (prim-chip (chip-prim chip-prim?))
  (comp-chip (in (list-of symbol?)) (out (list-of symbol?)) (circ circuito?))
)

;;<chip prim> := prim_or
;;               chip-or ()
;;               := prim_and
;;               chip-and ()
;;               := prim_not
;;               chip-not ()
;;               := prim_xor
;;               chip-xor ()
;;               := prim_nand
;;               chip-xor ()
;;               := prim_nor
;;               chip-nor ()
;;               := prim_xnor
;;               chip-xnor ()

(define-datatype chip-prim chip-prim?
  (chip_or)
  (chip_and)
  (chip_not)
  (chip_xor)
  (chip_nand)
  (chip_nor)
  (chip_xnor)
)

;; Pruebas

;;----------------------------------------------------------------------------------------

(define simple-circuit-1 (simple-circuit '(a) '(b) (prim-chip(chip_and))))
(define simple-circuit-2 (simple-circuit '(a b c) '(c d) (prim-chip(chip_or))))
(define simple-circuit-3 (simple-circuit '(a) '(b) (prim-chip(chip_not))))
(define simple-circuit-4 (simple-circuit '(a) '(b)  (prim-chip(chip_or))))
(define simple-circuit-5 (simple-circuit '(a b) '(c d) (prim-chip(chip_nor))))

;;----------------------------------------------------------------------------------------

(define complex-circuit-1 (complex-circuit simple-circuit-1 (list simple-circuit-2) '(a) '(b)))
(define complex-circuit-2 (complex-circuit simple-circuit-2 (list simple-circuit-1 simple-circuit-2) '(x) '(y)))
(define complex-circuit-3 (complex-circuit complex-circuit-1 (list simple-circuit-1 complex-circuit-2) '(w) '(y z)))
(define complex-circuit-4 (complex-circuit simple-circuit-4 (list simple-circuit-1)'(a d) '(b)))
(define complex-circuit-5 (complex-circuit complex-circuit-4 (list simple-circuit-3 simple-circuit-2) '(x y) '(a b)))

;;--------------------------------------------------------------------------------------------

(define comp-chip-1 (comp-chip '(a b c) '(d) simple-circuit-1))
(define comp-chip-2 (comp-chip '(a b) '(c d) complex-circuit-2))
(define comp-chip-3 (comp-chip '(a) '(b) simple-circuit-4))
(define comp-chip-4 (comp-chip '( a b c d) '(e f) complex-circuit-5))
(define comp-chip-5 (comp-chip '(a b c) '(d e f) simple-circuit-3))

;;------------------------------------------------------------------------------------------------

(define prim-chip-1(prim-chip(chip_and)))
(define chip-prim-1 (chip_nand))
(define chip-prim-2(chip_or))
(define chip-prim-3(chip_xor))