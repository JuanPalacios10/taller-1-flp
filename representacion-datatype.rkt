#lang eopl

;; Autores: Juan Miguel Palacios Doncel, 2359321. Yeifer Ronaldo Muñoz Valencia, 227868665. Juan Carlos Rojas Quintero, 2359358

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

(simple-circuit '(a b) '(c) (prim-chip (chip_and)))
(complex-circuit (simple-circuit '(a) '(b) (prim-chip (chip_or))) (list (simple-circuit '(c) '(d) (prim-chip (chip_and)))) '(a c) '(d))
(comp-chip '(INA INB INC IND) '(OUTA) (simple-circuit '(a b) '(c) (prim-chip (chip_or))))