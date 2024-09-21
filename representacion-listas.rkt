#lang eopl

;; Autores: Juan Miguel Palacios Doncel, 2359321. Yeifer Ronaldo Muñoz Valencia, 227868665. Juan Carlos Rojas Quintero, 2359358

;; <circuito> := circ_simple({cable}∗)
;;               ({cable}∗)
;;               <chip>
;;               simple−circuit ( in out chip )

;; simple-circuit: in out chip -> circuito
;; Proposito: Construye un circuito simple con una entrada, una salida y un chip

(define simple-circuit
  (lambda (in out chip)
    (list 'simple-circuit in out chip)
  )
)

;; Pruebas
(simple-circuit '(a) '(b) 'chip1)

;; <circuito> := circ_comp <circuito> {<circuito>}+
;;                         input {cable}∗
;;                         output {cable}∗
;;               complex-circuit ( circ lcircs in out )

;; complex-circuit: circ lcircs in out -> circuito
;; Proposito: Construye un circuito complejo con un circuito, una lista de circuitos, una entrada y una salida

(define complex-circuit
  (lambda (circ lcircs in out)
    (list 'complex-circuit circ lcircs in out)
  )
)

;; Pruebas
(complex-circuit '(simple-circuit '(a) '(b) 'chip1) '((simple-circuit '(c) '(d) 'chip2)) '(a c) '(d))

;; <chip> := chip (-> {(port)}*)
;;                (<- {(port)}*)
;;                <circuito>
;;           comp-chip(in,out,circ)

;; comp-chip: in out circ -> chip

;; Proposito: Construye un chip con unos puertos de entrada, puertos de salida y un circuito

(define comp-chip
  (lambda (in out circ)
    (list 'comp-chip in out circ)
  )
)

;; Pruebas
(comp-chip '(INA INB INC IND) '(OUTA) (simple-circuit '(a b) '(c) 'chip1))

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

;; chip-or: -> chip-prim
;; chip-and: -> chip-prim
;; chip-not: -> chip-prim
;; chip-xor: -> chip-prim
;; chip-nand: -> chip-prim
;; chip-nor: -> chip-prim
;; chip-xnor: -> chip-prim

;; Proposito: Construye un chip primitivo para cada compuerta lógica

(define chip-or
  (lambda ()
    (list 'chip-or)
  )
)

(define chip-and
  (lambda ()
    (list 'chip-and)
  )
)

(define chip-not
  (lambda ()
    (list 'chip-not)
  )
)

(define chip-xor
  (lambda ()
    (list 'chip-xor)
  )
)

(define chip-nand
  (lambda ()
    (list 'chip-nand)
  )
)

(define chip-nor
  (lambda ()
    (list 'chip-nor)
  )
)

(define chip-xnor
  (lambda ()
    (list 'chip-xnor)
  )
)

;; Pruebas
(chip-or)
(chip-and)
(chip-not)
(chip-xor)
(chip-nand)
(chip-nor)
(chip-xnor)

;; <chip> := <chip_prim>
;;           prim-chip(chip-prim)

;; prim-chip: chip-prim -> chip

;; Proposito: Construye un chip con un chip primitivo

(define prim-chip
  (lambda (chip-prim)
    (list 'prim-chip chip-prim)
  )
)

;; Pruebas
(prim-chip (chip-or))
(prim-chip (chip-and))
(prim-chip (chip-not))



;; Observadores



;; Predicados

(define simple-circuit?
  (lambda (lst)
    (equal? (car lst) 'simple-circuit)
    )
)

(define complex-circuit?
  (lambda (lst)
    (equal? (car lst) 'complex-circuit)
    )
)


(define comp-chip?
  (lambda (lst)
    (equal? (car lst) 'comp-chip)
    )
  )

(define chip-or?
  (lambda (lst)
    (equal? (car lst) 'chip-or)
    )
  )


(define chip-and?
  (lambda (lst)
    (equal? (car lst) 'chip-and)
    )
  )


(define chip-xor?
  (lambda (lst)
    (equal? (car lst) 'chip-xor)
    )
  )

(define chip-not?
  (lambda (lst)
    (equal? (car lst) 'chip-not)
    )
  )


(define chip-nand?
  (lambda (lst)
    (equal? (car lst) 'chip-nand)
    )
  )



(define chip-nor?
  (lambda (lst)
    (equal? (car lst) 'chip-nor)
    )
  )

(define chip-xnor?
  (lambda (lst)
    (equal? (car lst) 'chip-xnor)
    )
  )

;; pruebas

;;circuitos
(define ejemplo8 '(simple-circuit '(a) '(b) 'chip1))
(define ejemplo9 '(complex-circuit '(simple-circuit '(a) '(b) 'chip1) '((simple-circuit '(c) '(d) 'chip2)) '(a c) '(d)))

;;chips
(define ejemplo1 '(chip-or))
(define ejemplo2 '(chip-and))
(define ejemplo3 '(chip-xor))
(define ejemplo4 '(chip-not))
(define ejemplo5 '(chip-nand))
(define ejemplo6 '(chip-nor))
(define ejemplo7 '(chip-xnor))

