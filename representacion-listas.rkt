#lang eopl

;; Autores: Juan Miguel Palacios Doncel, 2359321. Yeifer Ronaldo Muñoz Valencia, 227868665. Juan Carlos Rojas Quintero, 2359358

(provide simple-circuit complex-circuit comp-chip chip-or chip-and chip-not chip-xor chip-nand chip-nor chip-xnor prim-chip)

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

;; <circuito> := circ_comp <circuito> {<circuito>}+
;;                         input {cable}∗
;;                         output {cable}∗
;;               complex-circuit ( circ lcircs in out )

;; complex-circuit: circ lcircs in out -> circuito
;; Proposito: Construye un circuito complejo con un circuito, una lista de circuitos, una entrada y una salida

(define complex-circuit
  (lambda (circ lcircs in out)
    (if (> (length lcircs) 0)
      (list 'complex-circuit circ lcircs in out)
      (eopl:error "La lista de circuitos no puede ser vacía")
    )
  )
)

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

;; <chip> := <chip_prim>
;;           prim-chip(chip-prim)

;; prim-chip: chip-prim -> chip

;; Proposito: Construye un chip con un chip primitivo

(define prim-chip
  (lambda (chip-prim)
    (list 'prim-chip chip-prim)
  )
)

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

(define prim-chip?
  (lambda (lst)
    (equal? (car lst) 'prim-chip)
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

;;Extractores

(define simple-circuit->in
  (lambda(simple-circuit)
      (cadr(simple-circuit))))

(define simple-circuit->out
  (lambda(simple-circuit)
      (caddr(simple-circuit))))
    
(define simple-circuit->chip
  (lambda(simple-circuit)
      (cadddr(simple-circuit))))

;;----------------------------------------------------------------------

(define complex-circuit->circ
  (lambda(complex-circuit)
      (cadr(complex-circuit))))

(define complex-circuit->lcircs
  (lambda(complex-circuit)
      (caddr(complex-circuit))))

(define complex-circuit->in
  (lambda(complex-circuit)
      (cadddr(complex-circuit))))

(define complex-circuit->out
  (lambda(complex-circuit)
      ((caadr (cdddr (cdr complex-circuit))))))

;;-------------------------------------------------------------------------------------

(define comp-chip->in
  (lambda(comp-chip)
      (cadr(comp-chip))))

(define comp-chip->out
  (lambda(comp-chip)
      (caddr(comp-chip))))

(define comp-chip->circ
  (lambda(comp-chip)
      (cadddr(comp-chip))))

;;----------------------------------------------------------------------------------------

(define chip-or->chip
  (lambda (chip-or)
    (car chip-or))) 

(define chip-and->chip
  (lambda (chip-and)
    (car chip-and))) 

(define chip-nand->chip
  (lambda (chip-nand)
    (car chip-nand))) 

(define chip-nor->chip
  (lambda (chip-nor)
    (car chip-nor))) 

(define chip-not->chip
  (lambda (chip-not)
    (car chip-not))) 

(define chip-xnor->chip
  (lambda (chip-xnor)
    (car chip-xnor))) 

(define chip-xor->chip
  (lambda (chip-xor)
    (car chip-xor))) 

;;--------------------------------------------------------------------------------

(define prim-chip->chip-prim
  (lambda (prim-chip)
    (cadr prim-chip)
  )
)


;; Pruebas
;;----------------------------------------------------------------------------------------

(define simple-circuit-1 (simple-circuit '(a) '(b) (prim-chip(chip-and))))
(define simple-circuit-2 (simple-circuit '(a b c) '(c d) (prim-chip(chip-or))))
(define simple-circuit-3 (simple-circuit '(a) '(b) (prim-chip(chip-not))))
(define simple-circuit-4 (simple-circuit '(a) '(b)  (prim-chip(chip-or))))
(define simple-circuit-5 (simple-circuit '() '() (prim-chip(chip-nor))))

;;----------------------------------------------------------------------------------------

(define complex-circuit-1 (complex-circuit simple-circuit-1 (list simple-circuit-2) '(a) '(b)))
(define complex-circuit-2 (complex-circuit simple-circuit-2 (list simple-circuit-1 simple-circuit-2) '(x) '(y)))
(define complex-circuit-3 (complex-circuit complex-circuit-1 (list simple-circuit-1 complex-circuit-2) '(w) '(y z)))
(define complex-circuit-4 (complex-circuit simple-circuit-4 (list simple-circuit-1)'(a d) '(b)))
(define complex-circuit-5 (complex-circuit complex-circuit-4 (list simple-circuit-3 simple-circuit-2) '(x y) '(a b)))

;;--------------------------------------------------------------------------------------------

(define comp-chip-1 (comp-chip '(a b c) '(d) (list simple-circuit-1)))
(define comp-chip-2 (comp-chip '(a b) '(c d)(list complex-circuit-2)))
(define comp-chip-3 (comp-chip '(a) '(b) (list simple-circuit-4)))
(define comp-chip-4 (comp-chip '( a b c d) '(e f) (list complex-circuit-5 simple-circuit-2)))
(define comp-chip-5 (comp-chip '(a b c) '(d e f) (list simple-circuit-3)))

;;------------------------------------------------------------------------------------------------

(define chip-or-1(chip-or))
(define chip-and-1(chip-and))
(define chip-nand-1(chip-nand))
(define chip-xor-1(chip-xor))
(define chip-nor-1(chip-nor))
(define chip-xnor-1(chip-xnor))
(define chip-not-1(chip-not))

;;-------------------------------------------------------------------------------------------

(define prim-chip-1(prim-chip chip-and-1))