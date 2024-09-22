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
    (lambda (signal)
        (cond
            [(= signal 0) 'simple-circuit]
            [(= signal 1) in]
            [(= signal 2) out]
            [(= signal 3) chip]
            [else (eopl:error "Señal no válida")]
        )
    )
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
    (lambda (signal)
        (cond
            [(= signal 0) 'complex-circuit]
            [(= signal 1) circ]
            [(= signal 2) lcircs]
            [(= signal 3) in]
            [(= signal 4) out]
            [else (eopl:error "Señal no válida")]
        )
    )
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
    (lambda (signal)
        (cond
            [(= signal 0) 'comp-chip]
            [(= signal 1) in]
            [(= signal 2) out]
            [(= signal 3) circ]
            [else (eopl:error "Señal no válida")]
        )
    )
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
    (lambda (signal)
        (cond
            [(= signal 0) 'chip-or]
            [else (eopl:error "Señal no válida")]
        )
    )
  )
)

(define chip-and
  (lambda ()
    (lambda (signal)
        (cond
            [(= signal 0) 'chip-and]
            [else (eopl:error "Señal no válida")]
        )
    )
  )
)

(define chip-not
  (lambda ()
    (lambda (signal)
        (cond
            [(= signal 0) 'chip-not]
            [else (eopl:error "Señal no válida")]
        )
    )
  )
)


(define chip-xor
  (lambda ()
    (lambda (signal)
        (cond
            [(= signal 0) 'chip-xor]
            [else (eopl:error "Señal no válida")]
        )
    )
  )
)

(define chip-nand
  (lambda ()
    (lambda (signal)
        (cond
            [(= signal 0) 'chip-nand]
            [else (eopl:error "Señal no válida")]
        )
    )
  )
)


(define chip-nor
  (lambda ()
    (lambda (signal)
        (cond
            [(= signal 0) 'chip-nor]
            [else (eopl:error "Señal no válida")]
        )
    )
  )
)


(define chip-xnor
  (lambda ()
    (lambda (signal)
        (cond
            [(= signal 0) 'chip-xnor]
            [else (eopl:error "Señal no válida")]
        )
    )
  )
)


;; Pruebas
(chip-or)
(chip-and)
(chip-not)
(chip-xor)
(chip-nand)
(chip-nor)


;; Observadores

;; Predicados;;
(define simple-circuit?
    (lambda (simple-circuit)
        (eqv? ( simple-circuit 0) 'simple-circuit)
    )
)

;; Pruebas
(define sp (simple-circuit '(a) '(b) 'chip1))
(simple-circuit? sp)

(define complex-circuit?
    (lambda (complex-circuit)
        (eqv? ( complex-circuit 0) 'complex-circuit)
    )
)

(define comp-chip?
    (lambda (comp-chip)
        (eqv? ( comp-chip 0) 'comp-chip)
    )
)

(define chip-or?
    (lambda (chip-or)
        (eqv? ( chip-or 0) 'chip-or)
    )
)

(define chip-and?
    (lambda (chip-and)
        (eqv? ( chip-and 0) 'chip-and)
    )
)

(define chip-xor?
    (lambda (chip-xor)
        (eqv? ( chip-xor 0) 'chip-xor)
    )
)

(define chip-not?
    (lambda (chip-not)
        (eqv? ( chip-not 0) 'chip-not)
    )
)

(define chip-nand?
    (lambda (chip-nand)
        (eqv? ( chip-nand 0) 'chip-nand)
    )
)

(define chip-nor?
    (lambda (chip-nor)
        (eqv? ( chip-nor 0) 'chip-nor)
    )
)

(define chip-xnor?
    (lambda (chip-xnor)
        (eqv? ( chip-xnor 0) 'chip-xnor)
    )
)


;; Pruebas

(define cp (comp-chip '(INA INB INC IND) '(OUTA) (simple-circuit '(a b) '(c) 'chip1)))
(comp-chip? cp)
(define sp2 (simple-circuit '(a) '(b) 'chip1))
(simple-circuit? sp2)
(define cc (complex-circuit '(simple-circuit '(a) '(b) 'chip1) '((simple-circuit '(c) '(d) 'chip2)) '(a c) '(d)))
(complex-circuit? cc)
(define co (chip-or))
(chip-or? co)
(define ca (chip-and))
(chip-and? ca)
(define cx (chip-xor))
(chip-xor? cx)
(define cn (chip-nand))
(chip-nand? cn)
(define cnr (chip-nor))
(chip-nor? cnr)
(define cxn (chip-xnor))
(chip-xnor? cxn)


;; Extractores
(define simple-circuit->in
    (lambda (simple-circuit)
        (simple-circuit 1)
    )
)

(define simple-circuit->out
    (lambda (simple-circuit)
        (simple-circuit 2)
    )
)

(define simple-circuit->chip
    (lambda (simple-circuit)
        (simple-circuit 3)
    )
)

(define complex-circuit->circ
    (lambda (complex-circuit)
        (complex-circuit 1)
    )
)

(define complex-circuit->lcircs
    (lambda (complex-circuit)
        (complex-circuit 2)
    )
)

(define complex-circuit->in
    (lambda (complex-circuit)
        (complex-circuit 3)
    )
)

(define complex-circuit->out
    (lambda (complex-circuit)
        (complex-circuit 4)
    )
)

(define comp-chip->in
    (lambda (comp-chip)
        (comp-chip 1)
    )
)

(define comp-chip->out
    (lambda (comp-chip)
        (comp-chip 2)
    )
)

(define comp-chip->circ
    (lambda (comp-chip)
        (comp-chip 3)
    )
)

(define chip-or->symbol
    (lambda (chip-or)
        (chip-or 0)
    )
)

(define chip-and->symbol
    (lambda (chip-and)
        (chip-and 0)
    )
)

(define chip-not->symbol
    (lambda (chip-not)
        (chip-not 0)
    )
)

(define chip-xor->symbol
    (lambda (chip-xor)
        (chip-xor 0)
    )
)

(define chip-nand->symbol
    (lambda (chip-nand)
        (chip-nand 0)
    )
)

(define chip-nor->symbol
    (lambda (chip-nor)
        (chip-nor 0)
    )
)

(define chip-xnor->symbol
    (lambda (chip-xnor)
        (chip-xnor 0)
    )
)

;; Pruebas





