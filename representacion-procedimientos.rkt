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

;; <chip> := <chip_prim>
;;           prim-chip(chip-prim)

;; prim-chip: chip-prim -> chip

;; Proposito: Construye un chip con un chip primitivo

(define prim-chip
  (lambda (chip-prim)
    (lambda (signal)
    (cond
        [(= signal 0) 'prim-chip]
        [(= signal 1) chip-prim]
        [else (eopl:error "Señal no válida")]
    )
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


;; Observadores

;; Predicados;;

(define simple-circuit?
    (lambda (simple-circuit)
        (eqv? ( simple-circuit 0) 'simple-circuit)
    )
)

(define sp (simple-circuit '(a) '(b) 'chip1))
(simple-circuit? sp)

(define complex-circuit?
    (lambda (complex-circuit)
        (eqv? ( complex-circuit 0) 'complex-circuit)
    )
)

(define prim-chip?
    (lambda (prim-chip)
        (equal? ( prim-chip 0) 'prim-chip)
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

(define prim-chip->chip-prim
    (lambda (prim-chip)
        (prim-chip 1)
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

(define simple-circuit-1 (simple-circuit '(a c) '(b) '(chip-and)))
(define simple-circuit-2 (simple-circuit '(a b c) '(c d) '(chip-or)))
(define simple-circuit-3 (simple-circuit '(a) '(b c) '(chip-not)))
(define simple-circuit-4 (simple-circuit '(a b) '(c d)  '(chip-or)))
(define simple-circuit-5 (simple-circuit '() '()'(chip-nor)))

;;---------------------------------------------------------------------------

(define complex-circuit-1 (complex-circuit simple-circuit-1 (list simple-circuit-2) '(a) '(b)))
(define complex-circuit-2 (complex-circuit simple-circuit-2 (list simple-circuit-1 simple-circuit-2) '(x) '(y)))
(define complex-circuit-3 (complex-circuit complex-circuit-1 (list simple-circuit-1 complex-circuit-2) '(w) '(y z)))
(define complex-circuit-4 (complex-circuit simple-circuit-4 (list simple-circuit-1)'(a d) '(b)))
(define complex-circuit-5 (complex-circuit complex-circuit-4 (list simple-circuit-3 simple-circuit-2) '(x y) '(a b)))

;;---------------------------------------------------------------------------------

(define chip-or-1(chip-or))
(define chip-and-1(chip-and))
(define chip-nand-1(chip-nand))
(define chip-xor-1(chip-xor))
(define chip-nor-1(chip-nor))
(define chip-xnor-1(chip-xnor))
(define chip-not-1(chip-not))

;;--------------------------------------------------------------------------------------

(define comp-chip-1 (comp-chip '(a b c) '(d) (list simple-circuit-1)))
(define comp-chip-2 (comp-chip '(a b) '(c d)(list complex-circuit-2)))
(define comp-chip-3 (comp-chip '(a) '(b) (list simple-circuit-4)))
(define comp-chip-4 (comp-chip '( a b c d) '(e f) (list complex-circuit-5 simple-circuit-2)))
(define comp-chip-5 (comp-chip '(a b c) '(d e f) (list simple-circuit-3)))
