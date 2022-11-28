#lang typed/racket

(require typed/rackunit)

; PROGRESS: Full project implemented.

(define-type ExprC (U numC strC idC appC ifC lamC primC errC))
(struct numC ([n : Real]) #:transparent)
(struct strC ([s : String]) #:transparent)
(struct idC ([s : Symbol]) #:transparent)
(struct appC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct ifC ([if : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct lamC ([args : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct primC ([o : Symbol] [l : ExprC] [r : ExprC]) #:transparent)
(struct errC ([v : ExprC]) #:transparent)

(define-type Value (U numV strV boolV closV primV))
(struct numV ([n : Real]) #:transparent)
(struct strV ([s : String]) #:transparent)
(struct boolV ([b : Boolean]) #:transparent)
(struct closV ([args : (Listof Symbol)] [body : ExprC] [env : Environment]) #:transparent)
(struct primV ([args : (Listof Symbol)] [body : ExprC]) #:transparent)

(define-type Binding (U bind))
(struct bind ([name : Symbol] [val : Value]) #:transparent)

(define-type-alias Environment (Listof Binding))
(define mt-env '())
(define top-env (cast (list (bind 'true (boolV true))
                        (bind 'false (boolV false))
                        (bind '+ (primV (list 'a 'b) (primC '+ (idC 'a) (idC 'b))))
                        (bind '- (primV (list 'a 'b) (primC '- (idC 'a) (idC 'b))))
                        (bind '* (primV (list 'a 'b) (primC '* (idC 'a) (idC 'b))))
                        (bind '/ (primV (list 'a 'b) (primC '/ (idC 'a) (idC 'b))))
                        (bind '<= (primV (list 'a 'b) (primC '<= (idC 'a) (idC 'b))))
                        (bind 'equal? (primV (list 'a 'b) (primC 'equal? (idC 'a) (idC 'b))))
                        (bind 'error (primV (list 'v) (errC (idC 'v)))))
                        Environment))

; takes a binding and an environment and returns the given environment
; with the binding added
(define (extend-env [b : Binding] [env : Environment]) : Environment
  (cons b env))

; takes a list of closure arguments, a list of application arguments,
; a closure environment, and an application environment and adds the bindings
; to the closure environment
(define (extend-env-mult [c-args : (Listof Symbol)] [f-args : (Listof ExprC)]
                         [c-env : Environment] [f-env : Environment]) : Environment
  (cond
    [(and (empty? c-args) (empty? f-args)) c-env]
    [(empty? c-args) (error 'extend-env-mult "too many arguments provided (JYSS)")]
    [(empty? f-args) (error 'extend-env-mult "too few arguments provided (JYSS)")]
    [else (extend-env-mult (rest c-args) (rest f-args)
                           (extend-env (bind (first c-args)
                                             (interp (first f-args) f-env))
                                       c-env) f-env)]))

; takes a symbol and an environment and returns the value
; of the binding that has that symbol, if there is one
(define (lookup [for : Symbol] [env : Environment]) : Value
  (cond
    [(empty? env) (error 'lookup "name not found (JYSS)")]
    [else (cond
            [(equal? for (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup for (rest env))])]))

; takes a JYSS5 expression and an environment
; and returns the value the expression evaluates to
(define (interp [e : ExprC] [env : Environment]) : Value
  (match e
    [(numC n) (numV n)]
    [(strC s) (strV s)]
    [(idC s) (lookup s env)]
    [(appC f a) (match (interp f env)
                  [(? closV? c)(local ([define f-value c])
                  (interp (closV-body f-value)
                          (extend-env-mult (closV-args f-value) a (closV-env f-value) env)))]
                  [(? primV? p)(local ([define f-value (closV (primV-args p) (primV-body p) env)])
                  (interp (closV-body f-value)
                          (extend-env-mult (closV-args f-value) a (closV-env f-value) env)))]
                  [other (error 'interp "application of a non-closure (JYSS)")])]
    [(primC o l r) (interp-prim o (interp l env) (interp r env))]
    [(errC v) (error 'user-error (serialize (interp v env)))]
    [(ifC a b c) (match (interp a env)
                   [(boolV ?) (cond
                                [? (interp b env)]
                                [else (interp c env)])]
                   [other (error 'interp "non-boolean provided as conditional (JYSS)")])]
    [(lamC a b) (closV a b env)]))

; takes a symbol and a left and right value and returns
; the result of the operation the symbol represents on the two given values
(define (interp-prim [o : Symbol] [l : Value] [r : Value]) : Value
  (match (list o l r)
    [(list '+ (? numV? l) (? numV? r)) (numV (+ (numV-n l) (numV-n r)))]
    [(list '- (? numV? l) (? numV? r)) (numV (- (numV-n l) (numV-n r)))]
    [(list '* (? numV? l) (? numV? r)) (numV (* (numV-n l) (numV-n r)))]
    [(list '/ (? numV? l) (? numV? r)) (match (numV-n r)
                                         [0 (error 'interp-prim "division by zero (JYSS)")]
                                         [r (numV (/ (numV-n l) r))])]
    [(list '<= (? numV? l) (? numV? r)) (boolV (<= (numV-n l) (numV-n r)))]
    [(list 'equal? l r) (cond
                          [(or (closV? l) (primV? l) (closV? r) (primV? r)) (boolV false)]
                          [else (boolV (equal? l r))])]
    [other (error 'interp-prim "invalid input (JYSS)")]))

; takes an s-expression and returns the serialized form of its evaluation
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))

; takes an s-expression and returns the JYSS5 expression that it represents 
(define (parse [s : Sexp]) : ExprC
  (cond
    [(real? s) (numC s)]
    [(list? s)
     (match s
       [(list 'if a b c) (ifC (parse a) (parse b) (parse c))]
       [(list 'error v) (errC (parse v))]
       [(list 'proc (list (? symbol? a) ...) 'go b)
        (cond
          [(or (member 'vars: a) (member 'body: a) (member 'if a) (member 'proc a) (member 'go a))
           (error 'parse "invalid argument (JYSS)")]
          [(check-duplicates a) (error 'parse "duplicate arguments (JYSS)")]
          [else (lamC (cast a (Listof Symbol)) (parse b))])]
       [(list 'vars: (list (? symbol? a) '= b) ... 'body: c)
        (cond
          [(or (member 'vars: a) (member 'body: a) (member 'if a) (member 'proc a) (member 'go a))
           (error 'parse "invalid argument (JYSS)")]
          [(check-duplicates a) (error 'parse "duplicate arguments (JYSS)")]
          [else (appC (lamC (cast a (Listof Symbol)) (parse c)) (parse-args (cast b (Listof Sexp))))])]
       [(list a b ...) (appC (parse a) (parse-args b))])]
    [(symbol? s)
     (cond
       [(or (equal? s 'vars:) (equal? s 'body:) (equal? s 'if)
            (equal? s 'proc) (equal? s 'go))
        (error 'parse "invalid input (JYSS)")]
       [else (idC s)])]
    [else (strC (format "~v" s))]))

; takes a list of s-expressions and returns the list of JYSS5 expressions they represent
(define (parse-args [s : (Listof Sexp)]) : (Listof ExprC)
  (match s
    ['() '()]
    [(cons first rest) (cons (parse first) (parse-args rest))]))

; takes a value and returns the serialization of the value
(define (serialize [v : Value]) : String
  (match v
    [(? numV? v) (format "~v" (numV-n v))]
    [(? strV? s) (strV-s s)]
    [(? boolV? v) (cond
                    [(boolV-b v) "true"]
                    [else "false"])]
    [(? closV? v) "#<procedure>"]
    [(? primV? v) "#<primop>"]))

(check-equal? (serialize (numV 400)) "400")
(check-equal? (interp (primC '+ (numC 10) (appC (lamC '() (numC 5)) '())) mt-env) (numV 15))
(check-equal? (parse '{{proc {x} go {+ 2 x}} 98})
              (appC (lamC '(x) (appC (idC '+) (list (numC 2) (idC 'x)))) (list (numC 98))))
(check-equal? (parse '{vars: [z = {+ 9 14}][y = 98] body: {+ z y}})
              (appC (lamC '(z y) (appC (idC '+) (list (idC 'z) (idC 'y))))
                    (list (appC (idC '+) (list (numC 9) (numC 14))) (numC 98))))
(check-exn (regexp (regexp-quote "parse: invalid argument (JYSS)"))
           (lambda () (parse '{proc {vars:} go {+ 3 1}})))
(check-exn (regexp (regexp-quote "parse: invalid argument (JYSS)"))
           (lambda () (parse '{vars: [go = \"\"] body: \"World\"})))
(check-exn (regexp (regexp-quote "parse: duplicate arguments (JYSS)"))
           (lambda () (parse '{vars: {z = {proc {} go 3}} {z = 9} body: {z}})))
(check-exn (regexp (regexp-quote "parse: duplicate arguments (JYSS)"))
           (lambda () (parse '{proc {x x} go 3})))
(check-exn (regexp (regexp-quote "parse: invalid input (JYSS)"))
           (lambda () (parse '{+ go 5})))
(check-exn (regexp (regexp-quote "parse: invalid input (JYSS)"))
           (lambda () (parse 'body:)))
(check-equal? (interp (appC (lamC '(x) (appC (idC '+) (list (numC 2) (idC 'x)))) (list (numC 98))) top-env) (numV 100))
(check-equal? (interp (appC (lamC '(x) (appC (idC '+) (list (numC 2) (idC 'x)))) (list (numC 98))) top-env) (numV 100))
(check-equal? (top-interp '{{proc {x} go {+ 2 x}} 98}) "100")
(check-equal? (top-interp '{vars: [z = {+ 9 14}][y = 98] body: {+ z y}}) "121")
(check-exn (regexp (regexp-quote "user-error: true"))
           (lambda () (top-interp '{error true})))
(check-exn (regexp (regexp-quote "user-error: \"1234\""))
           (lambda () (top-interp '{+ 4 {error "1234"}})))
(check-equal? (top-interp '{{proc {x} go {<= 2 x}} 98}) "true")
(check-exn (regexp (regexp-quote "lookup: name not found (JYSS)"))
           (lambda () (top-interp '{{proc {x} go {<= 2 y}} 98})))
(check-exn (regexp (regexp-quote "extend-env-mult: too many arguments provided (JYSS)"))
           (lambda () (top-interp '{{proc {} go 9} 17})))
(check-exn (regexp (regexp-quote "extend-env-mult: too few arguments provided (JYSS)"))
           (lambda () (top-interp '{{proc {x} go 9}})))
(check-exn (regexp (regexp-quote "interp-prim: invalid input (JYSS)"))
           (lambda () (top-interp '{+ + +})))
(check-exn (regexp (regexp-quote "interp: non-boolean provided as conditional (JYSS)"))
           (lambda () (top-interp '{if {+ 4 3} 7 8})))
(check-exn (regexp (regexp-quote "interp: application of a non-closure (JYSS)"))
           (lambda () (top-interp '{3 4 5})))
(check-equal? (top-interp '{{proc {x y} go {* {x 3} y}}
                            {proc {z} go {+ z 5}} -98}) "-784")
(check-equal? (top-interp '{equal?
                            {{{proc {n1 n2} go {proc {f a} go {n1 f {n2 f a}}}}
                              {proc {f a} go {f a}}
                              {proc {f a} go {f {f a}}}}
                             {proc {a} go {* 2 a}}
                             10}
                            80}) "true")
(check-equal? (top-interp '{{proc {x} go {<= 2 x}} -98}) "false")
(check-equal? (top-interp '{{proc {x} go {equal? 2 x}} 2}) "true")
(check-equal? (top-interp '{{proc {x} go {equal? 2 x}} -98}) "false")
(check-equal? (top-interp '{{proc {x} go {equal? {proc {} go {x}} 5}} -98}) "false")
(check-equal? (top-interp '{if true 0 1}) "0")
(check-equal? (top-interp '{if false 0 1}) "1")
(check-equal? (top-interp '{{proc {x} go {* 2 x}} 98}) "196")
(check-equal? (top-interp '{{proc {x} go {- 200 x}} 98}) "102")
(check-equal? (top-interp '{{proc {x} go {/ 200 x}} 40}) "5")
(check-equal? (top-interp '{{proc {x} go {/ 200 x}} 40}) "5")
(check-exn (regexp (regexp-quote "interp-prim: division by zero (JYSS)"))
           (lambda () (top-interp '{{proc {x} go {/ 200 x}} 0})))
(check-equal? (top-interp '{{proc {x y} go {+ x y}} 50 17}) "67")
(check-equal? (top-interp '{{proc {x} go {{proc {y} go {+ x y}} 50}} 17}) "67")
(check-equal? (top-interp '{proc {x} go {{proc {y} go {+ x y}} 50}}) "#<procedure>")
(check-equal? (top-interp '+) "#<primop>")