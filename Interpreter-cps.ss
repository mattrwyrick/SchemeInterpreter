;:  Single-file version of the interpreter.
;; Easier to submit to server, probably harder to use in the development process

;(load "chez-init.ss") 

;-------------------+
;                   |
;    DATATYPES      |
;                   |
;-------------------+

; parsed expression

(define-datatype expression expression?  
  [var-exp        ; variable references
   (id symbol?)]
  
  [lit-exp        ; "Normal" data.  Did I leave out any types?
   (datum
    (lambda (x)
      (ormap 
       (lambda (pred) (pred x))
       (list number? vector? boolean? symbol? string? pair? null?))))]
  
  [lambda-exp1
    (params (list-of symbol?))
    (body (list-of expression?))]
  
  [lambda-exp2
    (param symbol?)
    (body (list-of expression?))]
  
  [lambda-exp3
    (params (list-of symbol?))
    (body (list-of expression?))]
  
  [while-exp
    (conditional expression?)
    (body (list-of expression?))]
  
  [begin-exp
    (bodies (list-of expression?))]
  
  [let-exp
    (vars (list-of symbol?))
    (exps (list-of expression?))
    (bodies (list-of expression?))]
  
  [let*-exp
    (vars (list-of symbol?))
    (values (list-of expression?))
    (body (list-of expression?))]
  
  [letrec-exp
    (proc-names (list-of symbol?))
    (idss (list-of (list-of symbol?)))
    (bodiess list?)
    (letrec-bodies (list-of expression?))]
  
  [app-exp        ; applications
   (rator expression?)
   (rands (list-of expression?))] 
  
  [if-exp
    (test-exp expression?)
    (then-exp expression?)
    (else-exp expression?)]
  
  [set!-exp
    (id symbol?)
    (value expression?)]
  
  [define-exp
    (id symbol?)
    (value expression?)]
  )


	
	

;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)]
  [recursively-extended-env-record
    (proc-names (list-of symbol?))
    (idss (list-of (list-of symbol?)))
    (bodiess list?)
    (env environment?)])

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  
  [closure (vars (list-of symbol?))
           (bodies (list-of expression?))
           (env environment?)]
  
  [closure1 (vars (list-of symbol?))
           (bodies (list-of expression?))
           (env environment?)]

  [closure2 (vars (list-of symbol?))
            (bodies (list-of expression?))
            (env environment?)]
  [closure3 (vars (list-of symbol?))
            (bodies (list-of expression?))
            (env environment?)])
	 
	

;-------------------+
;                   |
;    PARSER         |
;                   |
;-------------------+


; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

(define parse-exp         
  (lambda (datum)
    (cond
       [(symbol? datum) (var-exp datum)]
       [(pair? datum)
        (cond
        
       ;;;;;;;;;;;;;;;;;;;;
       ;LAMBDA EXPRESSIONS;
       ;;;;;;;;;;;;;;;;;;;;
       [(eqv? 'lambda (1st datum))
        (cond
          [(< (length datum) 3)
            (eopl:error 'parse-exp "Lambda missing args or body: ~s" datum)]
          [(list? (2nd datum))
           (lambda-exp1 (2nd datum) 
             (map parse-exp (cddr datum)))]
          [(symbol? (2nd datum))
           (lambda-exp2 (2nd datum) 
             (map parse-exp (cddr datum)))]
          [(pair? (2nd datum))
           (lambda-exp3 (pair-to-list (2nd datum))
             (map parse-exp (cddr datum)))]
          [else
            (eopl:error 'parse-exp "Invalid Lambda: ~s" datum)]
          )]
        
        ;;;;;;;;;;;;;;;;;
        ;LET EXPRESSIONS;
        ;;;;;;;;;;;;;;;;;
        [(eqv? 'let (1st datum))
         (cond
           [(< (length datum) 3)
              (eopl:error 'parse-exp "Let missing args or body: ~s" datum)]
           [(let-params? (2nd datum))
            (syntax-expand datum)]
           
           [(symbol? (2nd datum))
            (syntax-expand (cons 'nlet (cdr datum)))]
           
           [else
            (eopl:error 'parse-exp "Invalid Let: ~s" datum)]
            )]
        
        ;;;;;;;;;;;;;;;;;;
        ;LET* EXPRESSIONS;
        ;;;;;;;;;;;;;;;;;;
        [(eqv? 'let* (1st datum))
         (cond
           [(< (length datum) 3)
              (eopl:error 'parse-exp "Let* missing args or body: ~s" datum)]
           [(let-params? (2nd datum))
            (syntax-expand datum)]
           [else
            (eopl:error 'parse-exp "Invalid Let*: ~s" datum)]
            )]
        
        ;;;;;;;;;;;;;;;;;;;;
        ;LETREC EXPRESSIONS;
        ;;;;;;;;;;;;;;;;;;;;
        [(eqv? 'letrec (1st datum))
         (cond
           [(< (length datum) 3)
              (eopl:error 'parse-exp "Letrec missing args or body: ~s" datum)]
           [(let-params? (2nd datum))
            (syntax-expand datum)] 
           [else
            (eopl:error 'parse-exp "Invalid Letrec: ~s" datum)]
            )]
        
          
          
        ;;;;;;;;;;;;;;;;;;;;
        ;DEFINE EXPRESSIONS;
        ;;;;;;;;;;;;;;;;;;;;
        [(eqv? 'define (1st datum))
         (define-exp (2nd datum) (parse-exp (caddr datum)))]
          
          
          
        ;;;;;;;;;;;;;;;;;;;;
        ;IF EXPRESSIONS;
        ;;;;;;;;;;;;;;;;;;;;
        [(eqv? 'if (1st datum))
         (cond
           [(equal? (length datum) 4)
            (if-exp (parse-exp (2nd datum))
              (parse-exp (3rd datum))
              (parse-exp (4th datum)))]
           
           [(equal? (length datum) 3)
            (if-exp (parse-exp (2nd datum))
              (parse-exp (3rd datum))
              (lit-exp '()))]
                    
           
           [else
              (eopl:error 'parse-exp "If bad # of args: ~s" datum)]
            )]
          
        ;;;;;;;;;;;;;;;;;;;
        ;COND EXPRESSIONS;
        ;;;;;;;;;;;;;;;;;;
        [(eqv? 'cond (1st datum))
         (syntax-expand datum)]
         
          
        ;;;;;;;;;;;;;;;;;;;
        ;BEGIN EXPRESSIONS;
        ;;;;;;;;;;;;;;;;;;
        [(eqv? 'begin (1st datum))
         (begin-exp (map parse-exp (cdr datum)))]
          
        
        ;;;;;;;;;;;;;;;;;;;
        ;WHILE EXPRESSIONS;
        ;;;;;;;;;;;;;;;;;;
        [(eqv? 'while (1st datum))
         (while-exp (parse-exp (2nd datum)) (map parse-exp (cddr datum)))]
          
        ;;;;;;;;;;;;;;;;;;
        ;CASE EXPRESSIONS;
        ;;;;;;;;;;;;;;;;;;
        [(eqv? 'case (1st datum))
         (syntax-expand datum)]
     
          
        ;;;;;;;;;;;;;;;;;;;;
        ;SET! EXPRESSIONS;
        ;;;;;;;;;;;;;;;;;;;;
        [(eqv? 'set! (1st datum))
         (cond
           [(not (equal? (length datum) 3))
              (eopl:error 'parse-exp "set! bad # of args: ~s" datum)]
           [else
             (set!-exp (2nd datum) (parse-exp (3rd datum)))]               
            )]
        
        ;;;;;;;;;;;;;;;;;;;
        ;QUOTE EXPRESSIONS;
        ;;;;;;;;;;;;;;;;;;;
        [(eqv? 'quote (1st datum))
         (cond
           [(< (length datum) 2)
              (eopl:error 'parse-exp "quote bad # of args: ~s" datum)]
           [else
             (lit-exp (cadr datum))] 
            )]
        
        
        ;;;;;;;;;;;;;;;;;;
        ;PROC EXPRESSIONS;
        ;;;;;;;;;;;;;;;;;;
         [else
           (app-exp (parse-exp (1st datum)) 
             (map parse-exp (cdr datum)))]
          
          )]        

      [(is-lit? datum) (lit-exp datum)]
      
      [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))


(define list-of
  (lambda (proc)
    (lambda (ls)
      (andmap proc ls))))
      
(define let-params?
  (lambda (ls)
    (cond
      [(null? ls) #t]
      [(not (list? ls)) #f]
      [(not (list? (car ls))) #f]
      [(not (eqv? 2 (length (car ls)))) #f]
      [(not (symbol? (caar ls))) #f]
      [else (let-params? (cdr ls))]
      )))
      
(define let-get-vars
  (lambda (ls)
    (let loop ([ls ls] [rls '()])
      (if (null? ls)
          rls
          (loop (cdr ls) (append rls (list (caar ls))))))))
      
(define let-get-values
  (lambda (ls)
    (let loop ([ls ls] [rls '()])
      (if (null? ls)
          rls
          (loop (cdr ls) (append rls (list (cadar ls))))))))

(define is-lit?
  (lambda (x)
     (ormap 
       (lambda (pred) (pred x))
       (list number? vector? boolean? symbol? string? pair? null?))))

(define andee 
    (lambda (x)
      (if (null? x)
          #t
          (let loop ([ls x])
            (if (null? (cdr ls))
                (car ls)
                (let ([val (loop (cdr ls))])
                  (if (and (car ls) val)
                      val
                      #f)))))))

(define oree
  (lambda (x)
    (if (null? x)
        #f
        (if (car x)
            (car x)
            (oree (cdr x))))))
        
(define pair-to-list
  (lambda (x)
    (if (symbol? x) 
        (list x)
        (append (list (car x)) (pair-to-list (cdr x))))))
         





;-------------------+
;                   |
;   ENVIRONMENTS    |
;                   |
;-------------------+





; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))

(define apply-env
  (lambda (env sym k)
    (cases environment env
      (empty-env-record ()
        (apply-env-global sym k))
      
      (extended-env-record (syms vals env)
	(let ((pos (list-find-position sym syms)))
      	  (if (number? pos)
	      (apply-k k (list-ref vals pos))
	      (apply-env env sym k))))
      
      [recursively-extended-env-record
        (procnames idss bodiess old-env)
        (let ([pos
                (list-find-position sym procnames)])
          (if (number? pos)
              (apply-k k (closure (list-ref idss pos)
                (list-ref bodiess pos)
                env))
              (apply-env old-env sym k)))]
      )))

(define apply-env-global
  (lambda (sym k)
    (cases environment init-env
      [empty-env-record ()
        (display 'fail01)]
      
      [extended-env-record (syms vals env)
	(let ((pos (list-find-position sym syms)))
      	  (if (number? pos)
	      (apply-k k (list-ref vals pos))
	      (eopl:error 'apply-env ; procedure to call if id not in env
		          "variable not found in environment: ~s"
			   sym)))]
      [recursively-extended-env-record (procnames idss bodiess old-env)
        (display 'fail-recurs-env)]
      )))

(define apply-env-set!
  (lambda (env sym value)
    (cases environment env
      
      [empty-env-record ()
        (apply-env-global-set! sym value)]
      
      [extended-env-record (syms vals env)
	(let ([pos (list-find-position sym syms)])
      	  (if (number? pos)
      	      (list-set! vals pos value)
              (apply-env-set! env sym value)))]
      
       [recursively-extended-env-record
         (procnames idss bodiess old-env)
         (display 'fail2)])))

(define apply-env-global-set!
  (lambda (sym value)
    (cases environment init-env
      [empty-env-record ()
        (display 'fail3)]
      
      [extended-env-record (syms vals env)
	(let ([pos (list-find-position sym syms)])
      	  (if (number? pos)
      	      (list-set! vals pos value)
              (display'fail4)))]
      
       [recursively-extended-env-record
         (procnames idss bodiess old-env)
         (display 'fail5)])))






;no mutation
(define extend-env-recursively
  (lambda (proc-names idss bodiess old-env)
    (recursively-extended-env-record
      proc-names idss bodiess old-env)))

;mutation
(define extend-env-recursively1
  (lambda (proc-names idss bodiess old-env)
    (let ([len (length proc-names)])
      (let ([vec (make-vector len)])
        (let ([env (extended-env-record
                     proc-names vec old-env)])
          (for-each
            (lambda (pos ids bodies)
              (vector-set! vec
                pos
                (closure ids bodies env)))
            (iota len)
            idss
            bodiess)
          env)))))



(define iota-helper
  (lambda (cur max)
    (if (> cur (- max 1))
        '()
        (cons cur (iota-helper (+ cur 1) max)))))

(define iota
  (lambda (num)
    (iota-helper 0 num)))


(define list-set!
  (lambda (ls ref val)
    (if (zero? ref)
        (set-car! ls val)
        (list-set! (cdr ls) (- ref 1) val))))






;-----------------------+
;                       |
;   SYNTAX EXPANSION    |
;                       |
;-----------------------+

(define syntax-expand
  (lambda (exp)
    (case (car exp)
      [(let) (expand-let (cdr exp))]
      [(cond) (expand-cond (cdr exp))]
      [(let*) (expand-let* (cdr exp))]
      [(case) (expand-case (cdr exp))]
      [(letrec) (expand-letrec (cdr exp))]
      [(nlet) (expand-nlet (cdr exp))]
      )))
      

(define expand-cond
  (lambda (exp)
    (if (not (null? (cdr exp)))
        [if-exp (parse-exp (caar exp)) (parse-exp (cadar exp)) 
          (expand-cond (cdr exp))]
        (if (eqv? (caar exp) 'else)
            [if-exp (parse-exp '#t) (parse-exp (cadar exp)) 
              (lit-exp '())]
            [if-exp (parse-exp (caar exp)) (parse-exp (cadar exp)) 
              (parse-exp '(void))]))))
       
(define expand-let
  (lambda (exp)
    (app-exp
      (lambda-exp1 (let-get-vars (1st exp))
                   (map parse-exp (cdr exp)))
       (map parse-exp (let-get-values (1st exp))))))

(define expand-let*
  (lambda (exp)
    (parse-exp (make-lets-from-* (1st exp) (2nd exp)))))
    
    
(define make-lets-from-*
  (lambda (vars body)
    (if (null? vars)
        body
        (list 'let (list (car vars)) (make-lets-from-* (cdr vars) body)))))

(define expand-case
  (lambda (exp)
    
    (let ([value (1st exp)] [body (cdr exp)])
     
      (let outer ([bodies body])
        (if (null? bodies)
            (lit-exp '())  
            
            (let inner ([memls (caar bodies)] [rval (cadar bodies)])
              (if (null? memls)
                  (outer (cdr bodies))
                  (if (equal? memls 'else)
                      (parse-exp rval)
                      (if-exp (parse-exp (list 'equal? value (car memls)))
                        (parse-exp rval)
                        (inner (cdr memls) rval))))))))))


(define expand-letrec
  (lambda (exp)
    (display (lr-get-bodies (car exp)))
    (letrec-exp (lr-get-procs (car exp))
                (lr-get-idss (car exp))
                (lr-get-bodies (car exp))
                (map parse-exp (cdr exp)))))


(define lr-get-procs
  (lambda (exp)
    (if (null? exp)
        '()
        (cons (convert-pair-to-list (caar exp))
              (lr-get-procs (cdr exp))))))

(define convert-pair-to-list
  (lambda (exp)
    (if (not (pair? exp))
        exp
        (let loop ([exp exp])
          (if (symbol? exp)
              (list exp)
              (cons (car exp) (loop (cdr exp))))))))

(define lr-get-idss
  (lambda (exp)
    (if (null? exp)
        '()
        (cons (cadr (cadar exp)) (lr-get-idss (cdr exp))))))

(define lr-get-bodies
  (lambda (exp)
    (debug-lr-bodies (map parse-exp (lr-get-bodies-help exp)))))
        
        
(define lr-get-bodies-help
  (lambda (exp)
    (if (null? exp)
        '()
        (cons (caddr (cadar exp)) (lr-get-bodies-help (cdr exp))))))

;adds extra lists to if-exps
(define debug-lr-bodies
  (lambda (exp)
    (if (null? exp)
        '()
        (append (list (list (car exp)))
          (debug-lr-bodies (cdr exp))))))

(define expand-nlet
  (lambda (exp)
    (expand-letrec (nlet-helper exp))))

(define nlet-helper
  (lambda (exp)
    (list
      (list (list (car exp)
              (list 'lambda 
                (let-get-vars (cadr exp))
                (caddr exp)
                )))
      (append (list (car exp)) (let-get-values (cadr exp))))))
        

;-------------------+
;
;    CONTINUATION
;
;-------------------+

(define scheme-value?
  (lambda (x)
    #t))

(define-datatype continuation continuation?
  [test-k
    (then-exp expression?)
    (else-exp expression?)
    (env environment?)
    (k scheme-value?)]
  
  [rator-k (rands (list-of expression?))
    (env environment?)
    (k scheme-value?)]
  
  [rands-k (proc-value scheme-value?)
    (k scheme-value?)]
  
  [let-rands-k (vars (list-of symbol?))
               (bodies (list-of expression?))
               (env environment?)
               (k scheme-value?)]
  [identity-k (k scheme-value?)]
  
  )


(define apply-k
  (lambda (k1 val)
       
    (cases continuation k1
      [test-k (then-exp else-exp env k)
        (if val
            (eval-exp then-exp env k)
            (eval-exp else-exp env k))]
      
      [rator-k (rands env k)
        (eval-rands rands
          env
          (rands-k val k))]
      
      [rands-k (proc-value k)
        (apply-proc proc-value val k)]
      
      [let-rands-k (vars bodies env k)
        (eval-bodies bodies (extended-env vars val env) k)]
      
      [identity-k (k)
        (k val)]
      
      )))




;-------------------+
;                   |
;   INTERPRETER     |
;                   |
;-------------------+



; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    ;(eval-exp form init-env)
    (let ([rval (eval-exp form (empty-env) (identity-k (lambda (x) x)))])
      (if exit-bool
          (begin
            (set! exit-bool #f)
            exit-value
            )
      (if (contains-escape (list rval))
          (cdadr rval)
          rval))
    )))


; eval-exp is the main component of the interpreter

(define eval-exp
  
  (lambda (exp env k)
    
    (cases expression exp
      
      ;CHANGE v1
      [lit-exp (datum)
        (apply-k k datum)]
      
      [var-exp (id)
       	(apply-env env id k)]   

      [set!-exp (id value)
        (apply-env-set! env id (eval-exp value env k))]
      
      [define-exp (id value)
          (set! init-env 
            (list (car init-env)
              (cadr init-env)
              (append (list (eval-exp value env k)) (caddr init-env))
              (cadddr init-env)))
          
          (set! init-env 
            (list (car init-env)
              (append (list id) (cadr init-env))
              (caddr init-env)
              (cadddr init-env)))
        ]
          
      [lambda-exp1 (params body)
        (apply-k k (closure1 params body env))]
      
      [lambda-exp2 (param body)
        (apply-k k (closure2 (list param) body env))]
      
      [lambda-exp3 (params body)
        (apply-k k (closure3 params body env))]
      
      [let-exp (vars exps bodies)
        (eval-rands exps env (let-rands-k vars bodies env k))]
      
      [letrec-exp (proc-names idss bodiess letrec-bodies)
        (eval-bodies letrec-bodies (extend-env-recursively
                                     proc-names idss bodiess env) k)]
      
      [begin-exp (body)
        (eval-bodies body env k)]
      
      [while-exp (conditional body)
        (let loop [(check conditional)]
          (if (eval-exp check env k)
              (begin (eval-bodies body env k)
                     (loop check))
              '()))]
      
      
       [if-exp (test-exp then-exp else-exp)
         (eval-exp test-exp
           env
           (test-k then-exp else-exp env k))]
  
    
      [app-exp (rator rands)
        (eval-exp rator
          env
          (rator-k rands env k))]
      
      
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env k)
    (apply-k k (map (lambda (x) (eval-exp x env 
                                  (identity-k (lambda (x) x)))) rands))
    ))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define eval-bodies
  (lambda (bodies env k)
    (if (null? (cdr bodies))
        (eval-exp (car bodies) env k)
        (begin (eval-exp (car bodies) env k)
               (eval-bodies (cdr bodies) env k)))))

(define apply-proc
  (lambda (proc-value args k)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args k)]
			; You will add other cases
              
      
      [closure (vars bodies env)
        (let ([new-env (extend-env vars args env)])
              (eval-bodies bodies new-env k))]
      
      [closure1 (vars bodies env)
        (let ([new-env (extend-env vars args env)])
              (let ([val (eval-bodies bodies new-env k)])
                (if (and (list? val) (eqv? 'escape-val (car val)))
                    (begin
                      (set! escape-bool #f)
                      (cadr val))
                    val))
          )]
      
      [closure2 (vars bodies env)
        (let ([new-env (extend-env vars (list args) env)])
              (eval-bodies bodies new-env k))]
      
      [closure3 (vars bodies env)
        (let ([new-env (extend-env vars (c3-param-setup vars args) env)])
              (eval-bodies bodies new-env k))]
      
      
      [else (eopl:error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))
        
(define c3-param-setup
  (lambda (vars args)
    (if (null? (cdr vars))
        (list args)
        (append (list (car args)) (c3-param-setup (cdr vars) (cdr args))))))

(define *prim-proc-names* '(+ - * / < > >= <= zero? not car cdr
                            caar cddr cadr cdar caaar cadar caadr
                            cddar cdddr cdadr cdaar caddr list
                            null? assq eq? equal? eqv? atom? add1 sub1
                            cons = length list->vector list? pair?
                            procedure? vector->list vector make-vector
                            vector-ref vector-set! number? symbol? 
                            set-car! set-cdr! display newline vector?
                            map apply quotient void list-tail append 
                            call/cc escape and or exit-list))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))

(define exit-bool #f)
(define exit-value '())

(define escape-bool #f)
(define escape-value '())
(define escape-closure '())

(define contains-escape
  (lambda (ls)
    (cond
      [(null? ls) #f]
      [(not (list? ls)) #f]
      [(and (list? (car ls)) (not (null? (car ls))) (eqv? (caar ls) 'escape-val)) (car ls)]
      [else (contains-escape (cdr ls))])))
        

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args k)
    
    (let ([check (contains-escape args)])
      (if check
          check
        
    (case   prim-proc
       [(call/cc) (apply-k (rator-k (list (list 'var-exp 'escape))
                            (empty-env) k)
                    (1st args))]
        
       [(escape) (if escape-bool
                     escape-value
                     (begin
                       (set! escape-bool #t)
                       (set! escape-value (list 'escape-val (1st args)))
                       (escape-value)))]
       [(exit-list) (if (not exit-bool)
                        (begin
                          (set! exit-bool #t)
                          (set! exit-value args)
                          (car args))
                        (car args))]
       [(+) (apply-k k (apply + args))]
       [(-) (apply-k k (apply - args))] 
       [(*) (apply-k k(apply * args))] 
       [(/) (apply-k k (apply / args))] 
       [(<) (apply-k k (< (1st args) (2nd args)))]
       [(>) (apply-k k (> (1st args) (2nd args)))]
       [(>=) (apply-k k (>= (1st args) (2nd args)))]
       [(<=) (apply-k k (<= (1st args) (2nd args)))]   
       [(zero?) (apply-k k (zero? (1st args)))]
       [(not) (apply-k k (not (1st args)))]
       [(car) (apply-k k (car (1st args)))]
       [(cdr) (apply-k k (cdr (1st args)))]
       [(caar) (apply-k k (caar (1st args)))]
       [(cddr) (apply-k k (cddr (1st args)))]
       [(cadr) (apply-k k (cadr (1st args)))]
       [(cdar) (apply-k k (cdar (1st args)))]
       [(caaar) (apply-k k (caaar (1st args)))]
       [(cadar) (apply-k k (cadar (1st args)))]
       [(caadr) (apply-k k (caadr (1st args)))]
       [(cddar) (apply-k k (cddar (1st args)))]
       [(cdddr) (apply-k k (cdddr (1st args)))]
       [(cdadr) (apply-k k (cdadr (1st args)))]
       [(cdaar) (apply-k k (cdaar (1st args)))]
       [(caddr) (apply-k k (caddr (1st args)))]
       [(list) (apply-k k (apply list args))]
       [(null?) (apply-k k (null? (1st args)))]
       [(assq) (apply-k k (assq (1st args) (2nd args)))]
       [(eq?) (apply-k k (eq? (1st args) (2nd args)))]
       [(equal?) (apply-k k (equal? (1st args) (2nd args)))]
       [(eqv?) (apply-k k (eqv? (1st args) (2nd args)))]
       [(atom?) (apply-k k (atom? (1st args)))]      
       [(add1) (apply-k k (+ (1st args) 1))]
       [(sub1) (apply-k k (- (1st args) 1))]
       [(cons) (apply-k k (cons (1st args) (2nd args)))]
       [(=) (apply-k k (= (1st args) (2nd args)))]
       [(length) (apply-k k (length (1st args)))]
       [(list->vector) (apply-k k (list->vector (1st args)))]
       [(list?) (apply-k k (list? (1st args)))]
       [(pair?) (apply-k k (pair? (1st args)))]
       [(procedure?) (apply-k k (and (list? (1st args)) (or
                             (equal? (car (1st args)) 'closure1)
                             (equal? (car (1st args)) 'closure2)
                             (equal? (car (1st args)) 'prim-proc))))]
                       
       [(vector->list) (apply-k k (vector->list (1st args)))]
       [(vector) (apply-k k (apply vector args))]
       [(vector?) (apply-k k (vector? (1st args)))]
       [(make-vector) (apply-k k (make-vector (1st args) (2nd args)))]
       [(vector-ref) (apply-k k (vector-ref (1st args) (2nd args)))]
       [(vector-set!) (apply-k k (vector-set! (1st args) (2nd args) (3rd args)))]
       [(number?) (apply-k k (number? (1st args)))]
       [(symbol?) (apply-k k (symbol? (1st args)))]
       [(set-car!) (apply-k k (set-car! (1st args) (2nd args)))]
       [(set-cdr!) (apply-k k (set-cdr! (1st args) (2nd args)))]
       [(and) (andee args)]
       [(or) (oree args)]
       [(display) (apply-k k (display (1st args)))]
       [(newline) (apply-k k (newline))]
                    
       [(apply) (apply-prim-proc (cadar args) (2nd args) k)]
       [(map) (map (lambda (x) (apply-proc (1st args) (list x) k)) (2nd args))]
       
       [(quotient) (apply-k k (quotient (1st args) (2nd args)))]
       [(void) (apply-k k (void))]
       [(list-tail) (apply-k k (list-tail (1st args) (2nd args)))]
                      
       [(append) (append (1st args) (2nd args))]
      
      
      
       [else (error 'apply-prim-proc 
               "Bad primitive procedure name: ~s" 
               prim-proc)]
      )))))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (parse-exp x))))
    
(define reset-global-env
  (lambda ()
    (set! init-env 
      (extend-env           
        *prim-proc-names*  
        (map prim-proc      
          *prim-proc-names*)
        (empty-env)))))