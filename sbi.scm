#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.5 2019-01-04 17:04:42-08 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (dump-stdin)
    (let ((token (read)))
         (printf "token=~a~n" token)
         (when (not (eq? token eof)) (dump-stdin))))

(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))

;;
;;

;simple print function
(define (show label it)
    (display label)
    (display " = ")
    (display it)
    (newline)
)
;simple print for hash tables
(define (print-table table)
    (hash-for-each table (lambda (key value) (show key value)))
)
;simple print function that prints cars
(define (print-each-car program)
    (when (not (null? program))
        (show "car program" (car program))
        (print-each-car (cdr program))
    )
)

;; label definitions
(define *label-table* (make-hash))
(define (label-get key)
        (hash-ref *label-table* key #f))

(define (create-label-table program)
    (when (not (null? program))
        (when (> (length (car program)) 1)
            (cond [(symbol? (cadr (car program))) 
                ;store the 2nd object in the line as the key and the line number as the value
                (hash-set! *label-table* (cadr (car program)) program)])
            ;repeat recursively on the rest of the lines
        )
        (create-label-table (cdr program))
    )
)

(define *array-table* (make-hash))
;; variable definitions
(define *variable-table* (make-hash))
(define (variable-get key)
        (hash-ref *variable-table* key))
(define (variable-put! key value)
        (hash-set! *variable-table* key value))
(for-each
    (lambda (pair)
            (variable-put! (car pair) (cadr pair)))
    `(
        (nan     ,(/ 0.0 0.0))
        (eof     0.0)
        (pi      ,(acos -1.0))
        (e       ,(exp 1.0))

     ))

(define (print-type arg)
    (if (string? arg) (printf arg)
        (printf " ~a" (eval-expr arg))
    )
)

(define (call-print input)
    (for-each (lambda (x)
        (print-type x)
    ) input)
    (newline)
)

;; control function
(define (call-goto input)
    (if (eq? (label-get (car input)) #f) (display "hello\n") 
				   (interpret-program (label-get (car input))))
)

;; function definitions
(define *function-table* (make-hash))
(define (function-get key)
        (hash-ref *function-table* key))
(define (function-put! key value)
        (hash-set! *function-table* key value))
(for-each
    (lambda (pair)
            (function-put! (car pair) (cadr pair)))
    `(

        (abs     ,abs)
        (acos    ,acos)
        (asin    ,asin)
        (atan    ,atan)
        (ceil    ,ceiling)
        (cos     ,cos)
        (exp     ,exp)
        (floor   ,floor)
        (log     ,log)
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (log2   ,(lambda (x) (/ (log x) (log 2.0))))
        (round   ,round)
        (sin     ,sin)
        (sqrt    ,sqrt)
        (tan     ,tan)
        (trunc   ,truncate)
        (+       ,+)
        (-       ,-)
        (*       ,*)
        (/       ,/)
        (%       ,(lambda (x y) (- x (* (div x y) y))))
        (^       ,expt)
        ;; unecessary functions
        (log10_2 0.301029995663981195213738894724493026768189881)
        (sqrt_2  1.414213562373095048801688724209698078569671875)
        (div     ,(lambda (x y) (floor (/ x y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        ;; real functions
        (print   ,call-print)
	(goto	 ,call-goto)
     ))

;; takes in an expression
(define (eval-expr statement)
    (cond [(number? statement) (+ statement 0.0)]
          [(symbol? statement) (cond [(not (eq? (hash-ref *variable-table* statement 0) 0)) 
					(hash-ref *variable-table* statement 0)]
					[(not (eq? (hash-ref *array-table* statement 0) 0))
					(hash-ref *array-table* statement 0)]
					(else 0))
	  ]
	  [(pair? statement) (apply (hash-ref *function-table* (car statement) #f)
                                    (map eval-expr (cdr statement)))]
    )
)


(define (execute-program program)
    (create-label-table program)
;    (print-table *label-table*)
;    (print-table *variable-table*)
;    (print-table *function-table*)
    ;(print-each-car program)
    (interpret-program program)
)

(define (interpret-statement input)
    ((function-get (car input)) (cdr input))
)

(define (interpret-program program)
    (cond 
	  [ (null? program) (exit)]
	  [ (null? (cdar program)) (interpret-program (cdr program)) ]
	  [ (symbol? (cadar program)) ((interpret-statement (caddar program)) (interpret-program (cdr program)))]
          [ (pair? (cadar program)) ((interpret-statement (cadar program)) (interpret-program (cdr program)))]
    )
)

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* (  (sbprogfile (car arglist))
                 (program (readlist-from-inputfile sbprogfile)))
              ;(write-program-by-line sbprogfile program)
              (execute-program program)
        )
    )
)


(when (terminal-port? *stdin*)
      (main (vector->list (current-command-line-arguments))))


;;
;;


