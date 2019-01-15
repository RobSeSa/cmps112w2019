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
;    (printf "(car program) = ~s~n" (car program))
;    (map (lambda (output) (printf "(car program) = ~s~n" (car output))) program)
;    (map (lambda (output) (printf "(cdr program) = ~s~n" (cdr output))) program)
;    (map (lambda (output) (printf "(cadr program) = ~s~n" (cadr output))) program)
;    (map (lambda (output) (printf "(length output) = ~s~n" (length output))) program)
;    (map (lambda (output) (printf "(caadr program) = ~s~n" (caadr output))) program)
;    (print-each-car program)
;    (dump-stdin))


(define (print-each-car program)
   (if (not (null? program)) 
        [(printf "(car program = ~s~n" (car program)) 
         (printf "(cdr program = ~s~n" (cdr program)) 
         (printf "(length (car program) = ~s~n" (length (car program)))
         (printf "(length (car program) = ~s~n" (length (car program)))
         (print-each-car (cdr program))]
        [(printf "eof was reached")]
   )
)

;simple print function
(define (show label it)
    (display label)
    (display " = ")
    (display it)
    (newline)
)

(define *label-table* (make-hash))
(define (print-table table)
    (hash-for-each table (lambda (key value) (show key value)))
)
(define (create-label-table program)
    (when (not (null? program))
        ;when the number of objects in the first line in the list is > 2
        ;this implies that there is a line number, label, and statement
        ;;(when (> (length (car program)) 2)
        (cond [(symbol? (cadr (car program))) 
            ;store the 2nd object in the line as the key and the line number as the value
            (hash-set! *label-table* (cadr (car program)) (car (car program)))])
        ;repeat recursively on the rest of the lines
        (create-label-table (cdr program))
    )
)

(define (execute-program program)
    (display "in execute-program")
    (newline)
    (create-label-table program)
    (print-table *label-table*)
)

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* (  (sbprogfile (car arglist))
                 (program (readlist-from-inputfile sbprogfile)))
;              (write-program-by-line sbprogfile program)
              (execute-program program)
        )
    )
)


(when (terminal-port? *stdin*)
      (main (vector->list (current-command-line-arguments))))


;;
;;


