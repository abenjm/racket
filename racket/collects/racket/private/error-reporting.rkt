(module error-reporting "pre-base.rkt"

  ; module implements error reporting conforming to Racket error conventions

  (provide
   ;; structs
 
   error-report
   error-report?
           
   ; replaces short-field struct's constructor
   (rename-out [make-short-field short-field])
   short-field?
           
   ; replaces long-field struct's constructor
   (rename-out [make-long-field long-field])
   long-field?
           
   ; replaces ellipsis-field struct's constructor
   (rename-out [make-ellipsis-field ellipsis-field])
   ellipsis-field?

   ; predicate matching any short-field, long-field, and ellipsis-field
   error-field?
           
   ; use to indicate lack of provided value for an error-report's field
   ; since #f can be used in any of error-report's fields, need an alternative
   ; to indicate lack of valid values.
   absent
   absent?)

  (provide
   ;; procedures
   
   ; convert error-report to string for use as exn's message field
   error-report->string

   ; construct exn:fail:contract using an error-report
   exn:fail:contract/error-report

   ; commonly used error fields so provide for convenience
   expected-short-field
   expected-long-field
   given-short-field
   given-long-field)

  ;;; -----------------------------------------------------------------------------------------
  ;;; implementation section
  
  (require "struct.rkt")
  

  ; Control how long error details can be in error reporting output.
  (define error-detail-print-width
    (make-parameter 72
                    (lambda (v)
                      (if (exact-nonnegative-integer? v)
                          v
                          (raise-argument-error 'error-detail-print-width
                                                "exact-nonnegative-integer?"
                                                v)))))
  
  ; Assume that any/c and absent? are disjoint in rest of the module.
  (define absent (let ([private (let () (struct absent ()) (absent))]) (lambda () private)))
  (define (absent? v) (eq? (absent) v))
  
  ; Racket error reporting convention.
  ;
  ; <error-report> :-
  ; [<srcloc>:] [<name>:] <message>[;
  ;  <continued-message>] ...
  ;   [<short-field> | <long-field> | <collapisble-field>]
  ;   ...
  ;
  ; <short-field> :-
  ; <field>: <detail>
  ; 
  ; <long-field> :-
  ; <field>:
  ;  <detail>
  ;
  ; <ellipsis-field> :-
  ; <field>...:
  ;  <detail>

  ; struct error-field
  ; label : string/c
  ; detail : any/c
  ; detailfs (detail-format-style) : (or/c '~a '~v)
  (struct error-field (label detail detailfs)
    #:transparent
    #:guard (lambda (label detail detailfs struct-name)
              (unless (string? label)
                (raise-argument-error struct-name "string/c" 0 label detail detailfs))
              (unless (or (eq? detailfs '~a) (eq? detailfs '~v))
                (raise-argument-error struct-name "(or/c '~a '~v)" 2 label detail detailfs))
              (values label detail detailfs)))

  ; ~v is the default printing style for the field detail
  ; but allow ~a to be specified instead if desired.
  (define (make-constructor-with-optional-detailfs struct-name)
    (lambda (label detail [detailfs '~v])
      (struct-name label detail detailfs)))
  
  ; struct short-field  
  (struct short-field error-field () #:transparent)  
  (define make-short-field (make-constructor-with-optional-detailfs short-field))

  ; struct long-field  
  ; detail : (listof any/c)
  (struct long-field error-field ()
    #:transparent
    #:guard (lambda (label detail detailfs struct-name)
              (unless (list? detail)
                (raise-argument-error struct-name "(listof any/c)" 1 label detail))
              (values label detail detailfs)))  
  (define make-long-field (make-constructor-with-optional-detailfs long-field))

  ; struct ellipsis-field  
  ; detail : (listof any/c)
  (struct ellipsis-field error-field ()
    #:transparent
    #:guard (lambda (label detail detailfs struct-name)
              (unless (list? detail)
                (raise-argument-error struct-name "(listof any/c)" 1 label detail))
              (values label detail detailfs)))  
  (define make-ellipsis-field (make-constructor-with-optional-detailfs ellipsis-field))

  ; struct error-report
  ; srcloc : (or/c srcloc? absent?)
  ; name : (or/c any/c absent?)
  ; message : (or/c any/c absent?)
  ; continued-messages : (or/c (listof any/c) absent?)
  ; fields : (or/c (listof error-field?) absent?)
  (struct error-report (srcloc name message continued-messages fields)
    #:transparent
    #:guard (lambda (srcloc name message continued-messages fields struct-name)
              (unless (or (absent? srcloc) (srcloc? srcloc))
                (raise-argument-error struct-name
                                      "(or/c srcloc? absent?)"
                                      0
                                      srcloc
                                      name
                                      message
                                      continued-messages
                                      fields))
                
              (unless (or (absent? continued-messages) (list? continued-messages))
                (raise-argument-error struct-name
                                      "(or/c (listof any/c) absent?)"
                                      3
                                      srcloc
                                      name
                                      message
                                      continued-messages
                                      fields))
              (unless (or (absent? fields) (and (list? fields) (andmap error-field? fields)))
                (raise-argument-error struct-name
                                      "(or/c (listof error-field?) absent?)"
                                      4
                                      srcloc
                                      name
                                      message
                                      continued-messages
                                      fields))
              (values srcloc name message continued-messages fields)))

  ; cms : (listof any/c)
  ; Format continued-messages for error reporting output.
  ; Strings get special handling. If a string has line break characters then each break
  ; character is replaced by another break character that is postfixed with appropriate
  ; amount of whitespace for <continued-message> grammar form.
  (define (continued-messages-format cms)
    (define cms-format-string " ~a")
    (apply string-append (map (lambda (m)
                                (string-append "\n"
                                               (cond
                                                 [(string? m) (format cms-format-string (regexp-replace* #rx"\n" m "\n "))]
                                                 [else (format cms-format-string m)])))
                              cms)))

  ; short-field-format : short-field? -> string?
  ; Format short-field for error reporting output.
  ; If it's too long, meaning its printed
  ; representation exceeds error-detail-print-width then it's converted to
  ; long-field prior to formatting.
  (define (short-field-format sf)    
    (if (too-long? sf)
        (long-field-format (short-field->long-field sf))
        (if (eq? (error-field-detailfs sf) '~v)
            (format "  ~a: ~v" (error-field-label sf) (error-field-detail sf))
            (format "  ~a: ~a" (error-field-label sf) (error-field-detail sf)))))

  ; long-field-format : long-field? -> string?
  ; Format long-field for error reporting output.
  (define (long-field-format lf)
    (apply string-append (list* (format "  ~a:" (error-field-label lf))
                                (map (lambda (d)
                                       (string-append "\n"
                                                      (if (eq? (error-field-detailfs lf) '~v)
                                                          (format "   ~v" d)
                                                          (format "   ~a" d))))
                                     (error-field-detail lf)))))                               

  ; ellipsis-field-format : ellipsis-field? -> string?
  ; Format ellipsis-field for error reporting output.
  (define (ellipsis-field-format ef)
    (apply string-append (list* (format "  ~a...:" (error-field-label ef))
                                (map (lambda (d)
                                       (string-append "\n"
                                                      (if (eq? (error-field-detailfs ef) '~v)
                                                          (format "   ~v" d)
                                                          (format "   ~a" d))))
                                     (error-field-detail ef)))))

  ; too-long? : any/c -> boolean?
  ; checks to see if short-field when printed for error output would exceed
  ; (error-detail-print-width)
  (define (too-long? sf)
    (define detail (error-field-detail sf))
    (cond [(and (symbol? detail)
                (> (string-length (symbol->string detail)) (error-detail-print-width)))
           #t]
          [(and (string? detail)
                (> (string-length detail) (error-detail-print-width)))
           #t]
          [else #f]))

  (define (short-field->long-field sf)
    (long-field (error-field-label sf)
                (list (error-field-detail sf))
                (error-field-detailfs sf)))

  ; fields-format : (listof error-field?) -> string?
  (define (fields-format fs)
    (apply string-append (map (lambda (f) (string-append "\n"
                                                         (cond [(short-field? f) (short-field-format f)]
                                                               [(long-field? f) (long-field-format f)]
                                                               [(ellipsis-field? f) (ellipsis-field-format f)])))
                              fs)))

  ; exn:fail:contract/error-report : error-report? continuation-mark-set? -> exn:fail:contract?
  ; Make an exn:fail:contract using error-report as its message.
  (define (exn:fail:contract/error-report err-rpt cmarks)
    (exn:fail:contract (error-report->string err-rpt) cmarks))

  (define (error-report->string err-rpt)
    (unless (error-report? err-rpt) (raise-argument-error 'error-report->string "error-report?" err-rpt))
    
    (define srcloc (error-report-srcloc err-rpt))
    (define name (error-report-name err-rpt))
    (define message (error-report-message err-rpt))
    (define continued-messages (error-report-continued-messages err-rpt))
    (define fields (error-report-fields err-rpt))

    (define srcloc-string (if (absent? srcloc) "" (format "~v: " (srcloc->string srcloc))))
    (define name-string (if (absent? name) "" (format "~a: " name)))
    (define message-string (if (absent? message) "" (string-append (format "~a" message) (if (absent? continued-messages) "" ";"))))
    (define continued-messages-string (if (absent? continued-messages) "" (continued-messages-format continued-messages)))
    (define fields-string (if (absent? fields) "" (fields-format fields)))

    (string-append srcloc-string name-string message-string continued-messages-string fields-string))

  ;; --------------------------------------------
  ;; commonly used fields
  
  ; Expected fields always format detail using '~a style.
  (define (expected-short-field detail)
    (make-short-field "expected" detail '~a))

  (define (expected-long-field detail)
    (make-long-field "expected" detail '~a))

  (define (given-short-field detail)
    (make-short-field "given" detail))

  (define (given-long-field detail)
    (make-long-field "given" detail))
  )