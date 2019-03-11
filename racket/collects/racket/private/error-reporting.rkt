(module error-reporting "pre-base.rkt"
  (#%require racket/private/struct)
  (#%provide error-report
           error-report?
           short-field
           short-field?
           long-field
           long-field?
           ellipsis-field
           ellipsis-field?
           error-report->string
           exn:fail:contract/error-report
           expected-short-field
           expected-long-field
           given-short-field
           given-long-field)

  ;; To-dos add test cases! and do better job of appending line breaks to avoid
  ;; one extra break at the end.

  ;; Control how long error details can be in error reporting output.
  (define error-detail-print-width
    (make-parameter 72
                    (lambda (v)
                      (if (exact-nonnegative-integer? v)
                          v
                          (raise-argument-error 'error-detail-print-width
                                                "exact-nonnegative-integer?"
                                                v)))))
  
  ;; Assume that Any/c and absent? are disjoint in rest of the module.
  (define absent (let ([private (let () (struct absent ()) (absent))]) (lambda () private)))
  (define (absent? v) (eq? (absent) v))
  
  ;; Racket error reporting convention.
  ;;
  ;; <error-report> :-
  ;; [<srcloc>:] [<name>:] <message>[;
  ;;  <continued-message>] ...
  ;;   [<short-field> | <long-field> | <collapisble-field>]
  ;;   ...
  ;;
  ;; <short-field> :-
  ;; <field>: <detail>
  ;; 
  ;; <long-field> :-
  ;; <field>:
  ;;  <detail>
  ;;
  ;; <ellipsis-field> :-
  ;; <field>...:
  ;;  <detail>

  ;; struct error-field
  ;; label : String/c
  ;; detail : Any/c
  (struct error-field (label detail)
    #:transparent
    #:guard (lambda (label detail struct-name)
              (unless (string? label)
                (raise-argument-error struct-name "String/c" 0 label detail))
              (values label detail)))
  
  ;; struct short-field  
  (struct short-field error-field () #:transparent)

  ;; struct long-field  
  ;; detail : (Listof Any/c)
  (struct long-field error-field ()
    #:transparent
    #:guard (lambda (label detail struct-name)
              (unless (list? detail)
                (raise-argument-error struct-name "(Listof Any/c)" 1 label detail))
              (values label detail)))

  ;; struct ellipsis-field  
  ;; detail : (Listof Any/c)
  (struct ellipsis-field error-field ()
    #:transparent
    #:guard (lambda (label detail struct-name)
              (unless (list? detail)
                (raise-argument-error struct-name "(Listof Any/c)" 1 label detail))
              (values label detail)))

  ;; struct error-report
  ;; srcloc : (or/c srcloc? absent?)
  ;; name : (or/c Any/c absent?)
  ;; message : (or/c Any/c absent?)
  ;; continued-messages : (or/c (Listof Any/c) absent?)
  ;; fields : (or/c (Listof error-field?) absent?)
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
                                      "(or/c (Listof Any/c) absent?)"
                                      3
                                      srcloc
                                      name
                                      message
                                      continued-messages
                                      fields))
              (unless (or (absent? fields) (and (list? fields) (andmap error-field? fields)))
                (raise-argument-error struct-name
                                      "(or/c (Listof error-field?) absent?)"
                                      4
                                      srcloc
                                      name
                                      message
                                      continued-messages
                                      fields))
              (values srcloc name message continued-messages fields)))

  ;; cms : (Listof Any/c)
  ;; Format continued-messages for error reporting output.
  ;; Strings get special handling. If a string has line break characters then each break
  ;; character is replaced by another break character that is postfixed with appropriate
  ;; amount of whitespace for <continued-message> grammar form.
  (define (continued-messages-format cms)
    (define cms-format-string " ~a\n")
    (apply string-append (map (lambda (m)
                                (cond
                                  [(string? m) (format cms-format-string (regexp-replace* #rx"\n" m "\n "))]
                                  [else (format cms-format-string m)]))
                              cms)))

  ;; short-field-format : short-field? -> string?
  ;; Format short-field for error reporting output.
  ;; If it's too long, meaning its printed
  ;; representation exceeds error-detail-print-width then it's converted to
  ;; long-field prior to formatting.
  (define (short-field-format sf)
    (if (too-long? sf)
        (long-field-format (short-field->long-field sf))
        (format "  ~a: ~v\n" (error-field-label sf) (error-field-detail sf))))

  ;; long-field-format : long-field? -> string?
  ;; Format long-field for error reporting output.
  (define (long-field-format lf)
    (apply string-append (list* (format "  ~a:\n" (error-field-label lf))
                                (map (lambda (d) (format "   ~v\n" d))
                                     (error-field-detail lf)))))                               

  ;; ellipsis-field-format : ellipsis-field? -> string?
  ;; Format ellipsis-field for error reporting output.
  (define (ellipsis-field-format cf)
    (apply string-append (list* (format "  ~a...:\n" (error-field-label cf))
                                (map (lambda (d) (format "   ~v\n" d))
                                     (error-field-detail cf)))))

  ;; too-long? : any/c -> boolean?
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
                (error-field-detail sf)))

  ;; fields-format : (Listof error-field?) -> string?
  (define (fields-format fs)
    (apply string-append (map (lambda (f) (cond [(short-field? f) (short-field-format f)]
                                                [(long-field? f) (long-field-format f)]
                                                [(ellipsis-field? f) (ellipsis-field-format f)]))
                              fs)))

  ;; exn:fail:contract/error-report : error-report? continuation-mark-set? -> exn:fail:contract?
  ;; Make an exn:fail:contract using error-report as its message.
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
    (define message-string (if (absent? message) "" (string-append (format "~a" message) (if (absent? continued-messages) "" ";") "\n")))
    (define continued-messages-string (if (absent? continued-messages) "" (continued-messages-format continued-messages)))
    (define fields-string (if (absent? fields) "" (fields-format fields)))

    (string-append srcloc-string name-string message-string continued-messages-string fields-string))

  ;; Commonly used fields.
  (define (expected-short-field detail)
    (short-field "expected" detail))

  (define (expected-long-field detail)
    (long-field "expected" detail))

  (define (given-short-field detail)
    (short-field "given" detail))

  (define (given-long-field detail)
    (long-field "given" detail))
  )