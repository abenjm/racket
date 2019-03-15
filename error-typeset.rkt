#lang racket/base

(module pieces racket/private/pre-base
  (provide (rename-out [make-p p]                       
                       [make-div div]
                       [make-line line]
                       [make-indent indent])
           p?
           p-v
           p-format-mode
           div?
           div-content
           line?
           line-content
           indent?
           indent-content
           indent-amount)
  
  (require racket/private/struct)

  (define (format-mode? v)
    (or (eq? v 'w) (eq? v 'd) (eq? v 'p)))
  
  (struct p (v format-mode indent-amount)
    #:transparent
    #:guard (lambda (v format-mode indent-amount struct-name)
              (unless (format-mode? format-mode)
                (raise-argument-error struct-name "format-mode?" 1 v format-mode))
              (values v format-mode indent-amount)))

  ; content is a list of p or span or indent
  (struct div (content) #:transparent)

  ; content is a list of p
  (struct span (content) #:transparent) 

  ; amount of exact-nonnegative-integer?
  ; content is a list of p or span or indent or div
  (struct indent (amount content) #:transparent
    #:guard (lambda (amount content struct-name)
              (unless (exact-nonnegative-integer? amount)
                (raise-argument-error struct-name "exact-nonnegative-integer?" amount))
              (values amount content)))

  (define (plain-value? v)
    (and (not (p? v))
         (not (span? v))
         (not (indent? v))
         (not (div? v))))
  
  (define (plain-values->p lst)
    (map (lambda (v)
           (if (plain-value? v)
               (make-p v)
               v))
         lst))

  (define (make-p v [format-mode 'p])
    (p v format-mode))

  (define (make-div . content)
    (div (plain-values->p content)))
  
  (define (make-line . content)
    (line (plain-values->p content)))

  (define (make-indent amount . content)
    (indent amount (plain-values->p content)))
  
  )

(define (render content)  
  (cond
    [(not (list? content)) (render-item content)]
    [(null? content) ""]
    [else
     (apply string-append
            (map (lambda (i)
                   (render-item i))
                 content))]))

(define (render-item i)
  (cond
    [(p? i) (render-p i)]
    [(line? i) (render-line i)]
    [(indent? i) (render-indent i)]
    [(div? i) (render-div i)]
    [else (render-p (p i))]))

(define (render-p p-value)
  (define v (p-v p-value))
  (define format-mode (p-format-mode p-value))
  (cond
    [(eq? format-mode 'd) (format "~a" v)]
    [(eq? format-mode 'w) (format "~s" v)]
    [(eq? format-mode 'p) (format "~v" v)]
    [else (format "~v" v)]))

(define (render-line l-value)
  (define content (line-content l-value))
  (string-append (render content) "\n"))

(define (render-indent i)
  (define amount (indent-amount i))
  (define content (indent-content i))
  (define whitespace-str (make-string amount #\space))
  (string-append whitespace-str (render content)))

(define (render-div d-value)
  (define content (div-content d-value))
  (render content))
  

(require 'pieces)