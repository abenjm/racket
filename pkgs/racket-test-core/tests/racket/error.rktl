(load-relative "loadtest.rktl")

(Section 'error)

;; ----- raise-argument-error variants ----- ;;

;; (raise-argument-error 'variant-1 "value expected" 'other-value)
(let ()
    (define expected-message "variant-1: contract violation\n  expected: value expected\n  given: 'other-value")
    (define actual-message (with-handlers ([exn? (lambda (e) (exn-message e))]) 
				(raise-argument-error 'variant-1 "value-expected" 'other-value)))

    (test #t 'raise-argument-error/variant-1 (string=? expected-message actual-message)))

;; ----- raise-type-error variants ----- ;;

(let ()
    (define expected-message "variant-1: expected argument of type <expected?>; given: 'other")
    (define actual-message (with-handlers ([exn? (lambda (e) (exn-message e))]) 
				(raise-type-error 'variant-1 "expected?" 'other)))

    (test #t 'raise-type-error/variant-1 (string=? expected-message actual-message)))

(let ()
    (define expected-message "variant-2a: expects argument of type <expected?>; given: 'other")
    (define actual-message (with-handlers ([exn? (lambda (e) (exn-message e))]) 
				(raise-type-error 'variant-2a "expected?" 0 'other)))

    (test #t 'raise-type-error/variant-2a (string=? expected-message actual-message)))

(let ()
    (define expected-message "variant-2b: expects type <expected?> as 2nd argument, given: 'other2; other arguments were: 'other1 'other3")
    (define actual-message (with-handlers ([exn? (lambda (e) (exn-message e))]) 
				(raise-type-error 'variant-2b "expected?" 1 'other1 'other2 'other3)))

    (test #t 'raise-type-error/variant-2b (string=? expected-message actual-message)))

;; Check expected exceptions when variants are misused.

(test #t 'raise-type-error/variant-1-arity-exn 
	(with-handlers ([exn:fail:contract:arity? (lambda (e) #t)])
        	(raise-type-error 'variant-1 "expected?")))

(test #t 'raise-type-error/variant-1a-contract-exn 
	(with-handlers ([exn:fail:contract? (lambda (e) #t)])
        	(raise-type-error "variant-1a" "expected?" 'other)))

(test #t 'raise-type-error/variant-1b-contract-exn 
	(with-handlers ([exn:fail:contract? (lambda (e) #t)])
        	(raise-type-error 'variant-1b 'expected? 'other)))

(test #t 'raise-type-error/variant-2a-contract-exn 
	(with-handlers ([exn:fail:contract? (lambda (e) #t)])
        	(raise-type-error "variant-2a" "expected?" 0 'other)))

(test #t 'raise-type-error/variant-2b-contract-exn 
	(with-handlers ([exn:fail:contract? (lambda (e) #t)])
        	(raise-type-error 'variant-2b 'expected? 0 'other)))

(test #t 'raise-type-error/variant-2c-contract-exn 
	(with-handlers ([exn:fail:contract? (lambda (e) #t)])
        	(raise-type-error 'variant-2c "expected?" 'NaN 'other)))

(test #t 'raise-type-error/variant-2d-contract-exn 
	(with-handlers ([exn:fail:contract? (lambda (e) #t)])
        	(raise-type-error 'variant-2d "expected?" 1 'other)))

(test #t 'raise-type-error/variant-2e-contract-exn 
	(with-handlers ([exn:fail:contract? (lambda (e) #t)])
        	(raise-type-error "variant-2e" "expected?" 1 'other1 'other2)))

(test #t 'raise-type-error/variant-2f-contract-exn
	(with-handlers ([exn:fail:contract? (lambda (e) #t)])
        	(raise-type-error 'variant-2f 'expected? 1 'other1 'other2)))

(test #t 'raise-type-error/variant-2g-contract-exn
	(with-handlers ([exn:fail:contract? (lambda (e) #t)])
        	(raise-type-error 'variant-2g "expected?" 'NaN 'other1 'other2)))

(test #t 'raise-type-error/variant-2h-contract-exn
	(with-handlers ([exn:fail:contract? (lambda (e) #t)])
        	(raise-type-error 'variant-2h "expected?" 3 'other1 'other2)))

(report-errs)
