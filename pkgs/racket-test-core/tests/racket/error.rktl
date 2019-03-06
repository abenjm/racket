(load-relative "loadtest.rktl")

(Section 'error)

;; ----- raise-argument-error forms ----- ;;

(err/rt-test (raise-argument-error 'form-1 "expected?" 'other) 
             exn:fail:contract? 
             #rx"form-1: contract violation\n  expected: expected\\?\n  given: 'other")

(err/rt-test (raise-argument-error 'form-2a "expected?" 0 'other) 
             exn:fail:contract? 
             #rx"form-2a: contract violation\n  expected: expected\\?\n  given: 'other")

(err/rt-test (raise-argument-error 'form-2b "expected?" 2 'other1 'other2 'other3) 
             exn:fail:contract? 
             #rx"form-2b: contract violation\n  expected: expected\\?\n  given: 'other3\n  argument position: 3rd\n  other arguments...:\n   'other1\n   'other2")

;; Check expected exceptions when the forms are misused.

(err/rt-test (raise-argument-error 'form-1a "expected?") 
             exn:fail:contract:arity?
             #rx"raise-argument-error: arity mismatch")

(err/rt-test (raise-argument-error "form-1b" "expected?" 'other)
             exn:fail:contract?
             #rx"raise-argument-error: contract violation\n  expected: symbol\\?")

(err/rt-test (raise-argument-error 'form-1c 'expected? 'other)
             exn:fail:contract?
             #rx"raise-argument-error: contract violation\n  expected: string\\?")

;; ----- raise-type-error forms ----- ;;

(err/rt-test (raise-type-error 'form-1 "expected?" 'other)
             exn:fail:contract? 
             #rx"form-1: expected argument of type <expected\\?>; given: 'other")

(err/rt-test (raise-type-error 'form-2a "expected?" 0 'other)
             exn:fail:contract?
             #rx"form-2a: expects argument of type <expected\\?>; given: 'other")

(err/rt-test (raise-type-error 'form-2b "expected?" 1 'other1 'other2 'other3)
             exn:fail:contract?
             #rx"form-2b: expects type <expected\\?> as 2nd argument, given: 'other2; other arguments were: 'other1 'other3")

;; Check expected exceptions when the forms are misused.

(err/rt-test (raise-type-error 'form-1a "expected?") 
             exn:fail:contract:arity?
             #rx"raise-type-error: arity mismatch")

(err/rt-test (raise-type-error "form-1b" "expected?" 'other) 
             exn:fail:contract?
             #rx"raise-type-error: contract violation\n  expected: symbol\\?")

(err/rt-test (raise-type-error 'form-1c 'expected? 'other) 
             exn:fail:contract?
             #rx"raise-type-error: contract violation\n  expected: string\\?")

(err/rt-test (raise-type-error "form-2a" "expected?" 0 'other) 
             exn:fail:contract?
             #rx"raise-type-error: contract violation\n  expected: symbol\\?")

(err/rt-test (raise-type-error 'form-2b 'expected? 0 'other)
             exn:fail:contract? 
             #rx"raise-type-error: contract violation\n  expected: string\\?")

(err/rt-test (raise-type-error 'form-2c "expected?" 'NaN 'other) 
             exn:fail:contract? 
             #rx"raise-type-error: contract violation\n  expected: exact-nonnegative-integer\\?")

(err/rt-test (raise-type-error 'form-2d "expected?" 1 'other) 
             exn:fail:contract?
             #rx"raise-type-error: position index >= provided argument count")

(err/rt-test (raise-type-error "form-2e" "expected?" 1 'other1 'other2) 
             exn:fail:contract? 
             #rx"raise-type-error: contract violation\n  expected: symbol\\?")

(err/rt-test (raise-type-error 'form-2f 'expected? 1 'other1 'other2) 
             exn:fail:contract? 
             #rx"raise-type-error: contract violation\n  expected: string\\?")

(err/rt-test (raise-type-error 'form-2g "expected?" 'NaN 'other1 'other2) 
             exn:fail:contract? 
             #rx"raise-type-error: contract violation\n  expected: exact-nonnegative-integer\\?")

(err/rt-test (raise-type-error 'form-2h "expected?" 3 'other1 'other2) 
             exn:fail:contract? 
             #rx"raise-type-error: position index >= provided argument count")

(report-errs)
