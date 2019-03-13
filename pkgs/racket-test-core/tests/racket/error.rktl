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

(report-errs)