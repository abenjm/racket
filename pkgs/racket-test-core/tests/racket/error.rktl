(load-relative "loadtest.rktl")

(Section 'error)

;; ----- raise-argument-error forms ----- ;;

(err/rt-test (raise-argument-error 'form-1a "expected?" 'other) 
             exn:fail:contract? 
             #rx"form-1a: contract violation\n  expected: expected\\?\n  given: 'other")

(err/rt-test (raise-argument-error 'form-1b #:more-info "informative sentence explaining more about the argument" "expected?" 'other) 
             exn:fail:contract? 
             #rx"form-1b: contract violation;\n informative sentence explaining more about the argument\n  expected: expected\\?\n  given: 'other")

; make sure line break characters inside #:more-info string are properly handled
(err/rt-test (raise-argument-error 'form-1c #:more-info "informative sentence explaining more about the argument\nanother sentence with even more details\none more sentence" "expected?" 'other) 
             exn:fail:contract? 
             #rx"form-1c: contract violation;\n informative sentence explaining more about the argument\n another sentence with even more details\n one more sentence\n  expected: expected\\?\n  given: 'other")

; long detail on same line as label in a field should automatically move to next line and be indented
(err/rt-test (raise-argument-error 'form-1d "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" 'other) 
             exn:fail:contract? 
             #rx"form-1d: contract violation\n  expected:\n   aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n  given: 'other")

(err/rt-test (raise-argument-error 'form-1e "expected?" 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa) 
             exn:fail:contract? 
             #rx"form-1e: contract violation\n  expected: expected\\?\n  given:\n   'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

(err/rt-test (raise-argument-error 'form-2a "expected?" 0 'other) 
             exn:fail:contract? 
             #rx"form-2a: contract violation\n  expected: expected\\?\n  given: 'other")

(err/rt-test (raise-argument-error 'form-2b "expected?" 0 'other1 'other2 'other3) 
             exn:fail:contract? 
             #rx"form-2b: contract violation\n  expected: expected\\?\n  given: 'other1\n  argument position: 1st\n  other arguments...:\n   'other2\n   'other3")

(err/rt-test (raise-argument-error 'form-2c "expected?" 1 'other1 'other2 'other3) 
             exn:fail:contract? 
             #rx"form-2c: contract violation\n  expected: expected\\?\n  given: 'other2\n  argument position: 2nd\n  other arguments...:\n   'other1\n   'other3")

(err/rt-test (raise-argument-error 'form-2d "expected?" 2 'other1 'other2 'other3) 
             exn:fail:contract? 
             #rx"form-2d: contract violation\n  expected: expected\\?\n  given: 'other3\n  argument position: 3rd\n  other arguments...:\n   'other1\n   'other2")

; make sure ordinal position names are correct for 11, 12, 13
(err/rt-test (raise-argument-error 'form-2e "expected?" 10 'other1 'other2 'other3  'other4 'other5 'other6 'other7 'other8 'other9 'other10 'other11 'other12 'other13 'other14) 
             exn:fail:contract? 
             #rx"form-2e: contract violation\n  expected: expected\\?\n  given: 'other11\n  argument position: 11th\n")

(err/rt-test (raise-argument-error 'form-2f "expected?" 11 'other1 'other2 'other3  'other4 'other5 'other6 'other7 'other8 'other9 'other10 'other11 'other12 'other13 'other14) 
             exn:fail:contract? 
             #rx"form-2f: contract violation\n  expected: expected\\?\n  given: 'other12\n  argument position: 12th\n")

(err/rt-test (raise-argument-error 'form-2g "expected?" 12 'other1 'other2 'other3  'other4 'other5 'other6 'other7 'other8 'other9 'other10 'other11 'other12 'other13 'other14) 
             exn:fail:contract? 
             #rx"form-2g: contract violation\n  expected: expected\\?\n  given: 'other13\n  argument position: 13th\n")

(err/rt-test (raise-argument-error 'form-2h #:more-info "informative sentence explaining more about the argument" "expected?" 0 'other) 
             exn:fail:contract? 
             #rx"form-2h: contract violation;\n informative sentence explaining more about the argument\n  expected: expected\\?\n  given: 'other")

(err/rt-test (raise-argument-error 'form-2i "expected?" 2 'other1 'other2 'other3 #:more-info "informative sentence explaining more about the argument") 
             exn:fail:contract? 
             #rx"form-2i: contract violation;\n informative sentence explaining more about the argument\n  expected: expected\\?\n  given: 'other3\n  argument position: 3rd\n  other arguments...:\n   'other1\n   'other2")

; make sure line break characters inside #:more-info string are properly handled
(err/rt-test (raise-argument-error 'form-2j "expected?" 2 'other1 'other2 'other3 #:more-info "informative sentence explaining more about the argument\nanother sentence with even more details\none more sentence") 
             exn:fail:contract? 
             #rx"form-2j: contract violation;\n informative sentence explaining more about the argument\n another sentence with even more details\n one more sentence\n  expected: expected\\?\n  given: 'other3\n  argument position: 3rd\n  other arguments...:\n   'other1\n   'other2")

(err/rt-test (raise-argument-error 'form-2k "expected?" 3 2 2 1 2 1 2) 
             exn:fail:contract? 
             #rx"form-2k: contract violation\n  expected: expected\\?\n  given: 2\n  argument position: 4th\n  other arguments...:\n   2\n   2\n   1\n   1\n   2")


; Check expected exceptions when raise-argument-error is misused.

(err/rt-test (raise-argument-error 'form-1a "expected?") 
             exn:fail:contract:arity?
             #rx"raise-argument-error: arity mismatch")

(err/rt-test (raise-argument-error "form-1b" "expected?" 'other)
             exn:fail:contract?
             #rx"raise-argument-error: contract violation\n  expected: symbol\\?")

(err/rt-test (raise-argument-error 'form-1c 'expected? 'other)
             exn:fail:contract?
             #rx"raise-argument-error: contract violation\n  expected: string\\?")

(err/rt-test (raise-argument-error 'form-1d "expected?" 'other #:more-info 'not-string)
             exn:fail:contract?
             #rx"raise-argument-error: contract violation\n  expected: string\\?\n  given: 'not-string\n  keyword: #:more-info\n  arguments...:\n   'form-1d\n   \"expected\\?\"\n   'other")

(err/rt-test (raise-argument-error "form-2a" "expected?" 0 'other) 
             exn:fail:contract? 
             #rx"raise-argument-error: contract violation\n  expected: symbol\\?")

(err/rt-test (raise-argument-error 'form-2b 'expected? 0 'other1 'other2 'other3) 
             exn:fail:contract? 
             #rx"raise-argument-error: contract violation\n  expected: string\\?")

(err/rt-test (raise-argument-error 'form-2c "expected?" 'NaN 'other) 
             exn:fail:contract? 
             #rx"raise-argument-error: contract violation\n  expected: exact-nonnegative-integer\\?\n  given: 'NaN")

(err/rt-test (raise-argument-error 'form-2d "expected?" 5 'other1 'other2 'other3) 
             exn:fail:contract? 
             #rx"raise-argument-error: position index >= provided argument count\n  position index: 5\n  provided argument count: 3")

(err/rt-test (raise-argument-error 'form-2e #:more-info 345 "expected?" 0 'other1 'other2 'other3) 
             exn:fail:contract? 
             #rx"raise-argument-error: contract violation\n  expected: string\\?\n  given: 345\n  keyword: #:more-info\n  arguments...:\n")

; make sure keyword argument is properly reported in error output when raise-argument-error is misused
(err/rt-test (raise-argument-error 'form-2f 'expected? 0 'other1 #:more-info "string") 
             exn:fail:contract? 
             #rx"raise-argument-error: contract violation\n  expected: string\\?\n  given: 'expected\\?\n  argument position: 2nd\n  other arguments...:\n   'form-2f\n   0\n   'other1\n  keyword arguments...:\n   #:more-info \"string\"")

(require racket/private/error-reporting)

(define err-rpt (error-report (absent) (absent) (absent) (absent) (absent)))

(report-errs)