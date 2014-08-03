(in-package #:test-simut)

;; Test for create-fixture
(create-fixture initialize-a
                (setf a (make-hash-table))
                (setf (gethash 'name a) "santanu"))

;; Call setup function initialize-a to set variable a
(initialize-a)
(assert (string= (gethash 'name a) "santanu"))

;; Test for create-unit-test
;; 1. no suite, no set up, no tear down
(create-unit-test my-test () (setf t-sum (+ 1 2 3 4 5 6 7 8 9 10)))
;; Execute my-test and verify
(my-test)
(assert (= t-sum (+ 1 2 3 4 5 6 7 8 9 10)))

;; 2. no suite, set up and tear down
(create-fixture f1 (setf aa 20))
(create-fixture f2 (setf aa 30))
(create-unit-test my-test2 (:set-up f1 :tear-down f2) (setf t-aa aa))
;; Execute my-test2 and verify
(my-test2)
(assert (= t-aa 20))
(assert (= aa 30))

;; 3. suite, set up and tear down
(setf *global-test-suite* (make-hash-table))
(assert (null (gethash 'my-suite *global-test-suite*)))
(create-unit-test my-test3 (:in-suite my-suite :set-up f1 :tear-down f2) (setf z-aa aa))
;; Execute my-test3 and verify
(my-test3)
(assert (= z-aa 20))
(assert (= aa 30))
(assert (gethash 'my-suite *global-test-suite*))
;; Check that my-test3 is contained by my-suite
(let ((suite (gethash 'my-suite *global-test-suite*)))
  (setf test-fn1 (gethash 'my-test3 suite))
  (assert (eq test-fn1 (symbol-function 'my-test3))))

;; Test for run-test
;; 1. unknown suite name
(let ((mssg))
  (handler-case (run-test 'any-test :in-suite 'any-suite)
    (error (c) (setf mssg (princ-to-string c))))
  (assert (string= "Test suite ANY-SUITE not found" mssg)))

;; 2. unknown test name
(let ((mssg))
  (handler-case (run-test 'any-test :in-suite 'my-suite)
    (error (c) (setf mssg (princ-to-string c))))
  (assert (string= "Unit test ANY-TEST not found in test suite MY-SUITE" mssg)))

;; 3. unit test invoking error
(clrhash *global-test-suite*)
(create-unit-test unit-test-1 () (error "throwing error forcefully"))
(let ((run-output (make-array '(0) :element-type 'base-char
                              :fill-pointer 0 :adjustable t))
      (exp-output) (test-count-str))
  ;; capture test output to a string
  (with-output-to-string (*standard-output* run-output) (run-test 'unit-test-1))
  (setf exp-output (format nil "Test UNIT-TEST-1: failed~%throwing error forcefully"))
  (setf test-count-str (format nil "Test success: 0 and failure: 1"))
  ;; search if expected output is contained by run-output
  (assert (search exp-output run-output))
  (assert (search test-count-str run-output)))

;; 4. successful unit test with run-time set up and tear down
(clrhash *global-test-suite*)
(create-fixture f1 (setf a 20))
(create-fixture f2 (setf a 30))
(create-unit-test unit-test-2 () (assert (= 20 a)))
(let ((run-output (make-array '(0) :element-type 'base-char
                              :fill-pointer 0 :adjustable t))
      (exp-output) (test-count-str))
  ;; capture test output to a string
  (with-output-to-string (*standard-output* run-output)
    (run-test 'unit-test-2 :set-up 'f1 :tear-down 'f2))
  (setf exp-output (format nil "Test UNIT-TEST-2: passed"))
  (setf test-count-str (format nil "Test success: 1 and failure: 0"))
  ;; search if expected output is contained by run-output
  (assert (search exp-output run-output))
  (assert (search test-count-str run-output))
  (assert (= 30 a)))

;; Test for run-suite
;; 1. unknown suite name
(let ((mssg))
  (handler-case (run-suite 'any-suite)
    (error (c) (setf mssg (princ-to-string c))))
  (assert (string= "Test suite ANY-SUITE not found" mssg)))
;; 2. test suite containing two unit tests
(create-fixture tf1 (setf x 6))
(create-fixture tf2 (setf x 0))
(create-unit-test unit-test-3 (:in-suite test-suite-1) (assert (= 16 y)))
(create-unit-test unit-test-4 (:in-suite test-suite-1
                                         :set-up tf1 :tear-down tf2)
                  (setf x (+ y x)) (assert (= 16 x)))
(create-fixture tf3 (setf y 10))
(let ((run-output (make-array '(0) :element-type 'base-char
                              :fill-pointer 0 :adjustable t))
      (exp-output) (test-count-str))
  ;; capture test output to a string
  (with-output-to-string (*standard-output* run-output)
    (run-suite 'test-suite-1 :set-up 'tf3))
  ;; search if expected output is contained by run-output
  (setf exp-output (format nil "Test UNIT-TEST-4: passed"))
  (assert (search exp-output run-output))
  (setf exp-output (format nil "Test UNIT-TEST-3: failed"))
  (assert (search exp-output run-output))
  (setf test-count-str (format nil "Test success: 1 and failure: 1"))
  (assert (search test-count-str run-output))
  (assert (= 0 x))
  (assert (= 10 y)))

;; Test for run-all
(create-fixture tf4 (let ((x 1)) (defun seq-gen (increment) (incf x increment))))
(create-unit-test unit-test-5 (:in-suite test-suite-2 :set-up tf4)
                  (assert (= 11 (seq-gen y))))
(let ((run-output (make-array '(0) :element-type 'base-char
                              :fill-pointer 0 :adjustable t))
      (exp-output) (test-count-str))
  ;; capture test output to a string
  (with-output-to-string (*standard-output* run-output)
    (run-all :set-up 'tf3))
  ;; search if expected output is contained by run-output
  (setf exp-output (format nil "Result for test item: TEST-SUITE-1"))
  (assert (search exp-output run-output))
  (setf exp-output (format nil "Test UNIT-TEST-4: passed"))
  (assert (search exp-output run-output))
  (setf exp-output (format nil "Test UNIT-TEST-3: failed"))
  (assert (search exp-output run-output))
  (setf test-count-str (format nil "Test success: 1 and failure: 1"))
  (assert (search test-count-str run-output))
  (setf exp-output (format nil "Result for test item: TEST-SUITE-2"))
  (assert (search exp-output run-output))
  (setf exp-output (format nil "Test UNIT-TEST-5: passed"))
  (assert (search exp-output run-output))
  (setf test-count-str (format nil "Test success: 1 and failure: 0"))
  (assert (search test-count-str run-output))
  (setf test-count-str (format nil "Total test success: 2"))
  (assert (search test-count-str run-output))
  (setf test-count-str (format nil "Total test failure: 1"))
  (assert (search test-count-str run-output))
  (assert (= 0 x))
  (assert (= 10 y)))