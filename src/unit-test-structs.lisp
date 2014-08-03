(in-package #:simut)

;; Repository of all test cases and test suites
(setf *global-test-suite* (make-hash-table))

(defmacro create-fixture (fixture-name &rest forms)
  "create-fixture: builds a fixture function and assigns to a symbol

   Arguments: fixture-name - name of fixture function
              forms - Lisp forms to execute before or after a test"
  `(defun ,fixture-name ()
     (format t "Executing fixture: ~S ...~%" ',fixture-name)
     ,@forms))

(defmacro create-unit-test (test-name (&key in-suite set-up tear-down) &rest forms)
  "create-unit-test: builds a unit test function and assigns a name test-name

   Arguments: test-name - name of unit test function
              in-suite - test suite containing the unit test
              set-up - fixture to do initialization tasks
              tear-down - fixture to clear up after executing test
              forms - Lisp forms to execute as part of unit test"
  `(progn
     ;; Create test-name unit test function
     ;; Executes set-up if present then all forms and lastly tear-down if present
     (defun ,test-name ()
       (when ',set-up (,set-up))
       (format t "Executing test: ~S ...~%" ',test-name)
       ;; tear-down is executed even if test code fails
       (unwind-protect
            (progn
              ,@forms
              )
         (when ',tear-down (,tear-down))))
     ;; If test suite mentioned put the test case in test suite
     (when ',in-suite
       ;; Check if the suite is already present
       (let ((suite (gethash ',in-suite *global-test-suite*)))
         (when (not suite) ;; New test suite - create
           (setf (gethash ',in-suite *global-test-suite*) (make-hash-table))
           (setf suite (gethash ',in-suite *global-test-suite*)))
         ;; Add test case to test suite
         (setf (gethash ',test-name suite) (symbol-function ',test-name))))))

(defmacro exec-test (unit-test)
  "exec-test: sets up test execution to enable reporting success/failure

   Arguments: unit-test - symbol representing a unit test"
  `(let ((success-p
          (restart-case
              (progn
                (funcall ,unit-test)
                t)
            (continue-next-test () nil))))
     (when success-p (signal 'test-success-condition))))

;; test-success-condition: condition to signal successful test execution
(define-condition test-success-condition (simple-condition) ())

;; test-exec-report: test execution statistics and messages
;; Instance members: report-item - name of a test suite or test case
;;                   success-count - number of test successes
;;                   failure-count - number of test failures
;;                   messages - list of test messages
(defclass test-exec-report ()
  ((report-item :initarg :report-item
                :initform (error "Must provide report item name")
                :reader report-item)
   (success-count :initform 0
                  :reader success-count)
   (failure-count :initform 0
                  :reader failure-count)
   (messages :initform (make-array 128 :adjustable t :fill-pointer 0)
             :reader messages)))
;; Interfaces of test-exec-report
(defgeneric increment-success (test-report &optional count)
  (:documentation "Increments success count"))
(defgeneric increment-failure (test-report &optional count)
  (:documentation "Increments failure count"))
(defgeneric add-message (test-report message)
  (:documentation "Adds a message to the list of messages"))

(defmethod increment-success ((test-report test-exec-report) &optional (count 1))
  (with-slots (success-count) test-report
    (incf success-count count)))
(defmethod increment-failure ((test-report test-exec-report) &optional (count 1))
  (with-slots (failure-count) test-report
    (incf failure-count count)))
(defmethod add-message ((test-report test-exec-report) message)
  (with-slots (messages) test-report
    (vector-push-extend message messages)))

;; Global repository of all test results
(setf *global-test-results* (make-hash-table))

(defun report-test (test-name &optional (suite-name nil) (failure-report-p nil) (message nil))
  "report-test: reports test result for a unit test

   Arguments: test-name - name of the unit test
              suite-name - test suite where unit test belongs
              failure-report-p - success or failure report?
              message - if failure report additional failure message"
  (let ((report-item-name) (test-report) (test-message))
    (if suite-name (setf report-item-name suite-name)
        (setf report-item-name test-name))
    (setf test-report (gethash report-item-name *global-test-results*))
    (when (not test-report)
      (setf (gethash report-item-name *global-test-results*) (make-instance 'test-exec-report :report-item report-item-name))
      (setf test-report (gethash report-item-name *global-test-results*)))
    (if failure-report-p (progn
                           (increment-failure test-report)
                           (setf test-message (format nil "Test ~S: failed~%~A" test-name message)))
        (progn
          (increment-success test-report)
          (setf test-message (format nil "Test ~S: passed~%" test-name))))
    (add-message test-report test-message)))

;; print-result: prints a single test result entry
(flet ((print-result (report-item total-success total-failure)
         (let ((test-result (gethash report-item *global-test-results*)))
           (with-slots (success-count failure-count messages) test-result
             (incf total-success success-count)
             (incf total-failure failure-count)
             (format t "Test success: ~D and failure: ~D~%~%" success-count failure-count)
             (loop for message across messages do (format t "~A~%" message)))
           (values total-success total-failure))))

  (defun print-result-for-test (test-name &key suite-name (summary-p t))
    " print-result-for-test: prints results after running the test

      Arguments: test-name - name of the unit test
                 suite-name - name of the test suite (optional)
                 summary-p - should display test summary? default true
      Note: reset *global-test-results* once printing is complete"
    (let ((total-success 0) (total-failure 0) (report-item))
      (if suite-name (setf report-item suite-name)
          (setf report-item test-name))
      (format t "Result for unit test: ~A~%" test-name)
      (multiple-value-setq (total-success total-failure)
        (print-result report-item total-success total-failure))
      (when summary-p
        (format t "========== Summary Result ==========~%")
        (format t "Total test success: ~D~%" total-success)
        (format t "Total test failure: ~D~%" total-failure)))
    ;; reset test result repository
    (clrhash *global-test-results*))

  (defun print-result-for-suite (suite-name &key (summary-p t))
    "print-result-for-suite: prints results after running the test suite

     Arguments: suite-name - name of the test suite
                summary-p - should display test summary? default true
     Note: reset *global-test-results* once printing is complete"
    (let ((total-success 0) (total-failure 0) (report-item))
      (setf report-item suite-name)
      (format t "Result for test case: ~A~%" suite-name)
      (multiple-value-setq (total-success total-failure)
        (print-result report-item total-success total-failure))
      (when summary-p
        (format t "========== Summary Result ==========~%")
        (format t "Total test success: ~D~%" total-success)
        (format t "Total test failure: ~D~%" total-failure)))
    ;; reset test result repository
    (clrhash *global-test-results*))

  (defun print-result-for-all (&key (summary-p t))
    "print-result-for-all: prints results after running all the tests

     Arguments: summary-p - should display test summary? default true
     Note: reset *global-test-results* once printing is complete"
    (let ((total-success 0) (total-failure 0) (report-item))
      (format t "===== Test Result Breakups =====~%")
      (maphash #'(lambda (k v) (setf report-item k)
                         (format t "Result for test item: ~A~%" k)
                         (multiple-value-setq (total-success total-failure)
                           (print-result report-item total-success total-failure)))
               *global-test-results*)
      (when summary-p
        (format t "========== Summary Result ==========~%")
        (format t "Total test success: ~D~%" total-success)
        (format t "Total test failure: ~D~%" total-failure)))
    ;; reset test result repository
    (clrhash *global-test-results*))
  )

(defun continue-next-test (c current-test &optional current-suite)
  "Restart for continue-next-test"
  (report-test current-test current-suite t (princ-to-string c))
  (let ((restart (find-restart 'continue-next-test)))
    (when restart (invoke-restart restart))))

(defmacro with-success-failure-handlers ((test-name &key suite-name) &rest forms)
  "with-success-failure-handlers: short-cut for handler definitions

   Arguments: test-name - unit test
              suite-name - test suite
              forms - Lisp forms that actually executes the unit test"
  `(handler-bind ((error #'(lambda (c) (continue-next-test c ,test-name ,suite-name)))
                  (test-success-condition #'(lambda (c) (report-test ,test-name ,suite-name))))
     ,@forms))

(defun run-test (test-name &key in-suite set-up tear-down (print-result-p t))
  "run-test: runs a unit test

   Arguments: test-name - name of the unit test
              in-suite - name of the test suite
              set-up - set up task needed before running the test
              tear-down - clean up needed after running the test
              print-result-p - need to print test result? default true"
  ;; if in-suite supplied, check test-name belongs to in-suite
  (when in-suite
    (let ((suite (gethash in-suite *global-test-suite*)))
      (cond
        ((not suite) (error (format nil "Test suite ~A not found" in-suite)))
        ((not (gethash test-name suite))
         (error (format nil "Unit test ~A not found in test suite ~A" test-name in-suite))))))
  (when set-up (funcall set-up))
  ;; set up success/failure handlers and execute test
  (with-success-failure-handlers (test-name :suite-name in-suite) (exec-test test-name))
  (when tear-down (funcall tear-down))
  (when print-result-p (print-result-for-test test-name :suite-name in-suite)))

(defun run-suite (suite-name &key set-up tear-down (print-result-p t))
  "run-suite: runs all unit tests in a test suite

   Arguments: suite-name - name of test suite
              set-up - set up task needed before running all tests
              tear-down - clean up task after all tests are complete
              print-result-p - need to print test result? default true"
  ;; check if test suite is defined
  (let ((suite (gethash suite-name *global-test-suite*)) (test-name))
    (when (not suite) (error (format nil "Test suite ~A not found" suite-name)))
    (when set-up (funcall set-up))
    ;; set up success/failure handlers and execute tests
    (with-success-failure-handlers (test-name :suite-name suite-name)
      (maphash #'(lambda (test-nm test-fn) (setf test-name test-nm) (exec-test test-fn))
               suite))
    (when tear-down (funcall tear-down))
    (when print-result-p (print-result-for-suite suite-name))))

(defun run-all (&key set-up tear-down (print-result-p t))
  "run-all: runs all unit tests in all test suites

   Arguments: set-up - set up task needed before running all tests
              tear-down - clean up task after all tests are complete
              print-result-p - need to print test result? default true"
  (let ((suite-name) (test-name))
    (when set-up (funcall set-up))
    ;; set up success/failure handlers and execute tests
    (with-success-failure-handlers (test-name :suite-name suite-name)
      (maphash #'(lambda (suite-nm suite)
                   (setf suite-name suite-nm)
                   (maphash #'(lambda (test-nm test-fn)
                                (setf test-name test-nm)
                                (exec-test test-fn))
                            suite))
               *global-test-suite*))
    (when tear-down (funcall tear-down))
    (when print-result-p (print-result-for-all))))