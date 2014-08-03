
## Rudimentary Documentation for UNIT-TEST-STRUCTS.LISP

*Containing Directory /host/santanu/programming/Lisp/simut/src/*

### API Documentation


#### CREATE-FIXTURE (fixture-name &rest forms)                          [MACRO]
>      
>      create-fixture: builds a fixture function and assigns to a symbol
>      Arguments: fixture-name - name of fixture function
>                   forms - Lisp forms to execute before or after a test

---

#### CREATE-UNIT-TEST (test-name (&key in-suite set-up tear-down) &rest forms)  [MACRO]
>      
>      create-unit-test: builds a unit test function and assigns a name
>      test-name 
>      Arguments: test-name - name of unit test function
>                   in-suite - test suite containing the unit test
>                   set-up - fixture to do initialization tasks
>                   tear-down - fixture to clear up after executing test
>                   forms - Lisp forms to execute as part of unit test

---

#### EXEC-TEST (unit-test)                                              [MACRO]
>      
>      exec-test: sets up test execution to enable reporting success/failure
>      Arguments: unit-test - symbol representing a unit test

---

#### TEST-SUCCESS-CONDITION ()                                      [CONDITION]

---

#### TEST-EXEC-REPORT                                                   [CLASS]
>      
>      Superclasses
>      None.
>      Initialization Arguments
>      The :report-item argument is a 
>      Readers
>      report-item	Generic Function
>      	test-exec-report
>      Returns 
>      success-count	Generic Function
>      	test-exec-report
>      Returns 
>      failure-count	Generic Function
>      	test-exec-report
>      Returns 
>      messages	Generic Function
>      	test-exec-report
>      Returns 
>      Writers

---

#### INCREMENT-SUCCESS (test-report &optional count)         [GENERIC FUNCTION]
>      
>      Increments success count

---

#### INCREMENT-FAILURE (test-report &optional count)         [GENERIC FUNCTION]
>      
>      Increments failure count

---

#### ADD-MESSAGE (test-report message)                       [GENERIC FUNCTION]
>      
>      Adds a message to the list of messages

---

#### INCREMENT-SUCCESS ((test-report test-exec-report) &optional (count 1))  [METHOD]

---

#### INCREMENT-FAILURE ((test-report test-exec-report) &optional (count 1))  [METHOD]

---

#### ADD-MESSAGE ((test-report test-exec-report) message)              [METHOD]

---

#### REPORT-TEST (test-name &optional (suite-name nil) (failure-report-p nil) (message nil))  [FUNCTION]
>      
>      report-test: reports test result for a unit test
>      Arguments: test-name - name of the unit test
>                   suite-name - test suite where unit test belongs
>                   failure-report-p - success or failure report?
>                   message - if failure report additional failure message

---

#### PRINT-RESULT-FOR-TEST (test-name &key suite-name (summary-p t))  [FUNCTION]
>      
>      print-result-for-test: prints results after running the test
>           Arguments: test-name - name of the unit test
>                      suite-name - name of the test suite (optional)
>                      summary-p - should display test summary? default
>      true Note: reset *global-test-results* once printing is complete

---

#### PRINT-RESULT-FOR-SUITE (suite-name &key (summary-p t))          [FUNCTION]
>      
>      print-result-for-suite: prints results after running the test suite
>          Arguments: suite-name - name of the test suite
>                     summary-p - should display test summary? default true
>          Note: reset *global-test-results* once printing is complete

---

#### PRINT-RESULT-FOR-ALL (&key (summary-p t))                       [FUNCTION]
>      
>      print-result-for-all: prints results after running all the tests
>          Arguments: summary-p - should display test summary? default true
>          Note: reset *global-test-results* once printing is complete

---

#### CONTINUE-NEXT-TEST (c current-test &optional current-suite)     [FUNCTION]
>      
>      Restart for continue-next-test

---

#### WITH-SUCCESS-FAILURE-HANDLERS ((test-name &key suite-name) &rest forms)  [MACRO]
>      
>      with-success-failure-handlers: short-cut for handler definitions
>      Arguments: test-name - unit test
>                   suite-name - test suite
>                   forms - Lisp forms that actually executes the unit test

---

#### RUN-TEST (test-name &key in-suite set-up tear-down (print-result-p t))  [FUNCTION]
>      
>      run-test: runs a unit test
>      Arguments: test-name - name of the unit test
>                   in-suite - name of the test suite
>                   set-up - set up task needed before running the test
>                   tear-down - clean up needed after running the test
>                   print-result-p - need to print test result? default
>      true 

---

#### RUN-SUITE (suite-name &key set-up tear-down (print-result-p t))  [FUNCTION]
>      
>      run-suite: runs all unit tests in a test suite
>      Arguments: suite-name - name of test suite
>                   set-up - set up task needed before running all tests
>                   tear-down - clean up task after all tests are complete
>                   print-result-p - need to print test result? default
>      true 

---

#### RUN-ALL (&key set-up tear-down (print-result-p t))              [FUNCTION]
>      
>      run-all: runs all unit tests in all test suites
>      Arguments: set-up - set up task needed before running all tests
>                   tear-down - clean up task after all tests are complete
>                   print-result-p - need to print test result? default
>      true 

---


## Dependency Documentations

### File Dependencies

"/host/santanu/programming/Lisp/simut/src/unit-test-structs.lisp" --> ("/host/santanu/programming/Lisp/simut/src/unit-test-structs.lisp")

### Call Dependencies


#### Function/Macro Calls


SIMUT::PRINT-RESULT-FOR-ALL is referenced by SIMUT:RUN-ALL.

SIMUT::PRINT-RESULT-FOR-SUITE is referenced by SIMUT:RUN-SUITE.

SIMUT::TEST-FN is referenced by SIMUT:RUN-ALL SIMUT:RUN-SUITE.

SIMUT::PRINT-RESULT-FOR-TEST is referenced by SIMUT:RUN-TEST.

SIMUT::TEAR-DOWN is referenced by SIMUT:RUN-ALL SIMUT:RUN-SUITE SIMUT:RUN-TEST.

SIMUT::TEST-NAME is referenced by SIMUT:RUN-TEST.

SIMUT::EXEC-TEST is referenced by SIMUT:RUN-ALL SIMUT:RUN-SUITE SIMUT:RUN-TEST.

SIMUT::CONTINUE-NEXT-TEST is referenced by SIMUT:RUN-ALL SIMUT:RUN-SUITE SIMUT:RUN-TEST.

SIMUT::WITH-SUCCESS-FAILURE-HANDLERS is referenced by SIMUT:RUN-ALL SIMUT:RUN-SUITE SIMUT:RUN-TEST.

SIMUT::SET-UP is referenced by SIMUT:RUN-ALL SIMUT:RUN-SUITE SIMUT:RUN-TEST.

SIMUT::REPORT-TEST is referenced by SIMUT:RUN-ALL SIMUT:RUN-SUITE SIMUT:RUN-TEST SIMUT::CONTINUE-NEXT-TEST.

:UNNAMED-LAMBDA is referenced by SIMUT:RUN-ALL SIMUT:RUN-SUITE SIMUT:RUN-TEST SIMUT::PRINT-RESULT-FOR-ALL.

SIMUT::PRINT-RESULT is referenced by SIMUT::PRINT-RESULT-FOR-ALL SIMUT::PRINT-RESULT-FOR-SUITE SIMUT::PRINT-RESULT-FOR-TEST.

SIMUT::ADD-MESSAGE is referenced by SIMUT::REPORT-TEST.

SIMUT::INCREMENT-SUCCESS is referenced by SIMUT::REPORT-TEST.

SIMUT::INCREMENT-FAILURE is referenced by SIMUT::REPORT-TEST.

SIMUT::TEST-EXEC-REPORT is referenced by SIMUT::REPORT-TEST.

#### Variable Readers


SIMUT::CONTINUE-NEXT-TEST is referenced by SIMUT:RUN-ALL SIMUT:RUN-SUITE SIMUT:RUN-TEST.

SIMUT::TEST-SUCCESS-CONDITION is referenced by SIMUT:RUN-ALL SIMUT:RUN-SUITE SIMUT:RUN-TEST.

SIMUT:\*GLOBAL-TEST-SUITE\* is referenced by SIMUT:RUN-ALL SIMUT:RUN-SUITE SIMUT:RUN-TEST.

SIMUT::ACROSS is referenced by SIMUT::PRINT-RESULT.

SIMUT::FOR is referenced by SIMUT::PRINT-RESULT.

SIMUT:\*GLOBAL-TEST-RESULTS\* is referenced by SIMUT::PRINT-RESULT-FOR-ALL SIMUT::PRINT-RESULT-FOR-SUITE SIMUT::PRINT-RESULT-FOR-TEST SIMUT::PRINT-RESULT SIMUT::REPORT-TEST.

SIMUT::MESSAGES is referenced by SIMUT::PRINT-RESULT SIMUT::ADD-MESSAGE.

SIMUT::MESSAGE is referenced by SIMUT::PRINT-RESULT SIMUT::ADD-MESSAGE.

SIMUT::FAILURE-COUNT is referenced by SIMUT::PRINT-RESULT SIMUT::INCREMENT-FAILURE.

SIMUT::SUCCESS-COUNT is referenced by SIMUT::PRINT-RESULT SIMUT::INCREMENT-SUCCESS.

SIMUT::TEST-REPORT is referenced by SIMUT::ADD-MESSAGE SIMUT::INCREMENT-FAILURE SIMUT::INCREMENT-SUCCESS.

#### Variable Setters


SIMUT::SUITE-NM is referenced by SIMUT:RUN-ALL.

SIMUT::TEST-FN is referenced by SIMUT:RUN-ALL SIMUT:RUN-SUITE.

SIMUT::TEST-NM is referenced by SIMUT:RUN-ALL SIMUT:RUN-SUITE.

SIMUT::SUCCESS-P is referenced by SIMUT:RUN-ALL SIMUT:RUN-SUITE SIMUT:RUN-TEST.

SIMUT::SUITE is referenced by SIMUT:RUN-ALL SIMUT:RUN-SUITE SIMUT:RUN-TEST.

SIMUT::PRINT-RESULT-P is referenced by SIMUT:RUN-ALL SIMUT:RUN-SUITE SIMUT:RUN-TEST.

SIMUT::TEAR-DOWN is referenced by SIMUT:RUN-ALL SIMUT:RUN-SUITE SIMUT:RUN-TEST.

SIMUT::SET-UP is referenced by SIMUT:RUN-ALL SIMUT:RUN-SUITE SIMUT:RUN-TEST.

SIMUT::IN-SUITE is referenced by SIMUT:RUN-TEST.

SIMUT::CURRENT-SUITE is referenced by SIMUT::CONTINUE-NEXT-TEST.

SIMUT::CURRENT-TEST is referenced by SIMUT::CONTINUE-NEXT-TEST.

SIMUT::C is referenced by SIMUT:RUN-ALL SIMUT:RUN-SUITE SIMUT:RUN-TEST SIMUT::CONTINUE-NEXT-TEST.

SIMUT::V is referenced by SIMUT::PRINT-RESULT-FOR-ALL.

SIMUT::K is referenced by SIMUT::PRINT-RESULT-FOR-ALL.

SIMUT::SUMMARY-P is referenced by SIMUT::PRINT-RESULT-FOR-ALL SIMUT::PRINT-RESULT-FOR-SUITE SIMUT::PRINT-RESULT-FOR-TEST.

SIMUT::TEST-RESULT is referenced by SIMUT::PRINT-RESULT.

SIMUT::TOTAL-FAILURE is referenced by SIMUT::PRINT-RESULT-FOR-ALL SIMUT::PRINT-RESULT-FOR-SUITE SIMUT::PRINT-RESULT-FOR-TEST SIMUT::PRINT-RESULT.

SIMUT::TOTAL-SUCCESS is referenced by SIMUT::PRINT-RESULT-FOR-ALL SIMUT::PRINT-RESULT-FOR-SUITE SIMUT::PRINT-RESULT-FOR-TEST SIMUT::PRINT-RESULT.

SIMUT::REPORT-ITEM is referenced by SIMUT::PRINT-RESULT-FOR-ALL SIMUT::PRINT-RESULT-FOR-SUITE SIMUT::PRINT-RESULT-FOR-TEST SIMUT::PRINT-RESULT.

SIMUT::TEST-MESSAGE is referenced by SIMUT::REPORT-TEST.

SIMUT::REPORT-ITEM-NAME is referenced by SIMUT::REPORT-TEST.

SIMUT::FAILURE-REPORT-P is referenced by SIMUT::REPORT-TEST.

SIMUT::SUITE-NAME is referenced by SIMUT:RUN-ALL SIMUT:RUN-SUITE SIMUT::PRINT-RESULT-FOR-SUITE SIMUT::PRINT-RESULT-FOR-TEST SIMUT::REPORT-TEST.

SIMUT::MESSAGE is referenced by SIMUT::REPORT-TEST SIMUT::ADD-MESSAGE.

SIMUT::TEST-REPORT is referenced by SIMUT::REPORT-TEST SIMUT::ADD-MESSAGE SIMUT::INCREMENT-FAILURE SIMUT::INCREMENT-SUCCESS.

SIMUT::UNIT-TEST is referenced by SIMUT::EXEC-TEST.

SIMUT::TEST-NAME is referenced by SIMUT:RUN-ALL SIMUT:RUN-SUITE SIMUT:RUN-TEST SIMUT::PRINT-RESULT-FOR-TEST SIMUT::REPORT-TEST SIMUT:CREATE-UNIT-TEST.

SIMUT::FORMS is referenced by SIMUT:CREATE-FIXTURE.

SIMUT::FIXTURE-NAME is referenced by SIMUT:CREATE-FIXTURE.
