;;;; package.lisp

(in-package #:cl-user)

(defpackage #:simut
  (:use #:cl)
  (:export #:*global-test-suite*
           #:*global-test-results*
           #:create-fixture
           #:create-unit-test
           #:run-test
           #:run-suite
           #:run-all))

(defpackage #:test-simut
  (:use #:cl #:simut)
  (:shadowing-import-from #:simut))