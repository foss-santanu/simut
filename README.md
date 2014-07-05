SIMUT - (SIM)PLE (U)NIT (T)EST FRAMEWORK
========================================

**Current Version**: 0.1

1. **A bit of history**

I am newcomer to the *Land of Lisp*. To get a feel of the language I decided to code my [Algorithm to build Regular Grammar from Regular Expression](https://gist.github.com/santanuchakrabarti/7c4c74c7b25a9235fe42) in Lisp. As someone from the Java world I started looking for good unit testing framework to develop my test cases. I found *lisp-unit2* very near to something I am acquainted with in Java world. So I started writing my unit tests in lisp-unit2 and after I made some progress I did `ql:quickload` in MKCL REPL. Shattering all my expectations I couldn't even compile my package as lisp-unit2 has dependencies on other packages some of which has problem with the version of MKCL I was using.

So I decided to have my own unit testing frame - something that doesn't have any dependency on other packages and that only uses *features of Common Lisp* without relying on any implementation specific extensions.

2. **Features implemented**

+ *Fixtures* and *Unit Tests* can be created.
+ While creating a unit test *Test Suite* that contains this test can be specified.
+ Can *run* a *single unit test*, a *test suite* or *all test suites*.
+ *Reporting* the test results after each run. **Warning**: After reporting test results are reset.

3. **Future improvements**

+ Test suites can be created *independently* from unit test creation.
+ Unit tests *can be added later* to a test suite.
+ *Default test suite* - test cases not added to any named test suite belong to this one.
+ Hence *run-all* can run all test cases *whether or not* added to a test suite. Currently test cases must belong to a named test suite to have them run by run-all.

4. **How to use this framework?**

Currently there is no API documentation or usage manual, but the test cases that I wrote to test the code can be taken as a guide to the usage of this framework.

5. **Any help** in any form, be it *code review* or *feature suggestions* or *bug reporting* or anything else, is heartily welcomed.

Developing this unit test framework is a much enjoyed learning experience whereby I put my footings into the Land of Lisp. :-)