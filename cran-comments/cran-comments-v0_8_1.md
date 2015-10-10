This is a resubmission from 8-Oct-15 to address problems with tests on sparc-sun-solaris2.10 (32-bit) using R 3.2.2 patched.

# Notes
* Bumped version to 0.8.1.
* Changed all tests in `test_AgeLengthKey` that had used `==` to use `expect_equivalent()` (from `testthat`) which uses `all.equal()` with `check.attributes=FALSE`.
* Changed all tests in `test_PSD` that had used `==` to use `expect_equivalent()` (from `testthat`) which uses `all.equal()` with `check.attributes=FALSE`.
* Examined other test files for similar issues to the previous two notes.
* Added `col2rgbt()`, `compIntercepts()`, and `compSlopes()` functions (and tests).
* Retested on win-builder using R-release and R-devel.
