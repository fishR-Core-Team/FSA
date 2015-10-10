This is a resubmission from 8-Oct-15 to address problems with tests on sparc-sun-solaris2.10 (32-bit) using R 3.2.2 patched.  In addition to the changes shown below, I examined the other test files for similar issues.

# Changes
* Changed all tests in `test_AgeLengthKey` that had used `==` to use `expect_equivalent()` (from `testthat`) which uses `all.equal()` with `check.attributes=FALSE`.
* Changed all tests in `test_PSD` that had used `==` to use `expect_equivalent()` (from `testthat`) which uses `all.equal()` with `check.attributes=FALSE`.

