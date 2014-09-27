context("wsXXX and wrXXX function Messages")

test_that("wsVal() errors and warnings",{
  ## bad species name
  expect_that(wsVal("Derek"),throws_error())
  ## too many species name
  expect_that(wsVal(c("Bluegill","Yellow Perch")),throws_error())
  ## bad units
  # typed wrong
  expect_that(wsVal("Bluegill",units="inches"),throws_error())
  # don't exist for the species
  expect_that(wsVal("Ruffe",units="English"),throws_error())
  ## reference value does not exist
  expect_that(wsVal("Bluegill",ref=50),throws_error())
})
