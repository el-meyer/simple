

testthat::test_that("Helper Function is reproducible", {
  r <- runif(1)
  a <- lRecrPars(r)
  b <- lRecrPars(r)
  id_body  <- identical(body(a[[1]]), body(b[[1]]))
  id_list  <- identical(a[[2]], b[[2]])
  id_class <- identical(class(a), class(b))
  testthat::expect_true(all(id_body, id_list, id_class))
})

testthat::test_that("Helper Function creates valid lRecrPars object", {
  testthat::expect_silent(validate_lRecrPars(lRecrPars(runif(1))))
})

testthat::test_that("Validator function correctly throws error when object is misspecified", {
  y <- list(
    fnRecrProc = function(x) {x},
    lAddArgs = list()
  )
  testthat::expect_error(validate_lRecrPars(y))
})



