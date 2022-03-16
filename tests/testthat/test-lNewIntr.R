

testthat::test_that("Helper Function is reproducible", {
  r <- sample(1:10, size = 1)
  a <- lNewIntr(r)
  b <- lNewIntr(r)
  id_body  <- identical(body(a[[1]]), body(b[[1]]))
  id_list  <- identical(a[[2]], b[[2]])
  id_class <- identical(class(a), class(b))
  testthat::expect_true(all(id_body, id_list, id_class))
})

testthat::test_that("Helper Function creates valid lRecrPars object", {
  testthat::expect_silent(validate_lNewIntr(lNewIntr(sample(1:10, size = 1))))
})

testthat::test_that("Validator function correctly throws error when module lacks class argument", {
  y <- list(
    fnNewIntr = function(x) {x},
    lAddArgs = list()
  )
  testthat::expect_error(validate_lNewIntr(y))
})

testthat::test_that("Validator function correctly throws no error when manually correctly specified", {
  y <- new_lNewIntr(
    fnNewIntr = function(lPltfTrial, lAddArgs) {1},
    lAddArgs = list()
  )
  testthat::expect_silent(validate_lNewIntr(y))
})

testthat::test_that("Validator function correctly throws error when function is misspecified", {
  y <- new_lNewIntr(
    fnNewIntr = function(x) {x},
    lAddArgs = list()
  )
  testthat::expect_error(validate_lNewIntr(y))
})

testthat::test_that("Validator function correctly throws error when list is misspecified", {
  y <- new_lNewIntr(
    fnNewIntr = function(x) {x},
    lAddArgs = vector()
  )
  testthat::expect_error(validate_lNewIntr(y))
})

testthat::test_that("Validator function correctly throws error when module is misspecified", {
  y <- 
    structure(
      list(
        fnNewIntr = function(x) {x},
        lAddArgs = list(),
        extra = vector()
      ),
      class = "lNewIntr"
    )
  testthat::expect_error(validate_lNewIntr(y))
})


testthat::test_that("Plot function correctly throws error when length of supplied snapshot variables differs", {
  testthat::expect_error(plot(lNewIntr(sample(1:10, size = 1)), dActvIntr = c(1, 2, 3)))
})

testthat::test_that("Plot function works without errors in default scenario", {
  testthat::expect_silent(plot(lNewIntr(sample(1:10, size = 1))))
})

testthat::test_that("Helper Function correctly throws an error if no scalar is specified", {
  testthat::expect_error(validate_lRecrPars(lNewIntr(list(1))))
})

testthat::test_that("Plot function correctly throws error when ISA in trial time mismatch", {
  testthat::expect_error(plot(lNewIntr(5), cIntrTime = "random", dIntrTimeParam = 4))
})

testthat::test_that("Plot function correctly throws error when ISA in trial time mismatch (2)", {
  testthat::expect_error(plot(lNewIntr(5), cIntrTime = "fixed", dIntrTimeParam = 0.3))
})

testthat::test_that("Plot function correctly does not throw error when ISA in trial time correctly specified", {
  testthat::expect_silent(plot(lNewIntr(5), cIntrTime = "fixed", dIntrTimeParam = 6))
})

testthat::test_that("Plot function correctly does not throw error when ISA in trial time correctly specified", {
  testthat::expect_silent(plot(lNewIntr(5), cIntrTime = "random", dIntrTimeParam = 0.4))
})
