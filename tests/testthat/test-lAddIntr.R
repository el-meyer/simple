

testthat::test_that("Helper Function is reproducible", {
  r <- sample(1:10, size = 1)
  a <- lAddIntr(r)
  b <- lAddIntr(r)
  id_body  <- identical(body(a[[1]]), body(b[[1]]))
  id_list  <- identical(a[[2]], b[[2]])
  id_class <- identical(class(a), class(b))
  testthat::expect_true(all(id_body, id_list, id_class))
})

testthat::test_that("Helper Function creates valid lAddIntr object", {
  testthat::expect_silent(validate_lAddIntr(lAddIntr(sample(1:10, size = 1))))
})

testthat::test_that("Validator function correctly throws error when module lacks class argument", {
  y <- list(
    fnAddIntr = function(x) {x},
    lAddArgs = list()
  )
  testthat::expect_error(validate_lAddIntr(y))
})

testthat::test_that("Validator function correctly throws no error when manually correctly specified", {
  y <- new_lAddIntr(
    fnAddIntr = function(lPltfDsgn, lPltfTrial, lAddArgs) {1},
    lAddArgs = list()
  )
  testthat::expect_silent(validate_lAddIntr(y))
})

testthat::test_that("Validator function correctly throws error when function is misspecified", {
  y <- new_lAddIntr(
    fnAddIntr = function(x) {x},
    lAddArgs = list()
  )
  testthat::expect_error(validate_lAddIntr(y))
})

testthat::test_that("Validator function correctly throws error when list is misspecified", {
  y <- new_lAddIntr(
    fnAddIntr = function(x) {x},
    lAddArgs = vector()
  )
  testthat::expect_error(validate_lAddIntr(y))
})

testthat::test_that("Validator function correctly throws error when module is misspecified", {
  y <- 
    structure(
      list(
        fnAddIntr = function(x) {x},
        lAddArgs = list(),
        extra = vector()
      ),
      class = "lAddIntr"
    )
  testthat::expect_error(validate_lAddIntr(y))
})


testthat::test_that("Helper Function correctly throws an error if no scalar is specified", {
  testthat::expect_error(validate_lAddIntr(lAddIntr(list(1))))
})

testthat::test_that("Helper Function correctly throws an error if no integer is specified", {
  testthat::expect_error(validate_lAddIntr(lAddIntr(1.2)))
})
