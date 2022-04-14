

testthat::test_that("Helper Function is reproducible", {
  a <- lCheckEnrl()
  b <- lCheckEnrl()
  id_body  <- identical(body(a[[1]]), body(b[[1]]))
  id_list  <- identical(a[[2]], b[[2]])
  id_class <- identical(class(a), class(b))
  testthat::expect_true(all(id_body, id_list, id_class))
})

testthat::test_that("Helper Function creates valid lCheckEnrl object", {
  testthat::expect_silent(validate_lCheckEnrl(lCheckEnrl()))
})

testthat::test_that("Validator function correctly throws error when module lacks class argument", {
  y <- list(
    fnCheckEnrl = function(x) {x},
    lAddArgs = list()
  )
  testthat::expect_error(validate_lCheckEnrl(y))
})

testthat::test_that("Validator function correctly throws no error when manually correctly specified", {
  y <- new_lCheckEnrl(
    fnCheckEnrl = function(lPltfTrial, lAddArgs) {1},
    lAddArgs = list()
  )
  testthat::expect_silent(validate_lCheckEnrl(y))
})

testthat::test_that("Validator function correctly throws error when function is misspecified", {
  y <- new_lCheckEnrl(
    fnCheckEnrl = function(x) {x},
    lAddArgs = list()
  )
  testthat::expect_error(validate_lCheckEnrl(y))
})

testthat::test_that("Validator function correctly throws error when list is misspecified", {
  y <- new_lCheckEnrl(
    fnCheckEnrl = function(x) {x},
    lAddArgs = vector()
  )
  testthat::expect_error(validate_lCheckEnrl(y))
})

testthat::test_that("Validator function correctly throws error when module is misspecified", {
  y <- 
    structure(
      list(
        fnCheckEnrl = function(x) {x},
        lAddArgs = list(),
        extra = vector()
      ),
      class = "lCheckEnrl"
    )
  testthat::expect_error(validate_lCheckEnrl(y))
})

