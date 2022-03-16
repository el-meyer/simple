

testthat::test_that("Helper Function is reproducible", {
  a <- lSnap()
  b <- lSnap()
  id_body  <- identical(body(a[[1]]), body(b[[1]]))
  id_list  <- identical(a[[2]], b[[2]])
  id_class <- identical(class(a), class(b))
  testthat::expect_true(all(id_body, id_list, id_class))
})

testthat::test_that("Helper Function creates valid lSnap object", {
  testthat::expect_silent(validate_lSnap(lSnap()))
})

testthat::test_that("Validator function correctly throws error when module lacks class argument", {
  y <- list(
    fnSnap = function(x) {x},
    lAddArgs = list()
  )
  testthat::expect_error(validate_lSnap(y))
})

testthat::test_that("Validator function correctly throws no error when manually correctly specified", {
  y <- new_lSnap(
    fnSnap = function(lPltfTrial, lAddArgs) {1},
    lAddArgs = list()
  )
  testthat::expect_silent(validate_lSnap(y))
})

testthat::test_that("Validator function correctly throws error when function is misspecified", {
  y <- new_lSnap(
    fnSnap = function(x) {x},
    lAddArgs = list()
  )
  testthat::expect_error(validate_lSnap(y))
})

testthat::test_that("Validator function correctly throws error when list is misspecified", {
  y <- new_lSnap(
    fnSnap = function(x) {x},
    lAddArgs = vector()
  )
  testthat::expect_error(validate_lSnap(y))
})

testthat::test_that("Validator function correctly throws error when module is misspecified", {
  y <- 
    structure(
      list(
        fnSnap = function(x) {x},
        lAddArgs = list(),
        extra = vector()
      ),
      class = "lSnap"
    )
  testthat::expect_error(validate_lSnap(y))
})
