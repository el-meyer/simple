

testthat::test_that("Helper Function is reproducible (nWeeks)", {
  r <- sample(1:10, size = 1)
  a <- lStopRule(nWeeks = r)
  b <- lStopRule(nWeeks = r)
  id_body  <- identical(body(a[[1]]), body(b[[1]]))
  id_list  <- identical(a[[2]], b[[2]])
  id_class <- identical(class(a), class(b))
  testthat::expect_true(all(id_body, id_list, id_class))
})

testthat::test_that("Helper Function is reproducible (bNoActive)", {
  a <- lStopRule(bNoActive = TRUE)
  b <- lStopRule(bNoActive = TRUE)
  id_body  <- identical(body(a[[1]]), body(b[[1]]))
  id_list  <- identical(a[[2]], b[[2]])
  id_class <- identical(class(a), class(b))
  testthat::expect_true(all(id_body, id_list, id_class))
})

testthat::test_that("Helper Function creates valid lStopRule object (nWeeks)", {
  testthat::expect_silent(validate_lStopRule(lStopRule(nWeeks = sample(1:10, size = 1))))
})

testthat::test_that("Helper Function creates valid lStopRule object (bNoActive)", {
  testthat::expect_silent(validate_lStopRule(lStopRule(bNoActive = TRUE)))
})

testthat::test_that("Validator function correctly throws error when module lacks class argument", {
  y <- list(
    fnStopRule = function(x) {x},
    lAddArgs = list()
  )
  testthat::expect_error(validate_lStopRule(y))
})

testthat::test_that("Validator function correctly throws no error when manually correctly specified", {
  y <- new_lStopRule(
    fnStopRule = function(lPltfTrial, lAddArgs) {1},
    lAddArgs = list()
  )
  testthat::expect_silent(validate_lStopRule(y))
})

testthat::test_that("Validator function correctly throws error when function is misspecified", {
  y <- new_lStopRule(
    fnStopRule = function(x) {x},
    lAddArgs = list()
  )
  testthat::expect_error(validate_lStopRule(y))
})

testthat::test_that("Validator function correctly throws error when list is misspecified", {
  y <- new_lStopRule(
    fnStopRule = function(x) {x},
    lAddArgs = vector()
  )
  testthat::expect_error(validate_lStopRule(y))
})

testthat::test_that("Validator function correctly throws error when module is misspecified", {
  y <- 
    structure(
      list(
        fnStopRule = function(x) {x},
        lAddArgs = list(),
        extra = vector()
      ),
      class = "lStopRule"
    )
  testthat::expect_error(validate_lStopRule(y))
})

testthat::test_that("Helper Function correctly throws an error if no scalar is specified (nWeeks)", {
  testthat::expect_error(validate_lStopRule(lStopRule(nWeeks = list(1))))
})

testthat::test_that("Helper Function correctly throws an error if no integer is specified (nWeeks)", {
  testthat::expect_error(validate_lStopRule(lStopRule(nWeeks = 1.2)))
})

testthat::test_that("Helper Function correctly throws no error if integer is specified (nWeeks)", {
  testthat::expect_silent(validate_lStopRule(lStopRule(nWeeks = 1)))
})

testthat::test_that("Helper Function correctly throws an error if no TRUE is specified (bNoActive)", {
  testthat::expect_error(validate_lStopRule(lStopRule(bNoActive = list(1))))
})

testthat::test_that("Helper Function correctly throws no error if TRUE is specified (bNoActive)", {
  testthat::expect_silent(validate_lStopRule(lStopRule(bNoActive = TRUE)))
})

testthat::test_that("Helper Function correctly throws an error if nothing specified", {
  testthat::expect_error(validate_lStopRule(lStopRule()))
})
