test_that("clearing container works", {
  .injectr.container$foo <- c(1, 2, 3)

  expect_length(ls(envir = .injectr.container), 1)

  clear()

  expect_length(ls(envir = .injectr.container), 0)

})

test_that("registering object in container with specified name works", {
  clear()
  foo <- c(1, 2, 3)

  register(foo, "bar")

  # "bar" should not be in function environment
  expect_false(exists("bar", where = environment()))

  # But it should be in the container environment
  expect_true(exists("bar", where = .injectr.container))

  expect_equal(.injectr.container$bar$object, foo)

})

test_that("registering object in container with inferred name works", {
  clear()
  foo <- c(1, 2, 3)

  register(foo)

  expect_true(exists("foo", where = environment()))
  expect_true(exists("foo", where = .injectr.container))

  expect_equal(.injectr.container$foo$object, foo)
})


test_that("injecting object into function environment with inferred name works", {
  clear()

  .injectr.container$foo <- .object_wrapper(c(1, 2, 3))

  # foo should not be in function environment
  expect_false(exists("foo", where = environment()))

  inject("foo")

  # now it should be in the function environment
  expect_true(exists("foo", where = environment()))

  expect_equal(foo, .injectr.container$foo$object)

})

test_that("injecting object into function environment with specified name works", {
  clear()
  .injectr.container$foo <- .object_wrapper(c(1, 2, 3))

  # foo should not be in function environment
  expect_false(exists("foo", where = environment()))

  inject("foo", target = bar)

  # now it should be in the function environment
  expect_true(exists("bar", where = environment()))

  expect_equal(bar, .injectr.container$foo$object)

})

test_that("injecting object that doesn't exist has no effect when option set to not error", {
  clear()
  expect_false(exists("foo", where = environment()))

  inject("foo")

  expect_false(exists("foo", where = environment()))
})

test_that("injecting object that doesn't exist errors when option set to error", {
  clear()

  options("injectr.error_on_no_match" = TRUE)

  expect_error(inject("foo"))

  options("injectr.error_on_no_match" = NULL)
})

test_that("registering a function works", {
  clear()

  foo <- function() {
  }

  register(foo)

  expect_true(exists("foo", where = .injectr.container))

  expect_equal(foo, .injectr.container$foo$object)
})

test_that("registering a generator function works", {
  clear()

  foo <- function() {
  }

  register(foo, generator = TRUE)

  print(.injectr.container$foo)

  expect_true(exists("foo", where = .injectr.container))
  
  expect_equal(foo, .injectr.container$foo$object)
  expect_true(.injectr.container$foo$generator)
})



test_that("registering an anonymous generator function without a name fails", {

})

test_that("injecting a generator function returns the results of the function", {

})