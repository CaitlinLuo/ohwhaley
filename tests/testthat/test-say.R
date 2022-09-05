test_that("test that owhaley input works", {
  what <- c("This is a character string")
  say(what)
  expect_gt(length(what), 0)
})
test_that("test that owhaley output works", {
  expect_length(say(), 0) #Returns something with a length of 0
  expect_null(say()) #Returns null
  expect_invisible(say()) #Returns invisibly
  expect_message(say()) #Returns a message
})
