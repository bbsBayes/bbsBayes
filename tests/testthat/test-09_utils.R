
test_that("ext()", {
  expect_equal(ext("hello"), NA_character_)
  expect_equal(ext("hello.txt"), "txt")
  expect_equal(ext("hello.tiff"), "tiff")
})

