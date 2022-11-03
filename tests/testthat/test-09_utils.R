test_that("load_maps()", {

  load_map("bbs_cws")

})


test_that("ext()", {
  expect_equal(ext("hello"), NA_character_)
  expect_equal(ext("hello.txt"), "txt")
  expect_equal(ext("hello.tiff"), "tiff")
})

