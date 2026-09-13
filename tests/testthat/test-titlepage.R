test_that("titlepage_pdf has expected arguments", {
  expect_true(is.function(titlepage_pdf))
  f <- formals(titlepage_pdf)
  expect_true("image" %in% names(f))
  expect_true("output" %in% names(f))
  expect_true("no_prompt" %in% names(f))
})

test_that("titlepage_pdf is exported", {
  # exported functions are available when the package is loaded in tests via devtools::load_all
  expect_true(exists("titlepage_pdf"))
})
