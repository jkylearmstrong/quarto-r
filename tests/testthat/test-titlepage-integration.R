test_that("titlepage_pdf renders a pdf when Quarto is available", {
  skip_if_no_quarto()
  skip_if_not_installed("withr")
  skip_if_not_installed("xfun")

  tmp <- withr::local_tempdir("test-titlepage-")
  # prepare files; allow the helper to install the template into tmp
  prep <- prepare_titlepage_dir(tmp, title = "Integration Test", author = "Tester", quiet = TRUE, no_prompt = TRUE)
  # render using helper that asserts output exists
  out <- .render(prep$input_qmd, output_file = "test-titlepage.pdf", .quiet = TRUE)
  expect_true(file.exists(out))
})
