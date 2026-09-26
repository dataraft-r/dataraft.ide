test_that("README first example runs", {
  # Check the snippet readers copy from GitHub.
  readme <- test_path("..", "..", "README.md")
  if (!file.exists(readme)) {
    root <- Sys.getenv("GITHUB_WORKSPACE")
    readme <- file.path(root, "family", "dataraft.ide", "README.md")
    if (!file.exists(readme)) readme <- file.path(root, "README.md")
  }
  if (!file.exists(readme)) skip("README source is unavailable here")
  lines <- readLines(readme, warn = FALSE)
  heading <- match("## Start in R", lines)
  expect_false(is.na(heading))
  opening <- which(lines == "```r" & seq_along(lines) > heading)[1L]
  closing <- which(lines == "```" & seq_along(lines) > opening)[1L]
  expect_false(is.na(opening))
  expect_false(is.na(closing))
  code <- paste(lines[seq.int(opening + 1L, closing - 1L)], collapse = "\n")
  expect_no_error(eval(parse(text = code), envir = new.env(parent = globalenv())))
})
