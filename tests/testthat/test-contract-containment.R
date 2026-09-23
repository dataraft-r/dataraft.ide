test_that("contract paths cannot escape a trusted root", {
  root <- withr::local_tempdir()
  outside <- withr::local_tempfile(fileext = ".yaml")
  writeLines("id: private", outside)
  context <- ide_context(contract_root = root)
  expect_error(read_odcs(outside, context), class = "dataraft_ide_error")
  expect_error(
    read_odcs(file.path(root, "..", basename(outside)), context),
    class = "dataraft_ide_error"
  )
  sibling <- paste0(root, "-sibling")
  dir.create(sibling)
  withr::defer(unlink(sibling, recursive = TRUE))
  writeLines("id: private", file.path(sibling, "private.yaml"))
  expect_error(
    read_odcs(file.path(sibling, "private.yaml"), context),
    class = "dataraft_ide_error"
  )
  link <- file.path(root, "escape.yaml")
  if (isTRUE(file.symlink(outside, link))) {
    expect_error(read_odcs(link, context), class = "dataraft_ide_error")
  }
})

test_that("encoded requests cannot select their own contract root", {
  expect_error(
    check_request(list(
      version = 1L,
      operation = "validate_contract",
      contract_root = tempdir()
    )),
    class = "dataraft_ide_error"
  )
})
