test_that('ODCS import and sample checks stay bounded and do not export rows', {
  skip_if_not_installed('dataraft.adapters')
  path <- withr::local_tempfile(fileext = '.yaml')
  contract <- dataraft.core::dr_contract(
    'orders',
    columns = c(amount = 'numeric')
  )
  dataraft.adapters::dr_contract_odcs(contract, path)
  expect_identical(ide_validate_contract(path, ide_context(read_roots = dirname(path)))$id, 'orders')
  e <- new.env(parent = emptyenv())
  e$orders <- data.frame(amount = c(12345, 67890))
  quality <- ide_sample_quality(
    'binding:orders',
    path,
    ide_context(e, read_roots = dirname(path)),
    row_limit = 1
  )
  expect_gt(length(quality$items), 0)
  expect_false(grepl(
    '12345|67890',
    jsonlite::toJSON(quality, auto_unbox = TRUE, null = 'null')
  ))
  expect_error(
    ide_sample_quality(
      'binding:orders',
      path,
      ide_context(e, read_roots = dirname(path)),
      row_limit = 1001
    ),
    class = 'dataraft_ide_error'
  )
  writeLines('!!expr stop("secret")', path)
  expect_error(ide_validate_contract(path, ide_context(read_roots = dirname(path))), class = "error")
})

test_that('ODCS named expression evidence counts the explicit bounded prefix', {
  skip_if_not_installed('dataraft.adapters')
  path <- withr::local_tempfile(fileext = '.yaml')
  contract <- dataraft.core::dr_contract(
    'orders',
    columns = c(amount = 'numeric'),
    rules = list(dataraft.core::dr_quality_rule(
      ~ amount >= 0,
      name = 'nonnegative'
    ))
  )
  dataraft.adapters::dr_contract_odcs(contract, path)
  e <- new.env(parent = emptyenv())
  e$delivery <- data.frame(amount = c(-10, 20, -30))
  prefix <- ide_sample_quality('binding:delivery', path, ide_context(e, read_roots = dirname(path)), 2L)
  full <- ide_sample_quality('binding:delivery', path, ide_context(e, read_roots = dirname(path)), 3L)
  select <- function(x) {
    Filter(function(rule) identical(rule$rule, 'nonnegative'), x$items)[[1]]
  }
  expect_identical(select(prefix)$status, 'failed')
  expect_equal(select(prefix)$n_failed, 1)
  expect_equal(select(prefix)$n_total, 2)
  expect_equal(select(full)$n_failed, 2)
  expect_equal(select(full)$n_total, 3)
})

test_that("contract paths are contained before any parser runs", {
  root <- withr::local_tempdir()
  outside <- withr::local_tempdir()
  context <- ide_context(read_roots = root)
  inside <- file.path(root, "contract.yaml")
  secret <- file.path(outside, "secret.yaml")
  writeLines("id: inside", inside)
  writeLines("id: private", secret)
  expect_identical(contract_location(inside, context),
    normalizePath(inside, winslash = "/", mustWork = TRUE))
  expect_error(read_odcs(secret, context), class = "dataraft_ide_error")
  expect_error(contract_location(file.path(root, "..", basename(outside),
    "secret.yaml"), context), class = "dataraft_ide_error")
  sibling <- paste0(root, "-sibling")
  dir.create(sibling)
  withr::defer(unlink(sibling, recursive = TRUE))
  writeLines("id: sibling", file.path(sibling, "contract.yaml"))
  expect_error(contract_location(file.path(sibling, "contract.yaml"), context),
    class = "dataraft_ide_error")
  expect_error(contract_location(inside, ide_context(read_roots = character())),
    class = "dataraft_ide_error")
})

test_that("contract roots are captured and symlink escapes are rejected", {
  skip_on_os("windows") # Creation of links requires optional Windows privileges.
  root <- withr::local_tempdir()
  outside <- withr::local_tempdir()
  context <- ide_context(read_roots = root)
  secret <- file.path(outside, "secret.yaml")
  writeLines("id: secret", secret)
  expect_true(file.symlink(secret, file.path(root, "linked.yaml")))
  expect_error(contract_location(file.path(root, "linked.yaml"), context),
    class = "dataraft_ide_error")
  expect_true(file.symlink(outside, file.path(root, "escape")))
  expect_error(contract_location(file.path(root, "escape", "secret.yaml"), context),
    class = "dataraft_ide_error")
  # A replaced canonical root must not silently authorize its replacement.
  unlink(root, recursive = TRUE)
  expect_true(file.symlink(outside, root))
  withr::defer(unlink(root))
  expect_error(check_context(context), class = "dataraft_ide_error")
})

test_that("encoded requests cannot authorize an outside contract", {
  root <- withr::local_tempdir()
  outside <- withr::local_tempdir()
  secret <- file.path(outside, "secret.yaml")
  writeLines("id: PRIVATE_CONTRACT", secret)
  workspace <- new.env(parent = emptyenv())
  workspace$rows <- data.frame(id = 1L)
  context <- ide_context(workspace, response_root = root, read_roots = root)
  for (operation in c("validate_contract", "sample_quality")) {
    request <- list(version = 1L, operation = operation, request_id = operation,
      response_path = file.path(root, paste0(operation, ".json")),
      file_path = secret, handle = "binding:rows")
    wire <- gsub("[[:space:]]", "", jsonlite::base64_enc(charToRaw(as.character(
      jsonlite::toJSON(request, auto_unbox = TRUE)))))
    result <- ide_request(wire, context)
    expect_identical(result$error$code, "unsafe_path")
    expect_false(grepl("PRIVATE_CONTRACT|secret.yaml", paste(
      readLines(request$response_path, warn = FALSE), collapse = "")))
    request$response_path <- file.path(root, paste0(operation, "-root.json"))
    request$read_roots <- outside
    wire <- gsub("[[:space:]]", "", jsonlite::base64_enc(charToRaw(as.character(
      jsonlite::toJSON(request, auto_unbox = TRUE)))))
    expect_identical(ide_request(wire, context)$error$code, "invalid_request")
  }
})
