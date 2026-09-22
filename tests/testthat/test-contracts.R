test_that('ODCS import and sample checks stay bounded and do not export rows', {
  skip_if_not_installed('dataraft.adapters')
  path <- withr::local_tempfile(fileext = '.yaml')
  contract <- dataraft.core::dr_contract(
    'orders',
    columns = c(amount = 'numeric')
  )
  dataraft.adapters::dr_contract_odcs(contract, path)
  expect_identical(ide_validate_contract(path)$id, 'orders')
  e <- new.env(parent = emptyenv())
  e$orders <- data.frame(amount = c(12345, 67890))
  quality <- ide_sample_quality(
    'binding:orders',
    path,
    ide_context(e),
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
      ide_context(e),
      row_limit = 1001
    ),
    class = 'dataraft_ide_error'
  )
  writeLines('!!expr stop("secret")', path)
  expect_error(ide_validate_contract(path), class = "error")
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
  prefix <- ide_sample_quality('binding:delivery', path, ide_context(e), 2L)
  full <- ide_sample_quality('binding:delivery', path, ide_context(e), 3L)
  select <- function(x) {
    Filter(function(rule) identical(rule$rule, 'nonnegative'), x$items)[[1]]
  }
  expect_identical(select(prefix)$status, 'failed')
  expect_equal(select(prefix)$n_failed, 1)
  expect_equal(select(prefix)$n_total, 2)
  expect_equal(select(full)$n_failed, 2)
  expect_equal(select(full)$n_total, 3)
})
