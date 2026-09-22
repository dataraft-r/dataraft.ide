test_that('published lake definitions expose contracts and actual run status', {
  skip_if_not_installed('dataraft.lake')
  skip_if_not_installed('duckdb')
  root <- withr::local_tempdir()
  lake <- dataraft.lake::dr_open_lake(
    file.path(root, 'lake'),
    backend = 'duckdb'
  )
  withr::defer(dataraft.lake::dr_close_lake(lake))
  product <- dataraft.core::dr_product(
    'orders',
    data.frame(id = 1:2, amount = c(10, 20))
  ) |>
    dataraft.core::dr_add_contract(c(id = 'integer', amount = 'numeric')) |>
    dataraft.core::dr_add_quality(~ amount > 0) |>
    dataraft.core::dr_set_target(lake)
  result <- dataraft.core::dr_run(product)
  context <- ide_context(new.env(parent = emptyenv()), lake = lake)
  products <- ide_products(context, 'lake')
  expect_length(products$items, 1)
  expect_identical(products$items[[1]]$status, 'published')
  expect_true(products$items[[1]]$can_view)
  handle <- products$items[[1]]$handle
  detail <- ide_product(handle, context)
  expect_setequal(
    vapply(detail$contract$columns, `[[`, '', 'name'),
    c('id', 'amount')
  )
  expect_gt(detail$source_count, 0)
  expect_gt(detail$rule_count, 0)
  expect_identical(
    ide_releases(context, handle = handle)$items[[1]]$release_id,
    result$release_id
  )
  expect_type(
    ide_releases(context, handle = handle)$items[[1]]$release_order,
    'character'
  )
  expect_gt(length(ide_runs(context, handle = handle)$items), 0)
  expect_gt(length(ide_quality(context, handle = handle)$items), 0)
  expect_gt(length(ide_lineage(context, handle = handle)$edges), 0)
})

test_that('release order stays exact beyond double integer precision', {
  skip_if_not_installed('bit64')
  rows <- data.frame(release_order = bit64::as.integer64('9007199254740993'))
  expect_identical(
    rows_metadata(rows, 'release_order')$items[[1]]$release_order,
    '9007199254740993'
  )
})
