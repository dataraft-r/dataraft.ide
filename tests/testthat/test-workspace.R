test_that('discovery never evaluates active, delayed or executable definitions', {
  e <- new.env(parent = emptyenv())
  makeActiveBinding('active', function() stop('must not execute'), e)
  delayedAssign('lazy', stop('must not force'), assign.env = e)
  e$orders <- dataraft.core::dr_product('orders', function() {
    stop('must not read')
  })
  e$table <- data.frame(
    id = 'sensitive-id',
    owner = 'Jane Person',
    description = 'claim details'
  )
  e$flow <- dataraft.core::dr_workflow() |>
    dataraft.core::dr_add_product(e$orders)
  before <- e$orders
  x <- ide_products(ide_context(e))
  expect_setequal(
    vapply(x$items, `[[`, '', 'id'),
    c('orders', 'orders', 'table')
  )
  expect_length(x$items, 3)
  wire <- as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = 'null'))
  expect_false(grepl('sensitive-id|Jane Person|claim details|must not', wire))
  detail <- ide_product('binding:table', ide_context(e))
  expect_null(detail$contract)
  expect_length(detail$sources, 0)
  expect_identical(e$orders, before)
  expect_error(
    ide_product('binding:active', ide_context(e)),
    class = 'dataraft_ide_error'
  )
  expect_error(
    ide_product('binding:lazy', ide_context(e)),
    class = 'dataraft_ide_error'
  )
})

test_that('explicit names are exact and selections are bounded', {
  e <- new.env(parent = emptyenv())
  e[['orders; stop("bad")']] <- data.frame(x = 1)
  e$second <- data.frame(x = 2)
  context <- ide_context(e, objects = 'orders; stop("bad")')
  expect_length(ide_products(context)$items, 1)
  expect_identical(
    ide_product('binding:orders; stop("bad")', context)$id,
    'orders; stop("bad")'
  )
  expect_error(
    ide_product('binding:second', context),
    class = 'dataraft_ide_error'
  )
  expect_true(ide_products(ide_context(e), limit = 1)$truncated)
  expect_error(ide_products(context, limit = 501), class = 'dataraft_ide_error')
  expect_error(
    ide_context(e, objects = rep('second', 501)),
    class = 'dataraft_ide_error'
  )
})

test_that('workspace schemas, quality and lineage contain metadata only', {
  e <- new.env(parent = emptyenv())
  e$table <- data.frame(amount = c(-12345, 12345))
  e$orders <- dataraft.core::dr_product('orders', e$table) |>
    dataraft.core::dr_add_quality(~ amount >= 0)
  e$result <- dataraft.core::dr_run(
    write = FALSE,
    stop_on_failure = FALSE,
    e$orders
  )
  context <- ide_context(e)
  expect_identical(
    ide_profile('binding:table', context)$columns[[1]]$name,
    'amount'
  )
  expect_gt(length(ide_quality(context)$items), 0)
  expect_gt(length(ide_incidents(context)$items), 0)
  expect_length(ide_runs(context)$items, 1)
  expect_gt(length(ide_lineage(context)$nodes), 0)
  expect_length(ide_releases(context)$items, 0)
  expect_length(ide_freshness(context)$items, 0)
  expect_length(ide_reports(context)$items, 0)
  wire <- jsonlite::toJSON(
    ide_quality(context),
    auto_unbox = TRUE,
    null = 'null'
  )
  expect_false(grepl('12345|amount >=', wire))
})

test_that('trial retains results and leaves original product unchanged', {
  e <- new.env(parent = emptyenv())
  e$flow <- dataraft.core::dr_workflow() |>
    dataraft.core::dr_add_product(dataraft.core::dr_product(
      'orders',
      data.frame(id = 1:3)
    ))
  before <- serialize(e$flow, NULL)
  withr::defer({
    .ide_state$results <- list()
    .ide_state$serial <- 0L
  })
  result <- run_action('trial', 'binding:flow', ide_context(e), 1L)
  expect_identical(result$status, 'completed')
  expect_identical(serialize(e$flow, NULL), before)
  expect_identical(ide_product(result$handle, ide_context(e))$kind, 'result')
  expect_identical(
    nrow(dataraft.core::dr_collect(.ide_state$results[[result$handle]])),
    3L
  )
})

test_that('viewer bounds retained data before materialization', {
  e <- new.env(parent = emptyenv())
  e$table <- data.frame(id = 1:20)
  e$result <- dataraft.core::dr_run(
    write = FALSE,
    stop_on_failure = FALSE,
    dataraft.core::dr_product(
      'orders',
      e$table
    )
  )
  seen <- NULL
  testthat::local_mocked_bindings(
    View = function(x, title) {
      seen <<- x
    },
    .package = 'utils'
  )
  expect_identical(
    run_action('view', 'binding:result', ide_context(e), 3L)$status,
    'viewed'
  )
  expect_equal(nrow(seen), 3)
  e$result$data <- NULL
  expect_false(ide_product('binding:result', ide_context(e))$can_view)
  expect_error(
    run_action('view', 'binding:result', ide_context(e), 3L),
    class = 'dataraft_ide_error'
  )
})

test_that('truncation stays visible when a single retained run exceeds the bound', {
  e <- new.env(parent = emptyenv())
  e$result <- dataraft.core::dr_run(
    write = FALSE,
    stop_on_failure = FALSE,
    dataraft.core::dr_product(
      'orders',
      data.frame(id = 1)
    )
  )
  e$result$quality <- data.frame(
    rule = c('one', 'two'),
    status = c('passed', 'failed')
  )
  expect_true(
    ide_quality(ide_context(e), handle = 'binding:result', limit = 1)$truncated
  )
})
