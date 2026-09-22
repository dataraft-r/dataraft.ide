test_that('disjoint lake lineage is bounded with complete edge endpoints', {
  rows <- data.frame(from_id = paste0('source_', seq_len(500)),
                     to_id = paste0('target_', seq_len(500)),
                     relation = 'depends_on')
  local_mocked_bindings(selected_rows = function(...) rows)
  for (limit in c(1L, 3L, 500L)) {
    graph <- ide_lineage(limit = limit)
    ids <- vapply(graph$nodes, `[[`, '', 'id')
    expect_lte(length(graph$nodes), limit)
    expect_lte(length(graph$edges), limit)
    expect_true(graph$truncated)
    expect_true(all(vapply(graph$edges, function(edge) {
      edge$from %in% ids && edge$to %in% ids
    }, logical(1))))
  }
})

test_that('workspace lineage bounds products and sources together', {
  workspace <- new.env(parent = emptyenv())
  for (i in seq_len(501)) {
    workspace[[sprintf('product%03d', i)]] <- dataraft.core::dr_product(
      paste0('product_', i), data.frame(id = i)
    )
  }
  graph <- ide_lineage(ide_context(workspace), limit = 500)
  ids <- vapply(graph$nodes, `[[`, '', 'id')
  expect_length(graph$nodes, 500)
  expect_lte(length(graph$edges), 500)
  expect_true(graph$truncated)
  expect_true(all(vapply(graph$edges, function(edge) {
    edge$from %in% ids && edge$to %in% ids
  }, logical(1))))
})

test_that('lineage preserves upstream truncation even when IDs collapse', {
  workspace <- new.env(parent = emptyenv())
  product <- dataraft.core::dr_product('same_product', data.frame(id = 1))
  workspace$first <- product
  workspace$second <- product
  workspace$third <- product
  graph <- ide_lineage(ide_context(workspace), limit = 2)
  expect_lte(length(graph$nodes), 2)
  expect_true(graph$truncated)
})

test_that('small complete lineage does not report truncation', {
  workspace <- new.env(parent = emptyenv())
  workspace$product <- dataraft.core::dr_product('one', data.frame(id = 1))
  graph <- ide_lineage(ide_context(workspace), limit = 3)
  expect_false(graph$truncated)
  expect_length(graph$nodes, 2)
  expect_length(graph$edges, 1)
})
