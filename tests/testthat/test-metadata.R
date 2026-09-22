
test_that("lake incidents retain unvalidated evidence", {
  testthat::local_mocked_bindings(
    ide_quality = function(...) list(items = list(
      list(rule = "schema", status = "unvalidated"),
      list(rule = "nonempty", status = "passed")
    ), truncated = FALSE)
  )
  result <- ide_incidents(ide_context(), selection = "lake")
  expect_length(result$items, 1L)
  expect_identical(result$items[[1L]]$status, "unvalidated")
  expect_false(result$truncated)
})
