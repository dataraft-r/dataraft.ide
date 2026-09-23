test_that('clients have one request boundary and an explicit context constructor', {
  expect_setequal(
    getNamespaceExports('dataraft.ide'),
    c('ide_context', 'ide_request')
  )
  expect_error(
    getExportedValue('dataraft.ide', 'ide_products'),
    'not an exported object'
  )
  expect_error(
    getExportedValue('dataraft.ide', 'ide_emit'),
    'not an exported object'
  )
})

test_that('independent channel schemas retain their existing wire discriminators', {
  for (channel in c('metadata', 'diagnostics')) {
    name <- paste0('bridge-', channel, '-v1.json')
    path <- system.file('schema', name, package = 'dataraft.ide')
    expect_true(nzchar(path))
    schema <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    expect_identical(
      schema[['$id']],
      paste0('https://dataraft-r.github.io/dataraft.ide/schema/', name)
    )
    expect_equal(
      schema$properties$contract$const,
      if (channel == 'metadata') 1 else 2
    )
  }
})
