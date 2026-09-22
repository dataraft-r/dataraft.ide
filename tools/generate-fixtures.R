# Run from the package directory with dataraft.ide installed or loaded.
workspace <- new.env(parent = emptyenv())
workspace$orders <- dataraft.core::dr_product(
  'orders',
  data.frame(amount = c(-1, 2))
) |>
  dataraft.core::dr_add_quality(~ amount >= 0)
workspace$delivery <- data.frame(amount = c(-1, 2))
workspace$result <- dataraft.core::dr_trial(workspace$orders)
context <- dataraft.ide::ide_context(workspace)
operations <- c(
  'contexts',
  'products',
  'product',
  'lineage',
  'quality',
  'releases',
  'runs',
  'freshness',
  'incidents',
  'reports',
  'trial',
  'profile'
)
for (operation in operations) {
  path <- tempfile(fileext = '.json')
  handle <- if (operation %in% c('product', 'trial')) {
    'binding:orders'
  } else if (operation == 'profile') {
    'binding:delivery'
  } else {
    NULL
  }
  dataraft.ide::ide_emit(
    operation,
    path,
    paste0('fixture-', operation),
    context,
    handle
  )
  file.copy(
    path,
    file.path('inst', 'fixtures', paste0(operation, '.json')),
    overwrite = TRUE
  )
  unlink(path)
}
path <- tempfile(fileext = '.json')
dataraft.ide::ide_emit(
  'product',
  path,
  'fixture-error',
  context,
  'binding:missing'
)
file.copy(path, 'inst/fixtures/error.json', overwrite = TRUE)
unlink(path)
if (requireNamespace('dataraft.adapters', quietly = TRUE)) {
  yaml <- tempfile(fileext = '.yaml')
  dataraft.adapters::dr_contract_odcs(
    dataraft.core::dr_contract('orders', columns = c(amount = 'numeric')),
    yaml
  )
  for (operation in c('validate_contract', 'sample_quality')) {
    path <- tempfile(fileext = '.json')
    dataraft.ide::ide_emit(
      operation,
      path,
      paste0('fixture-', operation),
      context,
      handle = if (operation == 'sample_quality') 'binding:delivery' else NULL,
      file_path = yaml
    )
    file.copy(
      path,
      file.path('inst', 'fixtures', paste0(operation, '.json')),
      overwrite = TRUE
    )
    unlink(path)
  }
  unlink(yaml)
}
# Mock the viewer only: the actual dispatch, bounded selection and envelope run.
testthat::with_mocked_bindings(
  {
    path <- tempfile(fileext = '.json')
    dataraft.ide::ide_emit(
      'view',
      path,
      'fixture-view',
      context,
      'binding:delivery'
    )
    file.copy(path, 'inst/fixtures/view.json', overwrite = TRUE)
    unlink(path)
  },
  View = function(x, title) invisible(x),
  .package = 'utils'
)
