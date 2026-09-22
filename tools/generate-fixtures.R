# Fixture generation exercises the same public request boundary as clients.
emit_fixture <- function(kind, response_path, request_id, context,
                         handle = NULL, file_path = NULL) {
  request <- list(version = 1L, operation = kind, response_path = response_path,
                  request_id = request_id)
  if (!is.null(handle)) request$handle <- handle
  if (!is.null(file_path)) request$file_path <- file_path
  encoded <- jsonlite::base64_enc(charToRaw(as.character(
    jsonlite::toJSON(request, auto_unbox = TRUE)
  )))
  dataraft.ide::ide_request(gsub("[\r\n]", "", encoded), context)
}

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
  emit_fixture(
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
emit_fixture(
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
  context$read_roots <- normalizePath(dirname(yaml), winslash = '/', mustWork = TRUE)
  for (operation in c('validate_contract', 'sample_quality')) {
    path <- tempfile(fileext = '.json')
    emit_fixture(
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
    emit_fixture(
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

# Diagnostics schema v1 uses wire discriminator 2; metadata is unchanged.
source_path <- tempfile(fileext = '.R')
writeLines('failed_rule <- function(data) data$amount >= 0', source_path)
source_env <- new.env(parent = baseenv())
source(source_path, local = source_env, keep.source = TRUE)
workspace$failed_rule <- source_env$failed_rule
workspace$located <- dataraft.core::dr_product(
  'located',
  data.frame(amount = c(-1, 2))
) |>
  dataraft.core::dr_add_quality(list(nonnegative = workspace$failed_rule))
trial_path <- tempfile(fileext = '.json')
trial <- emit_fixture(
  'trial',
  trial_path,
  'fixture-located-trial',
  context,
  'binding:located'
)
for (name in c('diagnostics-v2', 'diagnostics-error-v2')) {
  path <- tempfile(fileext = '.json')
  request <- list(
    version = 2L,
    operation = 'diagnostics',
    request_id = name,
    handle = if (name == 'diagnostics-v2') {
      trial$data$handle
    } else {
      'result:missing'
    },
    response_path = path,
    limit = 100L
  )
  encoded <- gsub(
    '[\r\n]',
    '',
    jsonlite::base64_enc(charToRaw(as.character(jsonlite::toJSON(
      request,
      auto_unbox = TRUE
    ))))
  )
  dataraft.ide::ide_request(encoded, context)
  file.copy(
    path,
    file.path('inst', 'fixtures', paste0(name, '.json')),
    overwrite = TRUE
  )
  unlink(path)
}
unlink(c(source_path, trial_path))
