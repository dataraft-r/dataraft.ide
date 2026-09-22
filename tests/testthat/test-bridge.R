request_wire <- function(x) {
  gsub(
    '[\r\n]',
    '',
    jsonlite::base64_enc(charToRaw(as.character(jsonlite::toJSON(
      x,
      auto_unbox = TRUE,
      null = 'null'
    ))))
  )
}
private_response <- function() {
  dir <- tempfile('ide-private-')
  dir.create(dir, mode = '0700')
  withr::defer(unlink(dir, recursive = TRUE), envir = parent.frame())
  file.path(dir, 'response.json')
}

test_that('private file response correlates requests and preserves JSON arrays', {
  path <- private_response()
  request <- list(
    version = 1,
    operation = 'products',
    request_id = 'request-123',
    response_path = path
  )
  result <- ide_request(
    request_wire(request),
    ide_context(new.env(parent = emptyenv()))
  )
  expect_identical(result$contract, 1L)
  expect_identical(result$request_id, 'request-123')
  raw <- paste(readLines(path, warn = FALSE), collapse = '')
  expect_match(raw, '"items":\\[\\]')
  expect_match(raw, '"error":null')
  expect_identical(
    list.files(dirname(path), all.files = TRUE, no.. = TRUE),
    'response.json'
  )
  old <- readBin(path, 'raw', n = file.info(path)$size)
  expect_identical(ide_request(request_wire(request))$error$code, 'unsafe_path')
  expect_identical(readBin(path, 'raw', n = file.info(path)$size), old)
})

test_that('malformed and unknown requests fail closed with redacted errors', {
  context <- ide_context(new.env(parent = emptyenv()))
  for (encoded in c(
    'not base64()',
    'e30=',
    paste(rep('a', 20000), collapse = '')
  )) {
    expect_identical(ide_request(encoded, context)$kind, 'error')
  }
  path <- private_response()
  request <- list(
    version = 1,
    operation = 'publish',
    request_id = 'bad',
    response_path = path,
    password = 'very-secret'
  )
  result <- ide_request(request_wire(request), context)
  expect_identical(result$error$code, 'invalid_request')
  expect_false(grepl(
    'very-secret|publish|password',
    paste(readLines(path, warn = FALSE), collapse = '')
  ))
  expect_false(file.exists(paste0(path, '.tmp')))
})

test_that('user callback errors never disclose messages or leave partial files', {
  e <- new.env(parent = emptyenv())
  e$orders <- dataraft.core::dr_product('orders', function() {
    stop('postgres://user:password@example/private')
  })
  path <- private_response()
  withr::defer({
    .ide_state$results <- list()
    .ide_state$serial <- 0L
  })
  result <- ide_emit(
    'trial',
    path,
    'trial-error',
    ide_context(e),
    'binding:orders'
  )
  expect_identical(result$kind, 'trial')
  expect_identical(result$data$status, 'error')
  expect_false(grepl(
    'postgres|password|example/private',
    paste(readLines(path, warn = FALSE), collapse = '')
  ))
})

test_that('symlinks are not overwritten and payload sizes are bounded', {
  path <- private_response()
  target <- file.path(dirname(path), 'target.json')
  writeLines('keep', target)
  fs::link_create(target, path)
  expect_error(
    ide_emit('contexts', path, 'links'),
    class = 'dataraft_ide_error'
  )
  expect_identical(readLines(target), 'keep')
  fs::link_delete(path)
  result <- write_response(
    bridge_envelope('products', list(payload = strrep('x', 1048577)), 'large'),
    path
  )
  expect_identical(result$error$code, 'response_too_large')
  expect_lt(file.info(path)$size, 1048576)
})

test_that('contract metadata excludes expressions and sensitive connection descriptions', {
  e <- new.env(parent = emptyenv())
  e$orders <- dataraft.core::dr_product('orders', data.frame(amount = 1)) |>
    dataraft.core::dr_add_quality(~ amount >= 0)
  e$orders$description <- 'token=do-not-emit'
  e$orders$sources[[1]] <- list(
    sql = 'SELECT private',
    password = 'do-not-emit'
  )
  detail <- ide_product('binding:orders', ide_context(e))
  raw <- jsonlite::toJSON(detail, auto_unbox = TRUE, null = 'null')
  expect_false(grepl('SELECT|do-not-emit|amount >=', raw))
  expect_identical(detail$description, '[redacted]')
})
