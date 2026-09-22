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

test_that('trusted response roots bound writes by canonical path components', {
  base <- withr::local_tempdir()
  root <- file.path(base, 'private')
  sibling <- paste0(root, '-other')
  dir.create(root)
  dir.create(sibling)
  dir.create(file.path(root, 'nested'))
  context <- ide_context(new.env(parent = emptyenv()), response_root = root)
  request <- list(version = 1, operation = 'contexts', request_id = 'boundary')
  for (path in c(
    file.path(base, 'outside.json'),
    file.path(sibling, 'prefix.json'),
    file.path(root, '..', 'escape.json')
  )) {
    request$response_path <- path
    result <- ide_request(request_wire(request), context)
    expect_identical(result$error$code, 'unsafe_path')
    expect_false(file.exists(path))
  }
  for (path in c(file.path(root, 'valid.json'),
                 file.path(root, 'nested', 'valid.json'))) {
    request$response_path <- path
    result <- ide_request(request_wire(request), context)
    expect_identical(result$kind, 'contexts')
    expect_true(file.exists(path))
  }
})

test_that('response root configuration is trusted and fails closed', {
  base <- withr::local_tempdir()
  root <- file.path(base, 'private')
  dir.create(root)
  expect_identical(ide_context()$response_root,
                   normalizePath(tempdir(), winslash = '/', mustWork = TRUE))
  for (root_value in list(NULL, NA_character_, character(), 'relative',
                         file.path(base, 'missing'))) {
    expect_error(ide_context(response_root = root_value),
                 class = 'dataraft_ide_error')
  }
  context <- ide_context(response_root = root)
  request <- list(version = 1, operation = 'contexts', request_id = 'override',
                  response_path = file.path(root, 'response.json'),
                  response_root = base)
  expect_identical(ide_request(request_wire(request), context)$error$code,
                   'invalid_request')
  request$response_path <- file.path(base, 'outside.json')
  expect_identical(ide_request(request_wire(request), context)$error$code,
                   'unsafe_path')
  expect_false(file.exists(request$response_path))
})

test_that('symlinked ancestors cannot redirect responses outside the root', {
  base <- withr::local_tempdir()
  root <- file.path(base, 'private')
  outside <- file.path(base, 'outside')
  dir.create(root)
  dir.create(outside)
  dir.create(file.path(outside, 'nested'))
  fs::link_create(outside, file.path(root, 'redirect'))
  context <- ide_context(response_root = root)
  request <- list(version = 1, operation = 'contexts', request_id = 'symlink',
                  response_path = file.path(root, 'redirect', 'nested', 'response.json'))
  expect_identical(ide_request(request_wire(request), context)$error$code,
                   'unsafe_path')
  expect_false(file.exists(file.path(outside, 'nested', 'response.json')))
})

test_that('writers recheck boundaries before creating temporary responses', {
  base <- withr::local_tempdir()
  root <- file.path(base, 'private')
  outside <- file.path(base, 'outside')
  dir.create(root)
  dir.create(outside)
  child <- file.path(root, 'child')
  dir.create(child)
  root <- normalizePath(root, winslash = '/', mustWork = TRUE)
  path <- response_location(file.path(child, 'response.json'), root)
  unlink(child, recursive = TRUE)
  fs::link_create(outside, child)
  expect_error(write_response(bridge_envelope('contexts'), path,
                             response_root = root),
               class = 'dataraft_ide_error')
  expect_length(list.files(outside, all.files = TRUE, no.. = TRUE), 0L)
})


test_that('the canonical root cannot move between dispatch and serialization', {
  base <- withr::local_tempdir()
  parent <- file.path(base, 'parent')
  root <- file.path(parent, 'private')
  outside <- file.path(base, 'outside')
  dir.create(parent)
  dir.create(root)
  dir.create(outside)
  dir.create(file.path(outside, 'private'))
  context <- ide_context(response_root = root)
  path <- file.path(context$response_root, 'response.json')
  unlink(parent, recursive = TRUE)
  fs::link_create(outside, parent)
  expect_error(write_response(bridge_envelope('contexts'), path,
                             response_root = context$response_root),
               class = 'dataraft_ide_error')
  expect_length(list.files(file.path(outside, 'private'),
                           all.files = TRUE, no.. = TRUE), 0L)
})
