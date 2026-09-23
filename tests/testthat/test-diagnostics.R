diagnostic_response <- function() {
  dir <- tempfile("diagnostic-private-")
  dir.create(dir, mode = "0700")
  withr::defer(unlink(dir, recursive = TRUE), envir = parent.frame())
  file.path(dir, "response.json")
}
diagnostic_wire <- function(request) {
  gsub(
    "[\r\n]",
    "",
    jsonlite::base64_enc(charToRaw(as.character(jsonlite::toJSON(
      request,
      auto_unbox = TRUE,
      null = "null"
    ))))
  )
}
source_trial <- function(lines = 'bad <- function(data) data$amount >= 0') {
  old_results <- .ide_state$results
  old_sources <- .ide_state$rule_sources
  old_serial <- .ide_state$serial
  withr::defer(
    {
      .ide_state$results <- old_results
      .ide_state$rule_sources <- old_sources
      .ide_state$serial <- old_serial
    },
    envir = parent.frame()
  )
  path <- tempfile(fileext = '.R')
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
  withr::defer(unlink(path), envir = parent.frame())
  workspace <- new.env(parent = globalenv())
  source(path, local = workspace, keep.source = TRUE, encoding = 'UTF-8')
  workspace$product <- dataraft.core::dr_product(
    'orders',
    data.frame(amount = c(-1, 2))
  ) |>
    dataraft.core::dr_add_quality(list(nonnegative = workspace$bad))
  list(path = path, workspace = workspace, context = ide_context(workspace))
}
retained_trial <- function(fixture) {
  run_action('trial', 'binding:product', fixture$context, 10L)$handle
}

test_that('actual failed functions have exact UTF16 source positions and a content hash', {
  line <- '"😀é"; bad <- function(data) data$amount >= 0'
  fixture <- source_trial(line)
  handle <- retained_trial(fixture)
  out <- ide_diagnostics(handle)
  expect_length(out$items, 1L)
  item <- out$items[[1L]]
  expect_identical(item$rule, 'nonnegative')
  expect_identical(item$status, 'failed')
  expect_identical(item$severity, 'error')
  expect_identical(item$start, list(line = 0L, character = 14L))
  expect_identical(item$end, list(line = 0L, character = nchar(line) + 1L))
  expect_identical(item$path, normalizePath(fixture$path, winslash = '/'))
  expect_identical(
    item$file_hash,
    digest::digest(file = fixture$path, algo = 'sha256')
  )
  json <- jsonlite::toJSON(out, auto_unbox = TRUE)
  expect_false(grepl('amount|function|data|😀|>=', json))
  expect_false(out$truncated)
})

test_that('tabs and multiline function refs convert R parser columns to UTF16', {
  fixture <- source_trial(c(
    '\tbad <- function(data) {',
    '  data$amount >= 0',
    '}'
  ))
  item <- ide_diagnostics(retained_trial(fixture))$items[[1L]]
  expect_identical(item$start, list(line = 0L, character = 8L))
  expect_identical(item$end, list(line = 2L, character = 1L))
})

test_that('changed files before and after trial never produce source annotations', {
  fixture <- source_trial()
  writeLines('# changed before trial', fixture$path)
  expect_length(ide_diagnostics(retained_trial(fixture))$items, 0L)
  fixture <- source_trial()
  handle <- retained_trial(fixture)
  expect_length(ide_diagnostics(handle)$items, 1L)
  writeLines('# changed after trial', fixture$path)
  expect_length(ide_diagnostics(handle)$items, 0L)
  unlink(fixture$path)
  expect_length(ide_diagnostics(handle)$items, 0L)
})

test_that('formulas and duplicate or absent rule ids are omitted without guessing', {
  fixture <- source_trial()
  fixture$workspace$product <- dataraft.core::dr_product(
    'formulas',
    data.frame(amount = -1)
  ) |>
    dataraft.core::dr_add_quality(list(nonnegative = ~ amount >= 0))
  expect_length(ide_diagnostics(retained_trial(fixture))$items, 0L)
  fixture <- source_trial()
  definition <- fixture$workspace$product
  definition$quality <- rep(definition$quality, 2)
  expect_length(capture_rule_sources(definition), 0L)
  handle <- retained_trial(fixture)
  result <- .ide_state$results[[handle]]
  quality <- result$quality
  .ide_state$results[[handle]]$quality <- rbind(
    quality,
    quality[quality$rule == 'nonnegative', ]
  )
  expect_length(ide_diagnostics(handle)$items, 0L)
  for (id in c(NA_character_, '', 'unknown')) {
    quality$rule[quality$rule == 'nonnegative'] <- id
    .ide_state$results[[handle]]$quality <- quality
    expect_length(ide_diagnostics(handle)$items, 0L)
    quality <- result$quality
  }
  .ide_state$results[[handle]]$quality <- quality[0, ]
  expect_length(ide_diagnostics(handle)$items, 0L)
})

test_that('malformed refs and active source metadata are never evaluated', {
  fixture <- source_trial()
  check <- fixture$workspace$bad
  for (value in list(
    c(0L, 1L, 1L, 1L, 1L, 1L),
    c(1L, NA_integer_, 1L, 1L, 1L, 1L),
    c(1L, 999L, 1L, 999L, 999L, 999L)
  )) {
    ref <- attr(check, 'srcref')
    ref[] <- rep_len(value, length(ref))
    attr(check, 'srcref') <- ref
    expect_null(function_location(check, new.env(parent = emptyenv())))
  }
  check <- fixture$workspace$bad
  source <- attr(attr(check, 'srcref'), 'srcfile')
  rm('filename', envir = source)
  makeActiveBinding('filename', function() stop('MUST_NOT_EXECUTE'), source)
  expect_null(function_location(check, new.env(parent = emptyenv())))
  expect_error(ide_diagnostics(NA_character_), class = 'dataraft_ide_error')
  expect_error(ide_diagnostics('result:missing'), class = 'dataraft_ide_error')
  expect_error(
    ide_diagnostics('result:missing', 0),
    class = 'dataraft_ide_error'
  )
})

test_that('v2 diagnostics is explicit and v1 responses keep their original schema version', {
  fixture <- source_trial()
  handle <- retained_trial(fixture)
  request <- list(
    version = 2,
    operation = 'diagnostics',
    handle = handle,
    request_id = 'diagnostics-request',
    response_path = diagnostic_response()
  )
  response <- ide_request(diagnostic_wire(request), fixture$context)
  expect_identical(response$contract, 2L)
  expect_identical(response$kind, 'diagnostics')
  expect_identical(response$request_id, 'diagnostics-request')
  expect_length(response$data$items, 1L)
  request$version <- 1
  request$response_path <- diagnostic_response()
  expect_identical(
    ide_request(diagnostic_wire(request), fixture$context)$error$code,
    'invalid_request'
  )
  request$version <- 2
  request$operation <- 'products'
  request$response_path <- diagnostic_response()
  error <- ide_request(diagnostic_wire(request), fixture$context)
  expect_identical(error$contract, 2L)
  expect_identical(error$error$code, 'invalid_request')
  expect_null(error$data)
  request <- list(
    version = 1,
    operation = 'products',
    request_id = 'v1',
    response_path = diagnostic_response()
  )
  expect_identical(
    ide_request(diagnostic_wire(request), fixture$context)$contract,
    1L
  )
})

test_that('retained source snapshots are evicted with results and source file reads are bounded', {
  fixture <- source_trial()
  first <- retained_trial(fixture)
  # Avoid executing 20 redundant trials: preload retained entries, then cross the boundary once.
  for (i in seq_len(20L)) {
    id <- paste0('result:eviction-', i)
    .ide_state$results[[id]] <- .ide_state$results[[first]]
    .ide_state$rule_sources[[id]] <- .ide_state$rule_sources[[first]]
  }
  retained_trial(fixture)
  expect_length(.ide_state$results, 20L)
  expect_identical(names(.ide_state$rule_sources), names(.ide_state$results))
  expect_error(ide_diagnostics(first), class = 'dataraft_ide_error')
  large <- tempfile()
  withr::defer(unlink(large))
  writeBin(charToRaw(paste(rep('x', 1048577L), collapse = '')), large)
  expect_null(source_file(large, new.env(parent = emptyenv())))
})

test_that('combining marks and astral Unicode within and before functions retain exact columns', {
  # Avoid mixing Unicode escapes and literal non-BMP characters in an R string:
  # Windows can replace the literal character while parsing that combination.
  unicode <- intToUtf8(c(101L, 769L, 128512L))
  line <- paste0(
    '"',
    unicode,
    '";\tbad <- function(data) { "',
    unicode,
    '"; data$amount >= 0 }'
  )
  points <- utf8ToInt(line)
  expect_identical(sum(points == 128512L), 2L)
  expect_identical(sum(points == 769L), 2L)
  expect_false(any(points == 65533L))
  fixture <- source_trial(line)
  item <- ide_diagnostics(retained_trial(fixture))$items[[1L]]
  # Prefix has 14 code points (one astral character), regardless of tab width.
  expect_identical(item$start, list(line = 0L, character = 15L))
  expect_identical(item$end, list(line = 0L, character = nchar(line) + 2L))
  expect_null(utf16_column('\tx', 3L))
})

test_that('diagnostic limits truncate only mapped failures and skip passing checks', {
  fixture <- source_trial()
  fixture$workspace$product <- dataraft.core::dr_product(
    'orders',
    data.frame(amount = -1)
  ) |>
    dataraft.core::dr_add_quality(list(
      first = fixture$workspace$bad,
      second = fixture$workspace$bad
    ))
  handle <- retained_trial(fixture)
  out <- ide_diagnostics(handle, limit = 1L)
  expect_length(out$items, 1L)
  expect_true(out$truncated)
  expect_length(ide_diagnostics(handle, limit = 2L)$items, 2L)
  .ide_state$results[[handle]]$quality$status <- 'passed'
  expect_length(ide_diagnostics(handle)$items, 0L)
})
