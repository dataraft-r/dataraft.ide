bridge_operations <- c(
  "contexts",
  "products",
  "product",
  "lineage",
  "quality",
  "releases",
  "runs",
  "freshness",
  "incidents",
  "reports",
  "view",
  "trial",
  "profile",
  "validate_contract",
  "sample_quality"
)

response_directory <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !fs::is_absolute_path(path)) {
    ide_abort("unsafe_path")
  }
  tryCatch({
    info <- fs::file_info(path, follow = FALSE)
    if (is.na(info$type) || info$type != "directory") {
      ide_abort("unsafe_path")
    }
    normalizePath(path, winslash = "/", mustWork = TRUE)
  }, error = function(e) ide_abort("unsafe_path"))
}

response_location <- function(path, response_root = response_directory(tempdir())) {
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      nchar(path, type = "bytes") > 4096L ||
      !fs::is_absolute_path(path) ||
      !grepl("^[A-Za-z0-9._-]+[.]json$", basename(path))
  ) {
    ide_abort("unsafe_path")
  }
  parent <- dirname(path)
  info <- tryCatch(fs::file_info(parent, follow = FALSE), error = function(e) {
    ide_abort("unsafe_path")
  })
  if (is.na(info$type) || info$type != "directory") {
    ide_abort("unsafe_path")
  }
  parent <- response_directory(parent)
  root <- response_directory(response_root)
  if (!identical(root, response_root)) {
    ide_abort("unsafe_path")
  }
  # Compare complete path components, not lexical prefixes; canonicalize first
  # so dot-dot segments and symlinked ancestors cannot escape the trusted root.
  prefix <- paste0(sub("/+$", "", root), "/")
  # normalizePath() resolves platform-native spelling, including Windows paths.
  if (!identical(parent, root) && !startsWith(parent, prefix)) {
    ide_abort("unsafe_path")
  }
  exists <- tryCatch(
    {
      !is.na(fs::file_info(path, follow = FALSE)$type)
    },
    ENOENT = function(e) FALSE,
    error = function(e) ide_abort("unsafe_path")
  )
  if (exists) {
    ide_abort("unsafe_path")
  }
  file.path(
    normalizePath(parent, winslash = "/", mustWork = TRUE),
    basename(path)
  )
}

bridge_envelope <- function(
  kind,
  data = NULL,
  request_id = NULL,
  error = NULL,
  version = 1L
) {
  list(
    contract = version,
    generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
    kind = kind,
    request_id = request_id,
    data = data,
    error = error
  )
}

error_envelope <- function(error, request_id = NULL, version = 1L) {
  code <- if (inherits(error, "dataraft_ide_error")) {
    error$code
  } else {
    "execution_failed"
  }
  message <- tryCatch(ide_abort(code), error = function(e) conditionMessage(e))
  bridge_envelope(
    "error",
    request_id = request_id,
    error = list(code = code, message = message),
    version = version
  )
}

write_response <- function(
  envelope, path, max_bytes = 1048576L,
  response_root = response_directory(tempdir())
) {
  path <- response_location(path, response_root)
  json <- jsonlite::toJSON(
    envelope,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA,
    force = TRUE
  )
  if (nchar(json, type = "bytes") > max_bytes) {
    envelope <- error_envelope(
      structure(
        list(code = "response_too_large"),
        class = "dataraft_ide_error"
      ),
      envelope$request_id,
      version = envelope$contract
    )
    json <- jsonlite::toJSON(
      envelope,
      auto_unbox = TRUE,
      null = "null",
      na = "null"
    )
  }
  temporary <- tempfile(".dataraft-response-", tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  writeBin(charToRaw(enc2utf8(as.character(json))), temporary)
  Sys.chmod(temporary, mode = "0600")
  response_location(path, response_root)
  if (!file.rename(temporary, path)) {
    ide_abort("unsafe_path")
  }
  invisible(envelope)
}

run_action <- function(operation, handle, context, row_limit) {
  resolved <- resolve_handle(handle, context)
  if (identical(operation, "trial")) {
    if (!identical(resolved$kind, "product")) {
      ide_abort("unsupported")
    }
    source_refs <- capture_rule_sources(resolved$object)
    result <- dataraft.core::dr_trial(resolved$object)
    .ide_state$serial <- .ide_state$serial + 1L
    result_handle <- paste0("result:r", .ide_state$serial)
    .ide_state$results[[result_handle]] <- result
    .ide_state$rule_sources[[result_handle]] <- source_refs
    if (length(.ide_state$results) > 20L) {
      .ide_state$results <- utils::tail(.ide_state$results, 20L)
      .ide_state$rule_sources <- .ide_state$rule_sources[names(
        .ide_state$results
      )]
    }
    return(list(
      handle = result_handle,
      status = text_value(field(result, "status"), "unknown"),
      result = ide_product(result_handle, context)
    ))
  }
  if (!resolved$kind %in% c("table", "result", "asset")) {
    ide_abort("unsupported")
  }
  if (resolved$kind == "asset") {
    if (!requireNamespace("dataraft.lake", quietly = TRUE)) {
      ide_abort("unavailable")
    }
    rows <- dataraft.lake::dr_tbl(resolved$object, resolved$id)
    rows <- dataraft.core::dr_collect(utils::head(rows, row_limit))
  } else if (resolved$kind == "result") {
    data <- field(resolved$object, "data")
    if (!viewable_data(data)) {
      ide_abort("unsupported")
    }
    rows <- dataraft.core::dr_collect(utils::head(data, row_limit))
  } else {
    rows <- utils::head(resolved$object, row_limit)
  }
  utils::View(rows, title = "DataRaft selected data")
  list(handle = handle, status = "viewed")
}

bridge_dispatch <- function(request, context) {
  operation <- request$operation
  if (identical(operation, "diagnostics")) {
    return(ide_diagnostics(
      request$handle,
      if (is.null(request$limit)) 100L else request$limit
    ))
  }
  selection <- if (is.null(request$context)) "workspace" else request$context
  limit <- if (is.null(request$limit)) 100L else limit_value(request$limit)
  row_limit <- if (is.null(request$row_limit)) {
    100L
  } else {
    limit_value(request$row_limit, 1000L)
  }
  handle <- request$handle
  if (
    operation %in%
      c("product", "view", "trial", "profile", "sample_quality") &&
      is.null(handle)
  ) {
    ide_abort()
  }
  switch(
    operation,
    contexts = ide_contexts(context, limit),
    products = ide_products(context, selection, limit),
    product = ide_product(handle, context),
    lineage = ide_lineage(context, selection, handle, limit),
    quality = ide_quality(context, selection, handle, limit),
    releases = ide_releases(context, selection, handle, limit),
    runs = ide_runs(context, selection, handle, limit),
    freshness = ide_freshness(context, selection, handle, limit),
    incidents = ide_incidents(context, selection, handle, limit),
    reports = ide_reports(context, selection, handle, limit),
    view = run_action(operation, handle, context, row_limit),
    trial = run_action(operation, handle, context, row_limit),
    profile = ide_profile(handle, context),
    validate_contract = ide_validate_contract(request$file_path, context),
    sample_quality = ide_sample_quality(
      handle,
      request$file_path,
      context,
      row_limit
    ),
    ide_abort()
  )
}

check_request <- function(request) {
  if (
    !is.list(request) ||
      is.null(names(request)) ||
      anyDuplicated(names(request)) ||
      !all(
        names(request) %in%
          c(
            "version",
            "request_id",
            "response_path",
            "operation",
            "context",
            "handle",
            "limit",
            "row_limit",
            "file_path"
          )
      )
  ) {
    ide_abort()
  }
  if (identical(request$version, 2L) || identical(request$version, 2)) {
    if (
      !identical(request$operation, "diagnostics") ||
        !all(
          names(request) %in%
            c(
              "version",
              "request_id",
              "response_path",
              "operation",
              "handle",
              "limit"
            )
        ) ||
        !is.character(request$handle) ||
        length(request$handle) != 1L ||
        is.na(request$handle) ||
        nchar(request$handle, type = "bytes") > 2048L ||
        !startsWith(request$handle, "result:")
    ) {
      ide_abort()
    }
    if (!is.null(request$limit)) {
      limit_value(request$limit)
    }
    return(invisible(request))
  }
  if (!identical(request$version, 1L) && !identical(request$version, 1)) {
    ide_abort()
  }
  if (
    !is.character(request$operation) ||
      length(request$operation) != 1L ||
      !request$operation %in% bridge_operations
  ) {
    ide_abort()
  }
  for (key in intersect(c("context", "handle", "file_path"), names(request))) {
    value <- request[[key]]
    if (
      !is.character(value) ||
        length(value) != 1L ||
        is.na(value) ||
        nchar(value, type = "bytes") > 4096L
    ) {
      ide_abort()
    }
  }
  if (
    !is.null(request$file_path) &&
      !request$operation %in% c("validate_contract", "sample_quality")
  ) {
    ide_abort()
  }
  if (!is.null(request$limit)) {
    limit_value(request$limit)
  }
  if (!is.null(request$row_limit)) {
    limit_value(request$row_limit, 1000L)
  }
  invisible(request)
}

request_id_value <- function(x) {
  if (
    !is.character(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !grepl("^[A-Za-z0-9_-]{1,128}$", x)
  ) {
    ide_abort()
  }
  x
}

#' Write one bounded metadata response to a private JSON file
#'
#' The caller creates a private response directory and supplies an unused file
#' name inside `context$response_root` (the R temporary directory by default).
#' The root is configured by trusted R code, never by encoded requests.
#' A sibling temporary file is renamed atomically after serialization.
#' Existing files and symbolic links are never overwritten. Use one outstanding
#' request per destination. Responses are limited to one MiB; errors contain
#' fixed redacted messages. The file transport is for a trusted local IDE, not
#' an authentication boundary or a remote network service.
#' @param kind Metadata operation, or explicitly requested view/trial action.
#' @param response_path Unused absolute JSON file path in an existing private directory.
#' @param request_id Correlation identifier containing letters, digits, hyphens or underscores.
#' @param context Context from [ide_context()].
#' @param handle Optional opaque product or result handle.
#' @param selection Workspace or lake context handle.
#' @param limit Maximum metadata records, at most 500.
#' @param row_limit Maximum viewed or sample-checked rows, at most 1000.
#' @param file_path Explicit local ODCS YAML file for contract operations only.
#' @returns Invisibly, the envelope written to the file.
#' @keywords internal
ide_emit <- function(
  kind,
  response_path,
  request_id,
  context = ide_context(),
  handle = NULL,
  selection = "workspace",
  limit = 100L,
  row_limit = 100L,
  file_path = NULL
) {
  context <- check_context(context)
  path <- response_location(response_path, context$response_root)
  id <- request_id_value(request_id)
  request <- list(
    version = 1L,
    operation = kind,
    request_id = id,
    response_path = path,
    context = selection,
    handle = handle,
    limit = limit,
    row_limit = row_limit,
    file_path = file_path
  )
  request <- request[!vapply(request, is.null, logical(1))]
  envelope <- tryCatch(
    {
      check_request(request)
      bridge_envelope(
        kind,
        bridge_dispatch(request, check_context(context)),
        id
      )
    },
    error = function(error) error_envelope(error, id)
  )
  write_response(envelope, path, response_root = context$response_root)
}

#' Receive a base64-encoded IDE request without parsing console output
#'
#' Requests are limited to 16 KiB before decoding. No R expressions are parsed
#' or evaluated. Only named operations and exact binding handles are accepted.
#' Invalid requests without a valid response channel return an invisible error
#' envelope and do not write a file. Active and delayed bindings are skipped.
#' Trials can execute user transformations and read sources; this bridge never
#' exposes a production publication operation. Response writes must remain inside
#' the canonical `response_root` configured by trusted [ide_context()] code.
#' This is a path boundary for requests, not a sandbox against R code running
#' as the same user. Contract reads stay inside the trusted `read_roots`
#' configured in that context; requests cannot configure either boundary.
#' @param encoded Base64 JSON request following the metadata-v1 schema (wire version 1), or the
#'   diagnostics-v1 schema (wire version 2).
#' @param context Context from [ide_context()].
#' @returns Invisibly, a redacted envelope; valid response channels receive it atomically.
#' @export
#' @examples
#' private <- tempfile("ide-private-")
#' dir.create(private, mode = "0700")
#' request <- list(
#'   version = 1L, operation = "products", request_id = "example",
#'   response_path = file.path(private, "response.json")
#' )
#' encoded <- jsonlite::base64_enc(charToRaw(as.character(
#'   jsonlite::toJSON(request, auto_unbox = TRUE)
#' )))
#' response <- ide_request(gsub("[[:space:]]", "", encoded),
#'                         ide_context(new.env(parent = emptyenv())))
#' response$contract
#' unlink(private, recursive = TRUE)
ide_request <- function(encoded, context = ide_context()) {
  id <- NULL
  path <- NULL
  version <- 1L
  envelope <- tryCatch(
    {
      if (
        !is.character(encoded) ||
          length(encoded) != 1L ||
          is.na(encoded) ||
          nchar(encoded, type = "bytes") > 16384L ||
          nchar(encoded) %% 4L != 0L ||
          !grepl("^[A-Za-z0-9+/]*={0,2}$", encoded)
      ) {
        ide_abort()
      }
      request <- jsonlite::fromJSON(
        rawToChar(jsonlite::base64_dec(encoded)),
        simplifyVector = FALSE
      )
      if (identical(request$version, 2L) || identical(request$version, 2)) {
        version <- 2L
      }
      id <- request_id_value(request$request_id)
      context <- check_context(context)
      path <- response_location(request$response_path, context$response_root)
      check_request(request)
      bridge_envelope(
        request$operation,
        bridge_dispatch(request, check_context(context)),
        id,
        version = version
      )
    },
    error = function(error) error_envelope(error, id, version)
  )
  if (!is.null(path)) {
    return(write_response(envelope, path, response_root = context$response_root))
  }
  invisible(envelope)
}
