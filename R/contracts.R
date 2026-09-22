#' Inspect a table schema or check an explicit ODCS contract sample
#'
#' Profiling reports column names and types only. Contract validation imports
#' ODCS with the adapter's safe parser. Sample checks use the first bounded rows
#' of an existing in-memory data frame, never an unevaluated source callback.
#' No values, diagnostics, SQL or expression text leave R. Structural validation
#' is not evidence that a contract describes the intended business semantics.
#' @param handle Exact handle of an in-memory data frame.
#' @param context Context from [ide_context()].
#' @param file_path Explicit local ODCS YAML file, at most one MiB.
#' @param row_limit Maximum sample size, from 1 to 1000.
#' @returns Schema metadata, or aggregate quality records without data rows.
#' @keywords internal
ide_profile <- function(handle, context = ide_context()) {
  resolved <- resolve_handle(handle, context)
  if (!identical(resolved$kind, "table")) {
    ide_abort("unsupported")
  }
  x <- resolved$object
  columns <- lapply(utils::head(names(x), 500L), function(name) {
    column <- .subset2(x, name)
    type <- if (inherits(column, "POSIXct")) {
      "POSIXct"
    } else if (inherits(column, "Date")) {
      "Date"
    } else if (inherits(column, "integer64")) {
      "integer64"
    } else {
      typeof(column)
    }
    list(name = text_value(name, "column"), type = type, required = FALSE)
  })
  list(id = NULL, version = NULL, columns = unname(columns), key = list())
}

read_directories <- function(paths) {
  if (!is.character(paths) || anyNA(paths) || length(paths) > 100L) {
    ide_abort("unsafe_path")
  }
  unname(vapply(paths, response_directory, character(1)))
}

contract_location <- function(path, context) {
  context <- check_context(context)
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      nchar(path, type = "bytes") > 4096L || !fs::is_absolute_path(path)) {
    ide_abort("unsafe_path")
  }
  tryCatch({
    real <- normalizePath(path, winslash = "/", mustWork = TRUE)
    roots <- context$read_roots
    inside <- vapply(roots, function(root) {
      startsWith(real, paste0(sub("/+$", "", root), "/"))
    }, logical(1))
    if (!any(inside)) ide_abort("unsafe_path")
    info <- fs::file_info(real, follow = FALSE)
    if (is.na(info$type) || info$type != "file" ||
        !is.finite(info$size) || info$size > 1048576L) {
      ide_abort("unsafe_path")
    }
    real
  }, error = function(e) ide_abort("unsafe_path"))
}

read_odcs <- function(file_path, context = ide_context()) {
  file_path <- contract_location(file_path, context)
  if (!requireNamespace("dataraft.adapters", quietly = TRUE)) {
    ide_abort("unavailable")
  }
  # Recheck the canonical path immediately before handing it to the parser.
  # This local boundary does not protect against concurrent same-user tampering.
  file_path <- contract_location(file_path, context)
  dataraft.adapters::dr_contract_from_odcs(file_path)
}

#' @rdname ide_profile
#' @keywords internal
ide_validate_contract <- function(file_path, context = ide_context()) {
  contract_metadata(read_odcs(file_path, context))
}

#' @rdname ide_profile
#' @keywords internal
ide_sample_quality <- function(
  handle,
  file_path,
  context = ide_context(),
  row_limit = 100L
) {
  resolved <- resolve_handle(handle, context)
  if (!identical(resolved$kind, "table")) {
    ide_abort("unsupported")
  }
  contract <- read_odcs(file_path, context)
  rows <- utils::head(resolved$object, limit_value(row_limit, 1000L))
  quality <- dataraft.core::dr_quality(dataraft.core::dr_validate(
    rows,
    contract
  ))
  rows_metadata(
    quality,
    c(
      "run_id",
      "asset",
      "rule",
      "status",
      "severity",
      "engine",
      "stage",
      "n_failed",
      "n_total"
    ),
    c("n_failed", "n_total"),
    500L
  )
}
