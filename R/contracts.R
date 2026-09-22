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

read_odcs <- function(file_path) {
  if (!requireNamespace("dataraft.adapters", quietly = TRUE)) {
    ide_abort("unavailable")
  }
  if (
    !is.character(file_path) ||
      length(file_path) != 1L ||
      is.na(file_path) ||
      !file.exists(file_path) ||
      dir.exists(file_path)
  ) {
    ide_abort()
  }
  size <- file.info(file_path)$size
  if (!is.finite(size) || size > 1048576L) {
    ide_abort()
  }
  dataraft.adapters::dr_contract_from_odcs(file_path)
}

#' @rdname ide_profile
#' @keywords internal
ide_validate_contract <- function(file_path) {
  contract_metadata(read_odcs(file_path))
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
  contract <- read_odcs(file_path)
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
