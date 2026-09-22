# Read only eager source-file metadata: never force user-supplied bindings.
source_field <- function(env, name) {
  if (
    !is.environment(env) ||
      !exists(name, env, inherits = FALSE) ||
      bindingIsActive(name, env) ||
      rlang::env_binding_are_lazy(env, name)[[1L]]
  ) {
    return(NULL)
  }
  get(name, env, inherits = FALSE)
}

# Files and UTF-8 text stay in R. At most 32 distinct 1 MiB files per operation.
source_file <- function(path, cache) {
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    return(NULL)
  }
  if (!nzchar(path) || nchar(path, type = "bytes") > 4096L) {
    return(NULL)
  }
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!nzchar(path) || nchar(path, type = "bytes") > 4096L) {
    return(NULL)
  }
  if (exists(path, cache, inherits = FALSE)) {
    return(get(path, cache))
  }
  if (length(ls(cache, all.names = TRUE)) >= 32L) {
    return(NULL)
  }
  assign(path, NULL, cache)
  out <- tryCatch(
    {
      info <- fs::file_info(path)
      if (
        is.na(info$size) ||
          is.na(info$type) ||
          info$type != "file" ||
          info$size > 1048576L
      ) {
        return(NULL)
      }
      con <- file(path, "rb")
      on.exit(close(con), add = TRUE)
      bytes <- readBin(con, "raw", n = 1048577L)
      if (length(bytes) > 1048576L || any(bytes == as.raw(0L))) {
        return(NULL)
      }
      text <- rawToChar(bytes)
      if (is.na(iconv(text, from = "UTF-8", to = "UTF-8", sub = NA))) {
        return(NULL)
      }
      lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
      lines <- sub("\r$", "", lines)
      list(
        path = path,
        lines = lines,
        hash = digest::digest(bytes, algo = "sha256", serialize = FALSE)
      )
    },
    error = function(e) NULL
  )
  assign(path, out, cache)
  out
}

# srcref columns count Unicode code points and expand tabs at eight-column stops.
# Byte slots differ across parser/encoding combinations; never infer from them.
utf16_column <- function(line, column) {
  points <- utf8ToInt(enc2utf8(line))
  if (anyNA(points) || !is.finite(column) || column < 0L) {
    return(NULL)
  }
  parser_column <- 0L
  utf16 <- 0L
  if (column == 0L) {
    return(utf16)
  }
  for (point in points) {
    parser_column <- if (point == 9L) {
      (parser_column %/% 8L + 1L) * 8L
    } else {
      parser_column + 1L
    }
    utf16 <- utf16 + if (point > 65535L) 2L else 1L
    if (parser_column == column) {
      return(as.integer(utf16))
    }
    if (parser_column > column) return(NULL)
  }
  NULL
}

function_location <- function(check, cache) {
  if (!is.function(check)) {
    return(NULL)
  }
  ref <- attr(check, "srcref", exact = TRUE)
  if (
    !inherits(ref, "srcref") ||
      !is.integer(ref) ||
      length(ref) < 6L ||
      anyNA(ref)
  ) {
    return(NULL)
  }
  source <- attr(ref, "srcfile", exact = TRUE)
  if (!inherits(source, "srcfilecopy")) {
    return(NULL)
  }
  path <- source_field(source, "filename")
  lines <- source_field(source, "lines")
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !is.character(lines) ||
      anyNA(lines)
  ) {
    return(NULL)
  }
  if (!fs::is_absolute_path(path)) {
    wd <- source_field(source, "wd")
    if (!is.character(wd) || length(wd) != 1L || is.na(wd)) {
      return(NULL)
    }
    path <- file.path(wd, path)
  }
  file <- source_file(path, cache)
  if (is.null(file) || !identical(enc2utf8(lines), enc2utf8(file$lines))) {
    return(NULL)
  }
  if (
    any(ref[c(1L, 2L, 3L, 4L)] < 1L) ||
      ref[[1L]] > ref[[3L]] ||
      ref[[3L]] > length(lines)
  ) {
    return(NULL)
  }
  start <- utf16_column(lines[[ref[[1L]]]], ref[[5L]] - 1L)
  end <- utf16_column(lines[[ref[[3L]]]], ref[[6L]])
  if (
    is.null(start) || is.null(end) || (ref[[1L]] == ref[[3L]] && end <= start)
  ) {
    return(NULL)
  }
  list(
    path = file$path,
    file_hash = file$hash,
    start = list(line = ref[[1L]] - 1L, character = start),
    end = list(line = ref[[3L]] - 1L, character = end)
  )
}

capture_rule_sources <- function(product) {
  rules <- c(
    field(field(product, "contract"), "rules"),
    field(product, "quality")
  )
  if (!is.list(rules) || !length(rules)) {
    return(list())
  }
  ids <- vapply(
    rules,
    function(rule) {
      name <- field(rule, "name")
      if (
        !is.character(name) ||
          length(name) != 1L ||
          is.na(name) ||
          !identical(rule_id(name, ""), name)
      ) {
        ""
      } else {
        name
      }
    },
    character(1)
  )
  duplicate <- duplicated(ids) | duplicated(ids, fromLast = TRUE)
  cache <- new.env(parent = emptyenv())
  out <- list()
  for (i in seq_len(min(length(rules), 500L))) {
    if (
      !nzchar(ids[[i]]) ||
        duplicate[[i]] ||
        !identical(field(rules[[i]], "engine"), "r")
    ) {
      next
    }
    location <- tryCatch(
      function_location(field(rules[[i]], "check"), cache),
      error = function(e) NULL
    )
    if (!is.null(location)) out[[ids[[i]]]] <- location
  }
  out
}

#' Locate failed native function rules in their unchanged source files
#'
#' Returns bounded metadata for an IDE-retained trial result. Before a bridge
#' trial executes, native function checks with real `srcref` and `srcfilecopy`
#' attributes are matched against the original UTF-8 source file. Only exact,
#' unique failed rule IDs are joined to this snapshot. Files must still match
#' their SHA-256 hash when diagnostics are requested.
#'
#' Source a file with `keep.source = TRUE` to preserve function references.
#' Ordinary formulas have no reliable source references and are omitted, as are
#' generated/schema rules, duplicates, changed or missing files, non-UTF-8 files,
#' and model or nested product checks without a unique flat rule mapping. This
#' does not infer positions from R expression text or execute a check again.
#'
#' Positions are zero-based UTF-16 offsets with an exclusive end, suitable for
#' IDE diagnostics. Each item includes an absolute path and SHA-256 `file_hash`;
#' clients must verify that hash against the editor text before annotating it.
#' Neither source text, row values nor underlying error messages are returned.
#' At most 500 original rules and 32 distinct files of one MiB each are inspected.
#' Snapshots are retained for the same last 20 bridge trials as their results.
#'
#' This operation uses the separate version-2 diagnostics wire schema. Existing
#' version-1 requests and responses are unchanged.
#' @param handle A retained `result:` handle returned by a bridge trial.
#' @param limit Maximum diagnostics to return, from 1 to 500.
#' @returns A list with `items` and `truncated`. Each item contains `rule`,
#'   `status`, `severity`, `path`, `file_hash`, `start` and `end`.
#' @export
ide_diagnostics <- function(handle, limit = 100L) {
  limit <- limit_value(limit)
  if (
    !is.character(handle) ||
      length(handle) != 1L ||
      is.na(handle) ||
      !startsWith(handle, "result:")
  ) {
    ide_abort()
  }
  result <- .ide_state$results[[handle]]
  if (is.null(result)) {
    ide_abort("not_found")
  }
  sources <- .ide_state$rule_sources[[handle]]
  quality <- field(result, "quality")
  if (
    !is.data.frame(quality) ||
      !all(c("rule", "status", "severity") %in% names(quality))
  ) {
    return(collection(list(), limit))
  }
  ids <- quality$rule
  duplicate <- duplicated(ids) | duplicated(ids, fromLast = TRUE)
  cache <- new.env(parent = emptyenv())
  out <- list()
  for (i in seq_len(nrow(quality))) {
    id <- ids[[i]]
    status <- quality$status[[i]]
    severity <- quality$severity[[i]]
    if (
      is.na(id) ||
        duplicate[[i]] ||
        is.na(status) ||
        !status %in% c("failed", "error", "warning") ||
        is.na(severity) ||
        !severity %in% c("error", "warning")
    ) {
      next
    }
    location <- sources[[id]]
    if (is.null(location)) {
      next
    }
    file <- source_file(location$path, cache)
    if (is.null(file) || !identical(file$hash, location$file_hash)) {
      next
    }
    out[[length(out) + 1L]] <- c(
      list(rule = id, status = status, severity = severity),
      location
    )
    if (length(out) > limit) break
  }
  collection(out, limit)
}
