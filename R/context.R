.ide_state <- new.env(parent = emptyenv())
.ide_state$results <- list()
.ide_state$rule_sources <- list()
.ide_state$serial <- 0L

ide_abort <- function(code = "invalid_request") {
  messages <- c(
    invalid_request = "Invalid IDE request.",
    not_found = "The selected object is no longer available.",
    unsupported = "This operation is not supported for the selected object.",
    unavailable = "An optional DataRaft component is unavailable.",
    execution_failed = "The R operation failed; inspect it locally in R.",
    response_too_large = "The metadata response exceeds the size limit.",
    unsafe_path = "The file path is outside its trusted directory or is unsafe."
  )
  stop(structure(
    list(message = unname(messages[[code]]), call = NULL, code = code),
    class = c("dataraft_ide_error", "error", "condition")
  ))
}

field <- function(x, name) {
  if (!is.list(x)) {
    return(NULL)
  }
  .subset2(x, name)
}

text_value <- function(x, default = NULL) {
  if (!is.character(x) || is.object(x) || length(x) != 1L || is.na(x)) {
    return(default)
  }
  x <- enc2utf8(substr(x, 1L, 512L))
  if (
    grepl(
      "password|passwd|bearer[[:space:]]|token[[:space:]=:]|secret[[:space:]=:]|api[_-]?key|authorization|credential",
      x,
      ignore.case = TRUE
    )
  ) {
    return("[redacted]")
  }
  x <- gsub("[A-Za-z][A-Za-z0-9+.-]*://[^[:space:]]+", "[redacted-uri]", x)
  x
}

number_value <- function(x) {
  if (!is.numeric(x) || is.object(x) || length(x) != 1L || !is.finite(x)) {
    NULL
  } else {
    as.numeric(x)
  }
}

limit_value <- function(x, maximum = 500L) {
  if (
    !is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      x != floor(x) ||
      x < 1L ||
      x > maximum
  ) {
    ide_abort()
  }
  as.integer(x)
}

#' Select an explicit metadata workspace and optional connected lake
#'
#' Discovery skips active and delayed bindings. No source, transformation or
#' adapter callbacks run during discovery. Only explicit trial and viewer
#' requests execute user code or read data. Objects and lake connections stay
#' inside R and are never serialized into responses.
#' @param workspace Environment containing product, result or table bindings.
#' @param objects Optional exact binding names to discover, at most 500.
#' @param lake Optional already connected DataRaft lake. Never opens a connection.
#' @param response_root Trusted existing directory allowed to receive responses.
#'   Defaults to the R session temporary directory. Configure this in trusted R
#'   code for an IDE-owned private directory; encoded requests cannot change it.
#' @param read_roots Trusted existing directories allowed for contract reads.
#'   Defaults to the working directory captured when this context is created.
#'   Use `character()` to disable reads. Encoded requests cannot grant access.
#' @returns An IDE context used by metadata functions.
#' @export
#' @examples
#' workspace <- new.env(parent = emptyenv())
#' workspace$orders <- dataraft.core::dr_product("orders", data.frame(id = 1:2))
#' context <- ide_context(workspace)
#' # Pass context to ide_request(); see its complete request example.
ide_context <- function(
  workspace = .GlobalEnv,
  objects = NULL,
  lake = NULL,
  response_root = tempdir(),
  read_roots = getwd()
) {
  if (!is.environment(workspace)) {
    ide_abort()
  }
  if (
    !is.null(objects) &&
      (!is.character(objects) ||
        anyNA(objects) ||
        length(objects) > 500L ||
        anyDuplicated(objects))
  ) {
    ide_abort()
  }
  if (!is.null(lake) && !inherits(lake, "dr_lake")) {
    ide_abort()
  }
  structure(
    list(
      workspace = workspace, objects = objects, lake = lake,
      response_root = response_directory(response_root),
      read_roots = read_directories(read_roots)
    ),
    class = "dataraft_ide_context"
  )
}

check_context <- function(context) {
  if (!inherits(context, "dataraft_ide_context")) {
    ide_abort()
  }
  # A previously canonical trusted root must not move through a replaced ancestor.
  if (!identical(response_directory(context$response_root), context$response_root)) {
    ide_abort("unsafe_path")
  }
  if (!identical(read_directories(context$read_roots), context$read_roots)) {
    ide_abort("unsafe_path")
  }
  context
}

bindings <- function(context) {
  context <- check_context(context)
  names <- context$objects
  if (is.null(names)) {
    names <- ls(context$workspace, all.names = FALSE, sorted = TRUE)
  }
  names <- names[names %in% ls(context$workspace, all.names = TRUE)]
  names <- names[nchar(names, type = "bytes") <= 256L]
  if (!length(names)) {
    return(character())
  }
  unsafe <- rlang::env_binding_are_active(context$workspace, names) |
    rlang::env_binding_are_lazy(context$workspace, names)
  names[!unsafe]
}

binding_object <- function(name, context) {
  if (length(name) != 1L || !name %in% bindings(context)) {
    ide_abort("not_found")
  }
  get(name, envir = context$workspace, inherits = FALSE)
}

object_kind <- function(x) {
  if (inherits(x, c("dr_product", "dr_product_workflow"))) {
    "product"
  } else if (inherits(x, "dr_run_result")) {
    "result"
  } else if (is.data.frame(x)) {
    "table"
  } else {
    NULL
  }
}

asset_handle <- function(context_name, id) {
  paste0(
    "asset:",
    gsub(
      "[\r\n]",
      "",
      jsonlite::base64_enc(charToRaw(jsonlite::toJSON(
        list(context_name, id),
        auto_unbox = TRUE
      )))
    )
  )
}

resolve_handle <- function(handle, context) {
  if (
    !is.character(handle) ||
      length(handle) != 1L ||
      is.na(handle) ||
      nchar(handle, type = "bytes") > 2048L
  ) {
    ide_abort()
  }
  if (startsWith(handle, "binding:")) {
    name <- substring(handle, 9L)
    x <- binding_object(name, context)
    return(list(object = x, kind = object_kind(x), id = name, handle = handle))
  }
  if (startsWith(handle, "result:")) {
    x <- .ide_state$results[[handle]]
    if (is.null(x)) {
      ide_abort("not_found")
    }
    return(list(object = x, kind = "result", id = handle, handle = handle))
  }
  if (startsWith(handle, "asset:")) {
    parts <- tryCatch(
      jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(substring(
        handle,
        7L
      )))),
      error = function(e) NULL
    )
    if (!is.character(parts) || length(parts) != 2L || anyNA(parts)) {
      ide_abort()
    }
    lake <- select_lake(parts[[1]], context)
    assets <- lake_table(lake, "assets")
    if (!parts[[2]] %in% as.character(assets$id)) {
      ide_abort("not_found")
    }
    return(list(
      object = lake,
      kind = "asset",
      id = parts[[2]],
      handle = handle
    ))
  }
  ide_abort("not_found")
}

select_lake <- function(name, context) {
  if (identical(name, "lake") && !is.null(context$lake)) {
    return(context$lake)
  }
  if (
    !is.character(name) || length(name) != 1L || !startsWith(name, "binding:")
  ) {
    ide_abort("not_found")
  }
  x <- binding_object(substring(name, 9L), context)
  if (!inherits(x, "dr_lake")) {
    ide_abort("unsupported")
  }
  x
}

lake_table <- function(lake, table) {
  if (!requireNamespace("dataraft.lake", quietly = TRUE)) {
    ide_abort("unavailable")
  }
  dataraft.lake::dr_registry(lake, table)
}

collection <- function(items, limit) {
  limit <- limit_value(limit)
  list(
    items = unname(utils::head(items, limit)),
    truncated = length(items) > limit
  )
}

#' List available workspace and lake contexts
#' @param context Context from [ide_context()].
#' @param limit Maximum records, from 1 to 500.
#' @returns A list containing items and a truncation flag.
#' @keywords internal
ide_contexts <- function(context = ide_context(), limit = 100L) {
  items <- list(list(
    handle = "workspace",
    label = "R workspace",
    kind = "workspace"
  ))
  if (!is.null(context$lake)) {
    items[[length(items) + 1L]] <- list(
      handle = "lake",
      label = "Connected lake",
      kind = "lake"
    )
  }
  for (name in bindings(context)) {
    if (inherits(binding_object(name, context), "dr_lake")) {
      items[[length(items) + 1L]] <- list(
        handle = paste0("binding:", name),
        label = text_value(name),
        kind = "lake"
      )
    }
  }
  collection(items, limit)
}

viewable_data <- function(x) is.data.frame(x) || inherits(x, "tbl_lazy")
rule_id <- function(x, fallback = "rule") {
  value <- text_value(x)
  if (is.null(value) || !grepl("^[[:alnum:]_.:-]+$", value)) fallback else value
}
