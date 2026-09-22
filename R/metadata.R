contract_metadata <- function(contract) {
  if (!is.list(contract)) {
    return(NULL)
  }
  columns <- field(contract, "columns")
  column_names <- utils::head(names(columns), 500L)
  required <- field(contract, "required")
  key <- field(contract, "key")
  list(
    id = text_value(field(contract, "id")),
    version = text_value(field(contract, "version")),
    columns = unname(lapply(column_names, function(name) {
      list(
        name = text_value(name, "column"),
        type = text_value(
          if (is.list(columns)) field(columns, name) else columns[[name]]
        ),
        required = name %in% required
      )
    })),
    key = unname(lapply(
      utils::head(if (is.character(key)) key else character(), 500L),
      text_value
    ))
  )
}

rules_metadata <- function(x) {
  rules <- c(field(field(x, "contract"), "rules"), field(x, "quality"))
  if (!is.list(rules)) {
    return(list())
  }
  unname(lapply(seq_len(min(length(rules), 500L)), function(i) {
    rule <- .subset2(rules, i)
    list(
      id = rule_id(field(rule, "name"), paste0("rule-", i)),
      engine = text_value(field(rule, "engine")),
      action = text_value(field(rule, "action")),
      dimension = text_value(field(rule, "dimension"))
    )
  }))
}

sources_metadata <- function(x) {
  sources <- field(x, "sources")
  if (!is.list(sources)) {
    return(list())
  }
  unname(lapply(seq_len(min(length(sources), 500L)), function(i) {
    source <- .subset2(sources, i)
    name <- if (length(names(sources)) >= i) names(sources)[[i]] else NULL
    list(
      name = text_value(name, paste0("source-", i)),
      kind = if (inherits(source, "dr_product")) {
        "product"
      } else if (is.data.frame(source)) {
        "table"
      } else if (is.function(source)) {
        "function"
      } else {
        "adapter"
      },
      product_id = if (inherits(source, "dr_product")) {
        text_value(field(source, "id"))
      } else {
        NULL
      }
    )
  }))
}

product_summary <- function(x, handle, fallback, kind = object_kind(x)) {
  if (is.null(kind)) {
    ide_abort("unsupported")
  }
  if (kind == "table") {
    x <- list()
  }
  if (inherits(x, "dr_product_workflow")) {
    x <- workflow_definition(x)
  }
  product <- kind == "product"
  list(
    handle = handle,
    id = text_value(
      field(x, if (kind == "result") "asset" else "id"),
      fallback
    ),
    version = text_value(field(x, "version")),
    status = if (product) {
      "defined"
    } else if (kind == "result") {
      text_value(field(x, "status"), "unknown")
    } else {
      "available"
    },
    kind = kind,
    owner = text_value(field(x, "owner")),
    description = text_value(field(x, "description")),
    source_count = if (product) length(field(x, "sources")) else 0L,
    rule_count = if (product) {
      length(field(field(x, "contract"), "rules")) + length(field(x, "quality"))
    } else {
      0L
    },
    can_trial = product,
    can_view = kind %in%
      c("table", "asset") ||
      (kind == "result" && viewable_data(field(x, "data")))
  )
}

#' Discover product and result metadata without running definitions
#' @param context Context from [ide_context()].
#' @param selection Workspace or lake context handle.
#' @param limit Maximum records, from 1 to 500.
#' @returns Bounded metadata records. No rows or executable code are included.
#' @keywords internal
ide_products <- function(
  context = ide_context(),
  selection = "workspace",
  limit = 100L
) {
  limit <- limit_value(limit)
  if (!identical(selection, "workspace")) {
    lake <- select_lake(selection, context)
    rows <- lake_definitions(lake)
    items <- lapply(seq_len(min(nrow(rows), limit + 1L)), function(i) {
      id <- as.character(rows$id[[i]])
      lake_product(lake, id, asset_handle(selection, id))$summary
    })
    result <- collection(items, limit)
    result$truncated <- nrow(rows) > limit
    return(result)
  }
  names <- bindings(context)
  items <- list()
  for (name in utils::head(names, 2000L)) {
    x <- binding_object(name, context)
    if (is.null(object_kind(x))) {
      next
    }
    items[[length(items) + 1L]] <- product_summary(
      x,
      paste0("binding:", name),
      text_value(name, "object")
    )
    if (length(items) > limit) break
  }
  for (handle in names(.ide_state$results)) {
    if (length(items) > limit) {
      break
    }
    items[[length(items) + 1L]] <- product_summary(
      .ide_state$results[[handle]],
      handle,
      handle
    )
  }
  result <- collection(items, limit)
  result$truncated <- result$truncated || length(names) > 2000L
  result
}

#' Read one product's descriptive contract and source metadata
#' @param handle Opaque handle returned by [ide_products()].
#' @param context Context from [ide_context()].
#' @returns A product metadata record, including contract, sources and rules.
#' @keywords internal
ide_product <- function(handle, context = ide_context()) {
  resolved <- resolve_handle(handle, context)
  x <- resolved$object
  if (identical(resolved$kind, "asset")) {
    metadata <- lake_product(resolved$object, resolved$id, handle)
    return(c(
      metadata$summary,
      list(
        contract = contract_metadata(metadata$definition$contract),
        sources = descriptor_sources(metadata$definition),
        rules = rules_metadata(metadata$definition)
      )
    ))
  }
  if (inherits(x, "dr_product_workflow")) {
    x <- workflow_definition(x)
  }
  if (resolved$kind == "table") {
    x <- list()
  }
  summary <- product_summary(
    x,
    handle,
    text_value(resolved$id, "object"),
    resolved$kind
  )
  contract <- field(x, "contract")
  if (resolved$kind == "result") {
    contract <- field(field(x, "metadata"), "contract")
  }
  c(
    summary,
    list(
      contract = contract_metadata(contract),
      sources = sources_metadata(x),
      rules = rules_metadata(x)
    )
  )
}

rows_metadata <- function(rows, fields, numeric = character(), limit = 100L) {
  if (!is.data.frame(rows)) {
    return(collection(list(), limit))
  }
  n <- nrow(rows)
  items <- lapply(seq_len(min(n, limit + 1L)), function(i) {
    out <- lapply(fields, function(name) {
      column <- .subset2(rows, name)
      if (is.null(column)) {
        return(NULL)
      }
      value <- column[[i]]
      if (name == "rule") {
        return(rule_id(value, NULL))
      }
      if (name %in% numeric) {
        return(number_value(value))
      }
      if (inherits(value, "integer64")) {
        return(as.character(value))
      }
      if (name == "release_order" && is.numeric(value) && is.finite(value)) {
        return(format(value, scientific = FALSE, trim = TRUE))
      }
      text_value(value)
    })
    names(out) <- fields
    out
  })
  result <- collection(items, limit)
  result$truncated <- n > limit
  result
}

selected_rows <- function(table, context, selection, handle = NULL) {
  if (!is.null(handle)) {
    resolved <- resolve_handle(handle, context)
    if (!identical(resolved$kind, "asset")) {
      return(NULL)
    }
    rows <- lake_table(resolved$object, table)
    if ("asset" %in% names(rows)) {
      rows <- rows[rows$asset == resolved$id, , drop = FALSE]
    }
    return(rows)
  }
  if (identical(selection, "workspace")) {
    return(NULL)
  }
  lake_table(select_lake(selection, context), table)
}

#' Read execution metadata for an IDE
#'
#' Lake functions read existing registry metadata; they do not connect, run
#' products, collect product rows or publish outputs. Workspace functions read
#' already retained run results. Descriptions, SQL, diagnostic messages and
#' failure rows are excluded from execution records.
#' @param context Context from [ide_context()].
#' @param selection Workspace or lake context handle.
#' @param handle Optional product or result handle.
#' @param limit Maximum records, from 1 to 500.
#' @returns Bounded records containing metadata only.
#' @keywords internal
ide_runs <- function(
  context = ide_context(),
  selection = "workspace",
  handle = NULL,
  limit = 100L
) {
  limit <- limit_value(limit)
  rows <- selected_rows("runs", context, selection, handle)
  fields <- c(
    "run_id",
    "asset",
    "status",
    "started_at",
    "finished_at",
    "release_id"
  )
  if (!is.null(rows)) {
    return(rows_metadata(
      rows[order(rows$started_at, decreasing = TRUE), , drop = FALSE],
      fields,
      limit = limit
    ))
  }
  results <- workspace_results(context, handle)
  items <- lapply(results, function(x) {
    stats::setNames(lapply(fields, function(f) text_value(field(x, f))), fields)
  })
  collection(items, limit)
}

workspace_results <- function(context, handle = NULL) {
  if (!is.null(handle)) {
    x <- resolve_handle(handle, context)
    return(if (identical(x$kind, "result")) list(x$object) else list())
  }
  results <- unname(.ide_state$results)
  for (name in utils::head(bindings(context), 2000L)) {
    x <- binding_object(name, context)
    if (inherits(x, "dr_run_result")) results[[length(results) + 1L]] <- x
  }
  results
}

#' @rdname ide_runs
#' @keywords internal
ide_quality <- function(
  context = ide_context(),
  selection = "workspace",
  handle = NULL,
  limit = 100L
) {
  limit <- limit_value(limit)
  rows <- selected_rows("quality_results", context, selection, handle)
  fields <- c(
    "run_id",
    "asset",
    "rule",
    "status",
    "severity",
    "engine",
    "stage",
    "n_failed",
    "n_total"
  )
  if (!is.null(rows)) {
    if (!is.null(handle)) {
      resolved <- resolve_handle(handle, context)
      runs <- lake_table(resolved$object, "runs")
      rows <- rows[
        rows$run_id %in% runs$run_id[runs$asset == resolved$id],
        ,
        drop = FALSE
      ]
    }
    return(rows_metadata(rows, fields, c("n_failed", "n_total"), limit))
  }
  items <- list()
  truncated <- FALSE
  for (result in workspace_results(context, handle)) {
    rows <- field(result, "quality")
    if (!is.data.frame(rows)) {
      next
    }
    rows <- as.data.frame(rows)
    rows$run_id <- rep(
      text_value(field(result, "run_id"), "unknown"),
      nrow(rows)
    )
    rows$asset <- rep(text_value(field(result, "asset"), "unknown"), nrow(rows))
    part <- rows_metadata(rows, fields, c("n_failed", "n_total"), limit)
    items <- c(items, part$items)
    truncated <- truncated || part$truncated
  }
  out <- collection(items, limit)
  out$truncated <- out$truncated || truncated
  out
}

#' @rdname ide_runs
#' @keywords internal
ide_incidents <- function(
  context = ide_context(),
  selection = "workspace",
  handle = NULL,
  limit = 100L
) {
  if (
    identical(selection, "workspace") &&
      (is.null(handle) ||
        !identical(resolve_handle(handle, context)$kind, "asset"))
  ) {
    items <- list()
    truncated <- FALSE
    for (run in workspace_results(context, handle)) {
      rows <- dataraft.core::dr_incidents(run)
      rows$asset <- rows$product
      part <- rows_metadata(
        rows,
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
      items <- c(items, part$items)
      truncated <- truncated || part$truncated
    }
    result <- collection(items, 500L)
    result$truncated <- result$truncated || truncated
  } else {
    result <- ide_quality(context, selection, handle, 500L)
    result$items <- Filter(
      function(x) x$status %in% c("failed", "fail", "error", "not_checked"),
      result$items
    )
  }
  out <- collection(result$items, limit)
  out$truncated <- out$truncated || result$truncated
  out
}

#' @rdname ide_runs
#' @keywords internal
ide_releases <- function(
  context = ide_context(),
  selection = "workspace",
  handle = NULL,
  limit = 100L
) {
  limit <- limit_value(limit)
  rows <- selected_rows("releases", context, selection, handle)
  if (is.null(rows)) {
    return(collection(list(), limit))
  }
  if ("release_order" %in% names(rows)) {
    rows <- rows[order(rows$release_order, decreasing = TRUE), , drop = FALSE]
  }
  rows_metadata(
    rows,
    c(
      "release_id",
      "asset",
      "run_id",
      "release_order",
      "published_at",
      "quality",
      "business_date",
      "parent_release"
    ),
    limit = limit
  )
}

#' @rdname ide_runs
#' @keywords internal
ide_freshness <- function(
  context = ide_context(),
  selection = "workspace",
  handle = NULL,
  limit = 100L
) {
  limit <- limit_value(limit)
  if (is.null(handle) && identical(selection, "workspace")) {
    return(collection(list(), limit))
  }
  lake <- if (is.null(handle)) {
    select_lake(selection, context)
  } else {
    resolve_handle(handle, context)$object
  }
  if (!inherits(lake, "dr_lake")) {
    return(collection(list(), limit))
  }
  if (!requireNamespace("dataraft.catalog", quietly = TRUE)) {
    ide_abort("unavailable")
  }
  rows <- dataraft.catalog::dr_freshness(lake)
  if (!is.null(handle)) {
    rows <- rows[
      rows$asset == resolve_handle(handle, context)$id,
      ,
      drop = FALSE
    ]
  }
  rows_metadata(
    rows,
    c(
      "asset",
      "release_id",
      "published_at",
      "freshness",
      "published_quality",
      "latest_attempt",
      "age_hours",
      "max_age_hours"
    ),
    c("age_hours", "max_age_hours"),
    limit
  )
}

#' @rdname ide_runs
#' @keywords internal
ide_reports <- function(
  context = ide_context(),
  selection = "workspace",
  handle = NULL,
  limit = 100L
) {
  rows <- selected_rows("reports", context, selection, handle)
  rows_metadata(rows, c("id", "created_at"), limit = limit_value(limit))
}

#' Read declared dataset lineage without evaluating transformations
#' @inheritParams ide_runs
#' @returns A list with nodes, edges and a truncation flag.
#' @keywords internal
ide_lineage <- function(
  context = ide_context(),
  selection = "workspace",
  handle = NULL,
  limit = 100L
) {
  limit <- limit_value(limit)
  rows <- selected_rows("lineage_edges", context, selection, handle)
  if (!is.null(rows)) {
    if (!is.null(handle)) {
      id <- resolve_handle(handle, context)$id
      rows <- rows[rows$from_id == id | rows$to_id == id, , drop = FALSE]
    }
    truncated <- nrow(rows) > limit
    rows <- utils::head(rows, limit)
    edges <- lapply(seq_len(nrow(rows)), function(i) {
      list(
        from = text_value(rows$from_id[[i]], "unknown"),
        to = text_value(rows$to_id[[i]], "unknown"),
        relation = text_value(rows$relation[[i]], "depends_on")
      )
    })
    ids <- unique(unlist(lapply(edges, function(edge) c(edge$from, edge$to))))
    return(list(
      nodes = unname(lapply(ids, function(id) list(id = id, kind = "asset"))),
      edges = unname(edges),
      truncated = truncated
    ))
  }
  products <- if (is.null(handle)) {
    ide_products(context, limit = limit)$items
  } else {
    list(ide_product(handle, context))
  }
  nodes <- list()
  edges <- list()
  for (product in products) {
    if (product$kind != "product") {
      next
    }
    detail <- ide_product(product$handle, context)
    nodes[[length(nodes) + 1L]] <- list(id = detail$id, kind = "product")
    for (source in detail$sources) {
      id <- source$product_id
      if (is.null(id)) {
        id <- paste0(detail$id, "/", source$name)
      }
      nodes[[length(nodes) + 1L]] <- list(id = id, kind = source$kind)
      edges[[length(edges) + 1L]] <- list(
        from = id,
        to = detail$id,
        relation = "depends_on"
      )
    }
  }
  if (length(nodes)) {
    nodes <- nodes[!duplicated(vapply(nodes, `[[`, character(1), "id"))]
  }
  list(
    nodes = unname(utils::head(nodes, 2L * limit + 1L)),
    edges = unname(utils::head(edges, limit)),
    truncated = length(edges) > limit || length(nodes) > 2L * limit + 1L
  )
}

workflow_definition <- function(x) {
  product <- field(x, "product")
  if (is.null(product)) {
    product <- list()
  }
  product$sources <- c(field(product, "sources"), field(x, "sources"))
  product
}

lake_definitions <- function(lake) {
  rows <- lake_table(lake, "assets")
  rows <- rows[
    rows$kind %in% c("product", "composed_product", "model_product"),
    ,
    drop = FALSE
  ]
  rows <- rows[order(rows$registered_at, decreasing = TRUE), , drop = FALSE]
  rows[!duplicated(rows$id), , drop = FALSE]
}

parse_definition <- function(value) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      nchar(value, type = "bytes") > 1048576L
  ) {
    return(list())
  }
  out <- tryCatch(
    jsonlite::fromJSON(value, simplifyVector = FALSE),
    error = function(e) list()
  )
  if (is.list(out)) out else list()
}

lake_product <- function(lake, id, handle) {
  rows <- lake_definitions(lake)
  rows <- rows[rows$id == id, , drop = FALSE]
  if (!nrow(rows)) {
    ide_abort("not_found")
  }
  definition <- parse_definition(rows$definition[[1]])
  x <- list(
    id = id,
    version = as.character(rows$version[[1]]),
    owner = as.character(rows$owner[[1]]),
    description = as.character(rows$description[[1]])
  )
  releases <- lake_table(lake, "releases")
  releases <- releases[releases$asset == id, , drop = FALSE]
  if (nrow(releases)) {
    releases <- releases[
      order(releases$release_order, decreasing = TRUE),
      ,
      drop = FALSE
    ]
    contract_id <- releases$contract[[1]]
    assets <- lake_table(lake, "assets")
    contracts <- assets[
      assets$kind == "contract" &
        paste(assets$id, assets$version, sep = "@") == contract_id,
      ,
      drop = FALSE
    ]
    if (nrow(contracts)) {
      definition$contract <- parse_definition(contracts$definition[[1]])
    }
  }
  summary <- product_summary(x, handle, id, "asset")
  summary$status <- if (nrow(releases)) {
    text_value(releases$quality[[1]], "unknown")
  } else {
    "unpublished"
  }
  runs <- lake_table(lake, "runs")
  runs <- runs[runs$asset == id, , drop = FALSE]
  if (nrow(runs)) {
    summary$status <- text_value(
      runs$status[[order(runs$started_at, decreasing = TRUE)[[1]]]],
      "unknown"
    )
  }
  summary$can_view <- nrow(releases) > 0L &&
    !startsWith(releases$table_name[[1]], "model_")
  summary$source_count <- length(field(definition, "sources"))
  summary$rule_count <- length(rules_metadata(definition))
  list(summary = summary, definition = definition)
}

descriptor_sources <- function(x) {
  sources <- field(x, "sources")
  if (!is.list(sources)) {
    return(list())
  }
  unname(lapply(seq_len(min(length(sources), 500L)), function(i) {
    source <- .subset2(sources, i)
    name <- if (length(names(sources)) >= i) names(sources)[[i]] else NULL
    list(
      name = text_value(name, paste0("source-", i)),
      kind = text_value(field(source, "type"), "adapter"),
      product_id = text_value(field(source, "id"))
    )
  }))
}
