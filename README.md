# dataraft.ide

An experimental metadata bridge for DataRaft IDE clients. The package provides an R-only workspace workflow; DuckDB and a running lake are optional. It does not publish products from the IDE.

```r
workspace <- new.env(parent = emptyenv())
workspace$orders <- dataraft.core::dr_product(
  "orders", data.frame(amount = c(-10, 20))
) |> dataraft.core::dr_add_quality(~ amount >= 0)
context <- dataraft.ide::ide_context(workspace)
request_metadata <- function(operation, handle = NULL) {
  private <- tempfile("ide-private-")
  dir.create(private, mode = "0700")
  on.exit(unlink(private, recursive = TRUE))
  request <- list(
    version = 1L, operation = operation, request_id = "example",
    response_path = file.path(private, "response.json")
  )
  if (!is.null(handle)) request$handle <- handle
  encoded <- jsonlite::base64_enc(charToRaw(as.character(
    jsonlite::toJSON(request, auto_unbox = TRUE)
  )))
  dataraft.ide::ide_request(gsub("[[:space:]]", "", encoded), context)
}
request_metadata("products")
workspace$result <- dataraft.core::dr_trial(workspace$orders)
request_metadata("incidents")
```

Only `ide_context()` and `ide_request()` are public. All other `ide_*` helpers are internal and may change without notice. Existing callers of `ide_products(context)` should use a `products` request as above; this applies equally to the other named operations. `ide_emit()` is internal too. The bridge validates allowed requests in R; clients validate the emitted envelope against the JSON Schema.

`ide_context()` accepts an explicit workspace environment, optional exact object names and an already connected lake. Discovery skips active and delayed bindings and never runs source or transform callbacks. Workflows, model products, retained results and in-memory tables have opaque handles. Lake metadata comes from the current registry, including published contracts, latest attempt status, release ordering and lineage. Incident records describe observed failed or unchecked checks, not incident resolution state.

The bridge only returns allowlisted metadata. Version 1 excludes data cells, source locations, credentials, SQL, formula expressions, diagnostic messages and closure environments. Product identifiers, column names, rule identifiers, owners and business descriptions are operational metadata and remain visible; treat access to the R session accordingly. Automatically derived rule expressions are omitted from evidence records; product rule summaries use positional display labels.

## Private file transport

The client creates a private temporary directory and sends base64 JSON to `dataraft.ide::ide_request(encoded)`. Requests specify numeric `version: 1`, `operation`, `request_id` and an unused absolute `response_path`. The response is a complete envelope with numeric `contract: 1`, timestamp, kind, matching request ID, data and error. The writer atomically renames a temporary sibling file. Existing files and links are rejected. Clients must bound file reads, validate the bundled JSON Schema and match request IDs; console output is not a transport.

Requests are limited to 16 KiB encoded, responses to 1 MiB, collections to 500 items and explicitly requested samples/views to 1000 rows. Current lake APIs can fetch full registry metadata before these output limits are applied. There is no polling or automatic execution. Invalid operations produce fixed redacted error envelopes. A trusted local session and caller-owned private directory are preconditions, not a remote authentication scheme.

`view` opens bounded data inside R and returns only an acknowledgement. It supports tables, retained tabular results and ordinary lake table releases. Nonretained result outputs and model manifests are not viewable. `trial` explicitly runs `dr_trial()` and retains up to 20 results in the session. Trials disable configured framework writers, but source and transformation functions remain user code with their own potential side effects. A completed request can contain a failed trial result; its status must be shown.

The optional adapters package supplies safe ODCS 3.2 contract import. `profile` returns only an in-memory table's column names and types. `validate_contract` checks a saved YAML file of at most 1 MiB. `sample_quality` evaluates that contract against the first bounded rows of an explicit in-memory table and returns aggregate evidence. The bridge never guesses editor source positions.

An explicit `diagnostics` request using wire discriminator `version: 2` accepts a retained trial result handle. Native function rules sourced with `keep.source = TRUE` can return verified local source paths, SHA-256 file hashes, and exact zero-based UTF-16 ranges with exclusive ends. References are captured before the trial and omitted if files have changed. Only unique failed rule IDs match; ordinary formulas, schema checks, ambiguous IDs and model/nested checks without a flat mapping are omitted. This is function-reference support, not general R-code annotation. Files must be UTF-8 and no larger than 1 MiB, with at most 32 files examined per operation. Clients must verify both workspace locality and the hash of the current editor content before displaying annotations. The explicit operation shares source locations, but never source text, rows or underlying error messages.

See `inst/schema/bridge-metadata-v1.json` for metadata channel schema version 1 and `inst/schema/bridge-diagnostics-v1.json` for diagnostics channel schema version 1, `inst/schema/DTO.md` for client guidance, and `inst/fixtures/` for actual R-generated response examples. Regenerate with `source("tools/generate-fixtures.R")`, then run `python tools/validate-schema.py` with jsonschema installed. Component dependencies are pinned to immutable commits in DESCRIPTION.

Schema filenames version each independent channel. The existing numeric wire values `version`/`contract` 1 (metadata) and 2 (diagnostics) are retained for compatibility. Diagnostics is not a successor to the metadata channel and does not provide its operations. No wire payload changes accompany the schema rename.
