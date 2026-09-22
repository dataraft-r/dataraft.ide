# dataraft.ide 0.1.0.9004

* Contract validation and sample quality reads are confined to trusted canonical
  `ide_context(read_roots = ...)` directories. Encoded requests cannot authorize
  additional directories. The default captures the working directory.

# dataraft.ide 0.1.0.9003

* Bound response writes to a trusted `ide_context(response_root = ...)`, defaulting
  to the R session temporary directory. Encoded requests cannot change this root;
  contract-file reads are unchanged.
* Bound R lineage output by both node and edge counts, retain complete edge
  endpoints, and propagate upstream truncation.
* Enforce the documented 500-node and 500-edge lineage limit in the metadata
  JSON schema as well as in R responses.

# dataraft.ide 0.1.0.9002

* Limit the experimental public R API to `ide_context()` and `ide_request()`.
  Raw `ide_*` metadata and transport helpers are now internal. Clients migrate
  to named operations through `ide_request()` for correlated envelopes.
* Name the independent schemas `bridge-metadata-v1.json` and
  `bridge-diagnostics-v1.json`. Existing wire discriminators 1 and 2 are
  unchanged; diagnostics is an opt-in parallel channel, not a metadata upgrade.

# dataraft.ide 0.1.0.9000

* Add an experimental, versioned metadata bridge for workspace and connected lake inspection.
* Add bounded private JSON file responses, redacted errors and exact binding handles.
* Support explicit bounded R data views, writer-disabled trials, safe ODCS schema checks and aggregate sample evidence.
* Include strict per-operation JSON schemas, generated response fixtures and compiler-free workspace examples.

* Add explicit v2 diagnostics for retained trials with verified native function source references, UTF-16 positions and bounded SHA-256 file checks. Version-1 metadata remains unchanged.
