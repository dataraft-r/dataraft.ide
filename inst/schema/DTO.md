# Bridge v1 DTOs

Request: `dataraft.ide::ide_request(base64_json)`. JSON Schema validates the envelope and request. All envelope keys are always present, absent values use JSON null; collections always use arrays, including zero or one element. IDs, release_order and handles are strings, not numbers. Envelope contract is numeric 1; generated is UTC RFC3339. Error message is a fixed redacted summary, never the caught R message.

Operations products, contexts, quality, releases, runs, freshness and incidents return `data = {items: [...], truncated: boolean}`. Lineage returns `{nodes: [...], edges: [...], truncated: boolean}`. Product returns the detail record directly. View/trial return `{handle: string, status: string}`; trial additionally includes `result: product detail` and retains a session-local result handle. No rows ever cross the bridge.

* Context: `{handle,label,kind}`; kind workspace or lake. Workspace handle is `workspace`.
* Product summary: `{handle,id,version,status,kind,owner,description,source_count,rule_count,can_trial,can_view}`. Nullable text fields. kind product/result/table/asset.
* Product detail adds `{contract,sources,rules}`. contract is null or `{id,version,columns:[{name,type,required}],key:[string]}`. Sources `{name,kind,product_id}`. Rules `{id,engine,action,dimension}`. No expressions, connections or raw source locations.
* Lineage node `{id,kind}`; edge `{from,to,relation}`.
* Quality/incident `{run_id,asset,rule,status,severity,engine,stage,n_failed,n_total}`. Counts nullable numbers. No failure rows/messages/details.
* Release `{release_id,asset,run_id,release_order,published_at,quality,business_date,parent_release}`.
* Run `{run_id,asset,status,started_at,finished_at,release_id}`.
* Freshness `{asset,release_id,published_at,freshness,published_quality,latest_attempt,age_hours,max_age_hours}`.

`binding:<name>` refers to an exact existing non-active, non-lazy binding in the configured workspace. Clients MUST treat every handle as opaque. `asset:<base64 JSON [lakeBinding,assetId]>` handles refer only to assets in a currently available lake context. `result:<token>` refers to the bridge's last 20 trial results. No console parsing, periodic execution, source evaluation or publishing operation exists. Explicit View is bounded and stays inside R; explicit Trial invokes dr_trial and may read sources/execute user transforms, without writing the configured destination.

The extension creates a private temporary directory, passes an unused absolute JSON response path, and checks matching request_id and a 1 MiB file bound. R writes a sibling temporary file and renames once; an existing response file is never replaced. Request is at most 16 KiB. Limits bound emitted selections (default 100, max500); full lake metadata queries may still be needed by current DataRaft APIs.

Reports operation returns collection `{id,created_at}`. Profile/validate_contract return the contract shape directly; sample_quality returns a quality collection. ODCS YAML uses explicit `file_path`, at most1MiB. Profile/sample only accept known in-memory table bindings.
