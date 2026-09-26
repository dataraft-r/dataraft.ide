# dataraft.ide

**Make DataRaft metadata available to an editor.**

This optional, experimental R package is the bridge between an existing R workspace and an IDE client. It exposes bounded product, run and lake metadata through explicit requests. The [Positron extension](https://github.com/dataraft-r/dataraft-positron) turns those responses into visual views; the bridge itself does not publish a product.

[`dataraft` overview](https://github.com/dataraft-r/dataraft) · [IDE reference](https://dataraft-r.github.io/dataraft/packages/dataraft.ide/)

## Start in R

```r
workspace <- new.env(parent = emptyenv())
workspace$orders <- dataraft.core::dr_product(
  "orders", data.frame(id = 1L, amount = 25)
)
context <- dataraft.ide::ide_context(workspace)
```

The context identifies objects the client may inspect. The extension sends explicit `ide_request()` calls; it requires an R session you select in Positron. DuckDB and a lake are optional for workspace-only inspection. Data cells are not included in the metadata channel; an explicit View action opens a bounded view inside R.

Install the development package with `pak::pak("dataraft-r/dataraft.ide")`. See the [workspace vignette](https://dataraft-r.github.io/dataraft/packages/dataraft.ide/) and [extension setup](https://github.com/dataraft-r/dataraft-positron#install).
