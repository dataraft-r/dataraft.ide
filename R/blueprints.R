#' Create a small data product starter project
#'
#' Creates runnable R definitions without opening a lake or publishing data.
#' Choose a governed table to add an explicit governance checklist. The caller
#' must review the example data, owner and policy fields before publishing.
#' Existing paths are never overwritten.
#' @param id Product identifier and new directory name. Letters, numbers and
#'   underscores only.
#' @param blueprint `table` or `governed-table`.
#' @param path Existing parent directory.
#' @return Created directory, invisibly.
#' @export
 dr_init_product <- function(id, blueprint = c("table", "governed-table"), path = ".") {
  blueprint <- match.arg(blueprint)
  if (!is.character(id) || length(id) != 1L || is.na(id) ||
      !grepl("^[A-Za-z][A-Za-z0-9_]*$", id)) {
    stop("id must start with a letter and use only letters, numbers and underscores.", call. = FALSE)
  }
  if (!is.character(path) || length(path) != 1L || is.na(path) || !dir.exists(path)) {
    stop("path must be an existing directory.", call. = FALSE)
  }
  destination <- file.path(path, id)
  if (file.exists(destination) || dir.exists(destination)) {
    stop("The product directory already exists.", call. = FALSE)
  }
  if (!dir.create(destination)) stop("Could not create product directory.", call. = FALSE)
  ok <- FALSE
  on.exit(if (!ok) unlink(destination, recursive = TRUE), add = TRUE)
  writeLines(c(
    sprintf('contract <- dataraft.core::dr_contract("%s",', id),
    '  columns = c(id = "integer"), key = "id", owner = "")',
    '# Review columns, key and owner before real publication.'
  ), file.path(destination, "contract.R"))
  writeLines(c(
    '# Load contract.R first. Replace the example input with your source adapter.',
    sprintf('product <- dataraft.core::dr_product("%s",', id),
    '  data.frame(id = 1L), contract = contract)'
  ), file.path(destination, "product.R"))
  writeLines(c(
    '# Load product.R first. Add business checks appropriate for your data.',
    'product <- dataraft.core::dr_add_quality(product, ~ !is.na(id))'
  ), file.path(destination, "quality.R"))
  writeLines(c(
    paste0('# ', id), '',
    'From this directory, run:', '',
    '```r',
    'source("contract.R")', 'source("product.R")', 'source("quality.R")',
    'dataraft.core::dr_run(product, write = FALSE)',
    '```', '',
    'Review the contract, source and quality rule before publishing.',
    if (blueprint == "governed-table") c('',
      '## Governance checklist', '',
      '- Set a real owner in contract.R.',
      '- Set classification and retention with dr_contract_meta().',
      '- Evaluate your organization policies before activation and publish.')
  ), file.path(destination, "README.md"))
  ok <- TRUE
  invisible(destination)
}
