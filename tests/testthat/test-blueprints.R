test_that("blueprints create an isolated runnable starter", {
  parent <- withr::local_tempdir()
  directory <- dr_init_product("Customers", "governed-table", parent)
  expect_equal(sort(list.files(directory)), sort(c("contract.R", "product.R", "quality.R", "README.md")))
  expect_match(readLines(file.path(directory, "README.md")) |> paste(collapse = " "), "Governance checklist")
  expect_error(dr_init_product("Customers", path = parent), "already exists")
  expect_error(dr_init_product("../other", path = parent), "id must")
})
