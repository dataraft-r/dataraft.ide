\name{dr_init_product}
\alias{dr_init_product}
\title{Create a data product starter project}
\description{Writes a runnable table or governed table project without publishing data. Existing directories are never overwritten.}
\usage{dr_init_product(id, blueprint = c("table", "governed-table"), path = ".")}
\arguments{
\item{id}{Safe product ID and directory name.}
\item{blueprint}{Starter variant.}
\item{path}{Existing parent directory.}
}
\value{Created directory, invisibly.}
