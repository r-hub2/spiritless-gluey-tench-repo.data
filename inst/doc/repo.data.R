## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
options(repos = c("@CRAN@" = "https://CRAN.R-project.org"))
library(repo.data)

## ----package_dependencies-----------------------------------------------------
pd <- package_dependencies("ggeasy")
head(pd)

## ----update_dependencies------------------------------------------------------
# Discover the requirements that can be upgraded
update_dependencies("ggeasy")

## ----package_date-------------------------------------------------------------
package_date("ggeasy")

## -----------------------------------------------------------------------------
alias <- cran_alias(c("fect", "gsynth"))
dup_alias <- duplicated_alias(alias)
head(dup_alias)

## -----------------------------------------------------------------------------
pkg <- "BaseSet"
head(cran_help_pages_wo_links(pkg))
head(cran_help_pages_not_linked(pkg))

## ----eval=requireNamespace("igraph", quietly = TRUE)--------------------------
cliques <- cran_help_cliques(pkg)
# Number of help pages connected
table(cliques$n) 

## -----------------------------------------------------------------------------
cran_help_pages_links_wo_deps(pkg)

## -----------------------------------------------------------------------------
cs <- cran_snapshot(as.Date("2020-01-31"))
nrow(cs)

## ----cran_sessions------------------------------------------------------------
cran_session()

## ----cran_date----------------------------------------------------------------
versions <- data.frame(Package = c("dplyr", "Rcpp", "rlang"),
                       Version = c("1.1.4", "0.8.9", NA))
cran_date(versions)

## -----------------------------------------------------------------------------
ip <- cran_date(installed.packages())
ip

## ----doom---------------------------------------------------------------------
cd <- cran_doom(bioc = TRUE)
cd[c("time_till_last", "last_archived", "npackages")]
knitr::kable(head(cd$details))

## ----sessions-----------------------------------------------------------------
sessionInfo()

