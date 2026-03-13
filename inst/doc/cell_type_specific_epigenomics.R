## ----setup, echo=FALSE, include=FALSE-----------------------------------------
pkg <- read.dcf("../DESCRIPTION", fields = "Package")[1]
library(pkg, character.only = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# dat <- echodata::BST1
# histo_out <- echoannot::NOTT2019_epigenomic_histograms(dat = dat)

## ----eval=FALSE---------------------------------------------------------------
# knitr::kable(head(histo_out$data$raw))
# knitr::kable(head(histo_out$data$peaks))

## ----Session Info-------------------------------------------------------------
utils::sessionInfo()

