## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = ""
)
source('vignette-common.R')

## ----eval = TRUE, echo = TRUE-------------------------------------------------
dt <- wyz.code.rdoc::opRdocInformation()

## ----eval = TRUE, echo = TRUE-------------------------------------------------
sort(dt[stratum == 'CORE' & nature == 'EXPORTED']$name)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
sort(dt[stratum == 'LAYER_1' & nature == 'EXPORTED']$name)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
sort(dt[stratum == 'LAYER_2' & nature == 'EXPORTED']$name)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
sort(dt[stratum == 'LAYER_3' & nature == 'EXPORTED']$name)

