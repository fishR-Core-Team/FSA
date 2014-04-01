#' Fisheries stock assessment methods and data.
#' 
#' Functions to support basic fisheries stock assessment methods.
#'
#' This package can be used to peform a variety of basic fisheries stock assessment
#' methods.  Some detailed vignettes are available on the fishR website
#' (\url{http://fishr.wordpress.com/}), which can be viewed with \code{fishR("general")}.
#' Additionally, vignettes for the boxed examples in the \dQuote{Analysis and Interpretation
#' of Freshwater Fisheries Data} can be viewed with \code{fishR("AIFFD")}.
#' 
#' Additional data sets are in the FSAdata package (\url{http://www.rforge.net/FSAdata})
#' and simulations useful for teaching concepts are in the FSATeaching package
#' (\url{http://www.rforge.net/FSATeach}).
#'
#' @importFrom car bootCase outlierTest
#' @importFrom gdata drop.levels nobs
#' @importFrom gplots rich.colors
#' @importFrom Hmisc binconf
#' @importFrom knitr purl
#' @importFrom multcomp cld
#' @importFrom nlme groupedData nfGroupedData nmGroupedData getGroupsFormula
#' @importFrom plotrix plotCI thigmophobe
#' @importFrom quantreg rq
#' @importFrom sciplot lineplot.CI se
#' 
#' @docType package
#' @name FSA
NULL
