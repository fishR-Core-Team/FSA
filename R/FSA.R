#' @title Fisheries stock assessment methods and data.
#' 
#' @description Functions to support basic fisheries stock assessment methods.
#'
#' @details Functions from this package can be used to peform a variety of basic fisheries stock assessment methods.  Detailed vignettes are available on the \href{http://fishr.wordpress.com/}{fishR website}, which can be accessed with \code{fishR("general")}.  Vignettes for the boxed examples in the \dQuote{Analysis and Interpretation of Freshwater Fisheries Data} book can be viewed with \code{fishR("AIFFD")}.
#' 
#' Questions, comments, or suggestions should be given on the \href{https://github.com/droglenc/FSA/issues}{GitHub FSA Issues page}.
#' 
#' Packages with related functionality by the same author are
#' \itemize{
#'   \item The \href{http://fishr.wordpress.com/fsa/fsadata/}{FSAdata package} contains additional data sets.
#'   \item The \href{https://github.com/droglenc/FSAsim}{FSAsim package} simulation routines for various fisheries methods.
#'   \item The \href{https://github.com/droglenc/FSAWs}{FSAWs package} contains functions for developing and validating standard weight equations.
#' }
#'
#' @docType package
#' 
#' @name FSA
#' 
#' @importFrom car bootCase outlierTest
#' @importFrom gdata drop.levels nobs
#' @importFrom gplots rich.colors
#' @importFrom Hmisc binconf
#' @importFrom knitr purl
#' @importFrom lmtest lrtest
#' @importFrom multcomp cld
#' @importFrom plotrix plotCI stackpoly thigmophobe
#' @importFrom relax gslider
#' @importFrom sciplot lineplot.CI se
#' 
NULL
