#' @title Fisheries stock assessment methods and data.
#' 
#' @description Functions to support basic fisheries stock assessment methods.
#'
#' @details Functions from this package can be used to peform a variety of basic fisheries stock assessment methods.  Detailed descriptions for most functions are available in the \href{https://fishr.wordpress.com/books/ifar/}{Introductory to Fisheries Analysis with R} book (Ogle 2016).  Vignettes for the boxed examples in the \dQuote{Analysis and Interpretation of Freshwater Fisheries Data} book can be viewed with \code{fishR("AIFFD")}.
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
#' @references Ogle, D.H.  2016.  Introductory Fisheries Analyses with R.  Chapman & Hall/CRC, Boca Raton, FL.
#' 
#' @docType package
#' 
#' @name FSA
#' 
#' @importFrom car bootCase outlierTest
#' @importFrom dplyr filter
#' @importFrom gdata drop.levels
#' @importFrom gplots rich.colors
#' @importFrom Hmisc binconf
#' @importFrom knitr purl
#' @importFrom lmtest lrtest
#' @importFrom multcomp cld
#' @importFrom plotrix plotCI stackpoly thigmophobe
#' @importFrom relax gslider
#' @importFrom sciplot lineplot.CI
NULL
