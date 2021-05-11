#' @title Defunct functions.
#' @description These functions were once part of FSA but have now been removed.
#' @name FSA-defunct
#' @keywords internal
NULL

chooseColors <- function(...) {
  .Defunct(msg="'chooseColors' has been removed; use one of many other available resources to choose appropriate colors.")
}

compIntercepts <- function(...) {
  .Defunct(msg="'compIntercepts' has been removed; use 'emmeans' from the 'emmeans' package as described in the fishR blog post of 12-May-2021.")
}

compSlopes <- function(...) {
  .Defunct(msg="'compSlopes' has been removed; use 'emtrends' from the 'emmeans' package as described in the fishR blog post of 11-May-2021.")
}

diags <- function(...) {
  .Defunct(msg="'diags' has been removed (to 'FSAmisc' on GitHub).")
}

hoCoef <- function(...) {
  .Defunct(msg="'hoCoef' has been removed (to 'FSAmisc' on GitHub).")
}

mapvalues <- function(...) {
  .Defunct(msg="'FSA::mapvalues' has been removed; use 'plyr::mapvalues' instead for the same functionality.")
}

Subset <- function(...) {
  .Defunct(msg="'Subset' has been removed; use 'subset' or 'dplyr::filter' followed by 'droplevels' for the same functionality.")
}
