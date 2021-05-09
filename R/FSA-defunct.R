#' @title Defunct functions.
#' @description These functions were once part of FSA but have now been removed.
#' @name FSA-defunct
#' @keywords internal
NULL

chooseColors <- function(...) {
  .Defunct(msg="'chooseColors' has been removed; use one of many other available resources to choose appropriate colors.")
}

diags <- function(...) {
  .Defunct(msg="'diags' has been removed (to 'FSAmisc' on GitHub).")
}

diags <- function(...) {
  .Defunct(msg="'hoCoef' has been removed (to 'FSAmisc' on GitHub).")
}

mapvalues <- function(...) {
  .Defunct(msg="'FSA::mapvalues' has been removed; use 'plyr::mapvalues' instead for the same functionality.")
}

Subset <- function(...) {
  .Defunct(msg="'Subset' has been removed; use 'subset' or 'dplyr::filter' followed by 'droplevels' for the same functionality.")
}
