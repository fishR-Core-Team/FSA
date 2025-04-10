#' @title DEFUNCT functions.
#' @description These functions were once part of FSA but have now been removed.
#' @param \dots Additional arguments to pass through.
#' @name FSA-defunct

#' @rdname FSA-defunct
#' @export
bootCase <- function(...) {
  .Defunct(msg="'bootCase' has been removed; use 'car::Boot' with 'match='case'' (the default) for the same functionality.")
}

#' @rdname FSA-defunct
#' @export
chooseColors <- function(...) {
  .Defunct(msg="'chooseColors' has been removed; use one of many other available resources to choose appropriate colors.")
}

#' @rdname FSA-defunct
#' @export
compIntercepts <- function(...) {
  .Defunct(msg="'compIntercepts' has been removed; use 'emmeans' from the 'emmeans' package as described in the fishR blog post of 12-May-2021.")
}

#' @rdname FSA-defunct
#' @export
compSlopes <- function(...) {
  .Defunct(msg="'compSlopes' has been removed; use 'emtrends' from the 'emmeans' package as described in the fishR blog post of 11-May-2021.")
}

#' @rdname FSA-defunct
#' @export
diags <- function(...) {
  .Defunct(msg="'diags' has been removed (to 'FSAmisc' on GitHub).")
}

#' @rdname FSA-defunct
#' @export
filterD <- function(...) {
  .Defunct(msg="'filter' has been removed (to 'FSAmisc' on GitHub); please use 'droplevels' after 'subset' or 'dplyr::filter' for the same result (see fishR post from 26-May-2021).")
}

#' @rdname FSA-defunct
#' @export
fitPlot <- function(...) {
  .Defunct(msg="'fitPlot' has been removed (to 'FSAmisc' on GitHub).")
}

#' @rdname FSA-defunct
#' @export
fsaNews <- FSANews <- function(...) {
  .Defunct(msg="'fsaNews' and 'FSANews' have been removed (to 'FSAmisc' on GitHub).")
}

#' @rdname FSA-defunct
#' @export
hoCoef <- function(...) {
  .Defunct(msg="'hoCoef' has been removed (to 'FSAmisc' on GitHub).")
}

#' @rdname FSA-defunct
#' @export
mapvalues <- function(...) {
  .Defunct(msg="'FSA::mapvalues' has been removed; use 'plyr::mapvalues' instead for the same functionality.")
}

#' @rdname FSA-defunct
#' @export
plotBinResp <- function(...) {
  .Defunct(msg="'plotBinResp' has been removed; use 'ggplot2' as described in the fishR blog post of 25-May-2021.")
}

#' @rdname FSA-defunct
#' @export
residPlot <- function(...) {
  .Defunct(msg="'residPlot' has been removed (to 'FSAmisc' on GitHub).")
}

#' @rdname FSA-defunct
#' @export
Subset <- function(...) {
  .Defunct(msg="'Subset' has been removed; use 'subset' or 'dplyr::filter' followed by 'droplevels' for the same functionality.")
}

#' @rdname FSA-defunct
#' @export
Schnute <- function(...) {
  .Defunct(msg="'Schnute' has been removed; use 'Schnute <- makeGrowthFun(\"Schnute\")'.")
}

#' @rdname FSA-defunct
#' @export
SchnuteRichards <- function(...) {
  .Defunct(msg="'SchnuteRichards' has been removed; use 'SchnuteRichards <- makeGrowthFun(\"Schnute-Richards\")'.")
}
