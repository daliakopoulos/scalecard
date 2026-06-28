#' scalecard: rectify and scale photos with the Credit Card Photography Scale
#'
#' Perspective-corrects and scales photographs taken with the Past Horizons
#' Credit Card Photography Scale calibration card. The card's crosshair targets,
#' colour patches and bar squares are detected automatically (\code{\link{detect_card}}),
#' or placed by hand (\code{\link{manual_targets}}), and used as ground-control
#' points by \code{\link{rectify_card}} to flatten the image at a known
#' pixels-per-millimetre scale, with optional \code{\link{white_balance}} and
#' \code{\link{color_correct}}.
#'
#' @keywords internal
#' @importFrom grDevices as.raster chull dev.cur dev.new dev.off
#' @importFrom stats median
"_PACKAGE"
