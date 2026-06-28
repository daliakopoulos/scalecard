#' Geometry of the Credit Card Photography Scale
#'
#' Returns the fixed, measured geometry of the Past Horizons "Credit Card
#' Photography Scale" calibration card: its outer size, the three crosshair
#' rectification targets (on a precise 50 x 20 mm frame), the red/yellow/blue
#' colour-patch centres, the 8 cm scale-bar squares, and the reference colours
#' of the six calibration swatches. All millimetre coordinates are measured from
#' the card's top-left corner, X to the right, Y downwards.
#'
#' Hue/Saturation/Value test functions use the OpenCV convention
#' (H in 0..180, S and V in 0..255) so they can be applied to the output of
#' \code{\link{rgb_to_hsv_cv}}. Each test is vectorised: it accepts matrices
#' \code{H}, \code{S}, \code{V} and returns a logical matrix.
#'
#' @return A list of class \code{"scalecard_spec"} with elements \code{card}
#'   (width, height mm), \code{tgt} (3x2 crosshair mm coords: corner, 50 mm arm,
#'   20 mm arm), \code{tgt_r_mm} (target ring outer radius mm), \code{patch}
#'   (red/yellow/blue centres + HSV tests), \code{bar} (2x2 mm coords of the bar
#'   squares), and \code{swatch} (six reference colours + HSV tests).
#' @export
#' @examples
#' sp <- card_spec()
#' sp$card        # 85.6 54
#' sp$tgt         # the three crosshair targets, in mm
card_spec <- function() {
  spec <- list(
    card     = c(width = 85.6, height = 54),   # card border, mm (fixed)
    margin_mm = 60,                            # scene kept around the card (output)

    # crosshair targets, mm from card top-left (measured from the design):
    #   corner(BR) at (77, 34.3); horizontal arm 50 mm left; vertical arm 20 mm up
    tgt = rbind(
      corner = c(x = 77.0, y = 34.3),   # 1: the L vertex
      horiz  = c(x = 27.0, y = 34.3),   # 2: 50 mm along the horizontal arm
      vert   = c(x = 77.0, y = 14.3)    # 3: 20 mm along the vertical arm
    ),
    tgt_r_mm = 4.8,                     # target ring outer radius, mm

    # colour patches (point centres), mm from card top-left + an HSV test to find
    # each on the card. Green is skipped here (it clashes with common backgrounds);
    # the corner patches (red/blue) can be clipped by the rounded card corners, so
    # the homography's least-squares fit averages out any small offset.
    patch = list(
      list(name = "red",    mm = c(12.05, 46.09),
           test = function(H, S, V) (H < 10 | H > 170) & S > 110 & V > 80),
      list(name = "yellow", mm = c(31.96, 46.05),
           test = function(H, S, V) H > 18 & H < 35 & S > 110 & V > 80),
      list(name = "blue",   mm = c(72.11, 46.09),
           test = function(H, S, V) H > 100 & H < 135 & S > 90 & V > 60)
    ),

    # black squares of the 8 cm bar (top of card) -> extra TOP control points
    bar = rbind(c(47.11, 4.75), c(67.04, 4.75)),

    # colour-correction swatches: reference RGB measured from the card design,
    # plus an HSV test used to locate each swatch on the photographed card.
    swatch = list(
      list(name = "red",    rgb = c(217, 17, 35),
           test = function(H, S, V) (H < 10 | H > 170) & S > 110 & V > 80),
      list(name = "yellow", rgb = c(253, 215, 5),
           test = function(H, S, V) H > 18 & H < 35 & S > 110 & V > 80),
      list(name = "green",  rgb = c(2, 132, 66),
           test = function(H, S, V) H > 40 & H < 90 & S > 90 & V > 60),
      list(name = "blue",   rgb = c(41, 54, 106),
           test = function(H, S, V) H > 100 & H < 140 & S > 90 & V > 60),
      list(name = "white",  rgb = c(211, 208, 201),
           test = function(H, S, V) S < 35 & V > 180),
      list(name = "black",  rgb = c(28, 28, 26),
           test = function(H, S, V) V < 70)
    )
  )
  class(spec) <- "scalecard_spec"
  spec
}
