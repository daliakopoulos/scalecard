# Core white balance (internal; the exported wrapper is below). Kept separate so
# rectify_card()'s `white_balance`/`color_correct` arguments don't shadow it.
.white_balance <- function(image, roi = NULL) {
  image <- magick::image_repage(image)   # bake any bestfit canvas offset (see rectify)
  ci <- imager::magick2cimg(image)
  a  <- as.array(ci)                          # (W, H, 1, C), 0..1
  W <- dim(a)[1]; Hh <- dim(a)[2]; nc <- dim(a)[4]
  if (nc < 3) return(image)
  rgb <- a[, , 1, 1:3, drop = FALSE]; dim(rgb) <- c(W, Hh, 3)
  if (is.null(roi)) roi <- c(1, 1, W, Hh)
  x0 <- max(1, roi[1]); y0 <- max(1, roi[2])
  x1 <- min(W, roi[1] + roi[3] - 1); y1 <- min(Hh, roi[2] + roi[4] - 1)
  if (x1 - x0 < 5 || y1 - y0 < 5) return(image)
  sub <- rgb[x0:x1, y0:y1, , drop = FALSE] * 255
  hsv <- rgb_to_hsv_cv(sub)
  white <- hsv$S < 45 & hsv$V > 170
  if (sum(white) < 50) return(image)
  means <- c(mean(sub[, , 1][white]), mean(sub[, , 2][white]), mean(sub[, , 3][white]))
  if (any(means < 1)) return(image)
  gains <- mean(means) / means
  for (c in 1:3) a[, , 1, c] <- pmin(1, a[, , 1, c] * gains[c])
  .cimg_to_magick(imager::as.cimg(a))
}

.color_correct <- function(image, detection) {
  sw <- detection$swatches
  ok <- Filter(function(s) !any(is.na(s$rgb)), sw)
  if (length(ok) < 4) {
    message("Colour-correct skipped: fewer than 4 swatches were measured.")
    return(image)
  }
  X <- t(vapply(ok, function(s) c(s$rgb, 1), numeric(4)))   # n x 4 (measured + 1)
  Y <- t(vapply(ok, function(s) s$ref, numeric(3)))         # n x 3 (reference)
  A <- qr.solve(X, Y)                                       # 4 x 3
  image <- magick::image_repage(image)   # bake any bestfit canvas offset (see rectify)
  ci <- imager::magick2cimg(image)
  a  <- as.array(ci); nc <- dim(a)[4]
  if (nc < 3) return(image)
  rgb <- cbind(as.vector(a[, , 1, 1]), as.vector(a[, , 1, 2]),
               as.vector(a[, , 1, 3])) * 255                # (W*Hh) x 3
  out <- cbind(rgb, 1) %*% A                                # (W*Hh) x 3, 0..255
  out[out < 0] <- 0; out[out > 255] <- 255
  for (c in 1:3) a[, , 1, c] <- out[, c] / 255              # vector fills column-major
  .cimg_to_magick(imager::as.cimg(a))
}

#' White-balance an image off the card's near-white pixels
#'
#' Gray-world white balance computed from the bright, low-saturation pixels
#' inside a region (normally the rectified card). Mirrors the web app's
#' \code{whiteBalance()}.
#'
#' @param image A \code{magick-image} (typically the rectified output).
#' @param roi Optional integer \code{c(x, y, w, h)} pixel rectangle to sample the
#'   white reference from. Defaults to the whole image.
#' @return A white-balanced \code{magick-image}. If no clear white is found the
#'   input is returned unchanged.
#' @export
white_balance <- function(image, roi = NULL) .white_balance(image, roi)

#' Colour-correct an image to the card's reference swatches
#'
#' Fits a 3x4 affine colour transform (measured swatch RGB -> reference swatch
#' RGB, least squares over all swatches that were located) and applies it to the
#' whole image. Mirrors the web app's \code{applyColorCorrect()}. Needs at least
#' four swatches measured by \code{\link{detect_card}}.
#'
#' @param image A \code{magick-image}.
#' @param detection A \code{"scalecard_detection"} (its \code{swatches} supply
#'   the measured-vs-reference colour pairs).
#' @return A colour-corrected \code{magick-image}, or the input unchanged (with a
#'   message) if fewer than four swatches are available.
#' @export
color_correct <- function(image, detection) .color_correct(image, detection)
