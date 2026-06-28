#' Rectify and scale a photograph using the calibration card
#'
#' The main entry point. Detects the Credit Card Photography Scale (unless you
#' pass a pre-computed \code{detection}), builds a projective transform from all
#' the card's ground-control points (3 crosshairs + colour patches + bar
#' squares), and warps the photo flat at a known pixels-per-millimetre scale.
#' Optionally white-balances or colour-corrects off the card, draws an mm grid,
#' and writes a PNG.
#'
#' @param image A file path or a \code{magick-image}.
#' @param px_per_mm Output resolution. Default 10 (so 0.1 mm/px), as in the
#'   register_*.R scripts.
#' @param white_balance Logical; gray-world white-balance off the card. Default
#'   \code{TRUE}. Ignored if \code{color_correct} succeeds.
#' @param color_correct Logical; match the card's reference colours (needs >= 4
#'   swatches). Default \code{FALSE}.
#' @param grid Logical; also return/write a copy with a 10/50 mm grid. Default
#'   \code{FALSE}.
#' @param max_px Safety cap on the output's long edge (px). The full scene is
#'   always kept (never cropped); for steeply-angled photos the rectified plane
#'   can balloon, so if the full extent would exceed \code{max_px} the effective
#'   \code{px_per_mm} is reduced to fit. Default 12000 (as in the web app).
#' @param trim_stretch Logical; blank to white the far-field regions that are
#'   stretched beyond \code{max_stretch} times the card's resolution (the smeared
#'   streaks toward the horizon), instead of rendering the smear. Default
#'   \code{TRUE}. Only affects perspective fits.
#' @param max_stretch Linear stretch factor (relative to the card) above which a
#'   region is blanked when \code{trim_stretch = TRUE}. Default 4.
#' @param output Optional file path. If given, the rectified PNG is written
#'   there (and a \code{*_grid.png} alongside it when \code{grid = TRUE}).
#' @param detection Optional pre-computed \code{"scalecard_detection"} (from
#'   \code{\link{detect_card}} or \code{\link{manual_targets}}). If \code{NULL},
#'   \code{detect_card()} is called.
#' @param verbose Print progress and a scale report.
#' @return An object of class \code{"scalecard_rectified"}: a list with
#'   \code{image} (rectified \code{magick-image}), \code{grid} (gridded image or
#'   \code{NULL}), \code{px_per_mm}, \code{mm_per_px}, \code{origin} (output px of
#'   the card's mm origin), \code{homography}, \code{rms_mm} (registration RMS for
#'   >= 4 points), \code{n_points}, and \code{detection}.
#' @export
#' @examples
#' \dontrun{
#' # one call: detect + rectify + white-balance, write a PNG
#' rec <- rectify_card("photo.jpg", output = "photo_rectified.png")
#' rec$px_per_mm
#'
#' # colour-correct as well, and a grid copy for QC
#' rectify_card("photo.jpg", color_correct = TRUE, grid = TRUE,
#'              output = "photo_rectified.png")
#' }
rectify_card <- function(image, px_per_mm = 10,
                         white_balance = TRUE, color_correct = FALSE,
                         grid = FALSE, trim_stretch = TRUE, max_stretch = 4,
                         max_px = 12000, output = NULL,
                         detection = NULL, verbose = TRUE) {
  im <- .load_image(image)
  if (is.null(detection)) detection <- detect_card(im, verbose = verbose)
  if (is.null(detection))
    stop("No card detected. Supply `detection = manual_targets(image)`.", call. = FALSE)
  spec <- detection$spec

  # assemble all source (px) <-> destination (mm) control points
  src <- detection$crosshairs
  mm  <- spec$tgt
  for (p in detection$patches) { src <- rbind(src, c(p$x, p$y)); mm <- rbind(mm, p$mm) }
  for (b in detection$bars)    { src <- rbind(src, c(b$x, b$y)); mm <- rbind(mm, b$mm) }
  n <- nrow(src)

  method <- if (n >= 4) "perspective" else "affine"
  dst <- cbind(mm[, 1] * px_per_mm, mm[, 2] * px_per_mm)

  # registration error + the homography we reproduce to locate the mm origin
  # Build the mm->px maps; size the canvas to the FULL warped scene (never crop).
  # The four image corners bound the rectified content; if that extent exceeds
  # max_px the resolution is reduced so steep shots don't balloon.
  info <- magick::image_info(im); W <- info$width; Hh <- info$height
  corners <- rbind(c(0, 0), c(W, 0), c(W, Hh), c(0, Hh))
  fit_maps <- function(ppm) {
    d <- cbind(mm[, 1] * ppm, mm[, 2] * ppm)
    if (method == "perspective") {
      Hm <- .fit_homography(src, d); fw <- function(p) .apply_h(Hm, p)
    } else {
      Hm <- NULL; P <- cbind(src, 1)
      ab <- cbind(qr.solve(P, d[, 1]), qr.solve(P, d[, 2])); fw <- function(p) cbind(p, 1) %*% ab
    }
    list(dst = d, H = Hm, fwd = fw, tc = fw(corners))
  }
  m <- fit_maps(px_per_mm)
  span <- c(max(m$tc[, 1]) - min(m$tc[, 1]), max(m$tc[, 2]) - min(m$tc[, 2]))
  capped <- FALSE
  if (max(span) > max_px) {
    px_per_mm <- px_per_mm * max_px / max(span); capped <- TRUE
    m <- fit_maps(px_per_mm)
  }
  dst <- m$dst; H <- m$H; fwd <- m$fwd
  minX <- min(m$tc[, 1]); minY <- min(m$tc[, 2])

  rms_mm <- NA_real_
  if (method == "perspective") {
    res <- sqrt(rowSums((.apply_h(H, src) - dst)^2)) / px_per_mm
    rms_mm <- sqrt(mean(res^2))
  }

  # warp with magick; bestfit keeps every mapped pixel (full scene, no crop).
  # image_repage() bakes bestfit's virtual-canvas offset into real pixels --
  # without it, imager (used by the colour steps) misreads the canvas and the
  # content comes out displaced/squished.
  coords <- as.numeric(t(cbind(src, dst)))
  rect <- magick::image_repage(magick::image_distort(
    magick::image_background(im, "white"),
    distortion = method, coordinates = coords, bestfit = TRUE))

  mm2px <- function(m) cbind(m[, 1] * px_per_mm - minX, m[, 2] * px_per_mm - minY)
  origin <- as.numeric(mm2px(matrix(c(0, 0), 1)))

  # colour adjustments (on the flat output)
  cc_done <- FALSE
  if (color_correct) {
    before <- rect; rect <- .color_correct(rect, detection)
    cc_done <- !identical(rect, before)
  }
  if (!cc_done && white_balance) {
    cw <- spec$card[["width"]]; chh <- spec$card[["height"]]
    roi <- round(c(origin[1], origin[2], cw * px_per_mm, chh * px_per_mm))
    rect <- .white_balance(rect, roi = roi)
  }

  # white out everything that isn't real photo content (out-of-frame edge smears)
  # plus any over-stretched far field
  if (trim_stretch && method == "perspective") {
    Tm <- matrix(c(1, 0, 0, 0, 1, 0, -minX, -minY, 1), 3, 3)   # translate by (-minX,-minY)
    M2 <- Tm %*% H                                             # source px -> output px
    rect <- .trim_stretch(rect, M2, colMeans(src), c(W, Hh), max_stretch)
  }

  gimg <- if (grid) .draw_mm_grid(rect, px_per_mm, origin) else NULL

  if (!is.null(output)) {
    magick::image_write(rect, output)
    if (grid) {
      gpath <- sub("\\.(png|jpg|jpeg|tif|tiff)$", "_grid.png", output, ignore.case = TRUE)
      if (identical(gpath, output)) gpath <- paste0(output, "_grid.png")
      magick::image_write(gimg, gpath)
    }
  }

  out <- list(image = rect, grid = gimg, px_per_mm = px_per_mm,
              mm_per_px = 1 / px_per_mm, origin = origin, homography = H,
              rms_mm = rms_mm, n_points = n, method = method, capped = capped,
              detection = detection)
  class(out) <- "scalecard_rectified"
  if (verbose) {
    oi <- magick::image_info(rect)
    cat(sprintf("Rectified: %d x %d px, %g px/mm (%.4f mm/px), %s from %d points.\n",
                oi$width, oi$height, round(px_per_mm, 3), 1 / px_per_mm, method, n))
    if (!is.na(rms_mm)) cat(sprintf("  registration RMS error: %.3f mm\n", rms_mm))
    if (capped) cat(sprintf("  full scene kept; resolution reduced to fit max_px (%d).\n", max_px))
    if (!is.null(output)) cat(sprintf("  wrote: %s\n", output))
  }
  out
}

# Replace with white the output pixels that don't come from real photo content:
# anything mapping OUTSIDE the original image (otherwise magick smears the edge
# pixels into long streaks), plus any far-field region stretched beyond
# `max_stretch` x the card's resolution. For a homography source->output (M2),
# linear stretch vs the card is (w_card / w)^1.5 where w is the homogeneous
# denominator. A coarse grid of output pixels is mapped back to source to build
# the keep-mask, which is then used as the alpha channel over a white background.
.trim_stretch <- function(rect, M2, src_center, src_dim, max_stretch) {
  w_card <- M2[3, 1] * src_center[1] + M2[3, 2] * src_center[2] + M2[3, 3]
  if (!is.finite(w_card) || w_card == 0) return(rect)
  M2inv <- tryCatch(solve(M2), error = function(e) NULL)
  if (is.null(M2inv)) return(rect)
  ci <- imager::magick2cimg(rect)                      # rect is repaged -> reads correctly
  a <- as.array(ci); outW <- dim(a)[1]; outH <- dim(a)[2]; nc <- dim(a)[4]
  uu <- rep.int(seq_len(outW), outH); vv <- rep(seq_len(outH), each = outW)
  hh <- cbind(uu, vv, 1) %*% t(M2inv)
  xs <- hh[, 1] / hh[, 3]; ys <- hh[, 2] / hh[, 3]
  wf <- M2[3, 1] * xs + M2[3, 2] * ys + M2[3, 3]
  r_min <- max_stretch^(-2 / 3)
  bad <- !(xs >= 0 & xs <= src_dim[1] & ys >= 0 & ys <= src_dim[2] &
           (wf / w_card) >= r_min)                     # out of frame, or over-stretched
  dim(bad) <- c(outW, outH)
  for (c in seq_len(min(3, nc))) { ch <- a[, , 1, c]; ch[bad] <- 1; a[, , 1, c] <- ch }
  if (nc >= 4) { ch <- a[, , 1, 4]; ch[bad] <- 1; a[, , 1, 4] <- ch }
  .cimg_to_magick(imager::as.cimg(a))
}

#' Convert a pixel measurement on a rectified image to millimetres
#'
#' @param rectified A \code{"scalecard_rectified"} object.
#' @param pixels A numeric distance in pixels measured on the rectified image.
#' @return The distance in millimetres.
#' @export
measure_mm <- function(rectified, pixels) pixels * rectified$mm_per_px

# draw a 10 mm (green) / 50 mm (red) grid aligned to the mm origin, like
# register_abfo.R's QC grid.
.draw_mm_grid <- function(rect, ppm, origin) {
  oi <- magick::image_info(rect); Wg <- oi$width; Hg <- oi$height
  g <- magick::image_draw(rect)
  step <- 10 * ppm; mx0 <- origin[1]; my0 <- origin[2]
  for (k in ceiling(-mx0 / step):floor((Wg - mx0) / step)) {
    x <- mx0 + k * step; m <- k * 10; big <- (m %% 50 == 0)
    graphics::segments(x, 0, x, Hg, col = if (big) "#FF0000C0" else "#00FF0070",
                       lwd = if (big) 2 else 1)
    if (big) graphics::text(x + 2, 16, m, col = "red", cex = 1, adj = 0)
  }
  for (k in ceiling(-my0 / step):floor((Hg - my0) / step)) {
    y <- my0 + k * step; m <- k * 10; big <- (m %% 50 == 0)
    graphics::segments(0, y, Wg, y, col = if (big) "#FF0000C0" else "#00FF0070",
                       lwd = if (big) 2 else 1)
    if (big) graphics::text(2, y - 4, m, col = "red", cex = 1, adj = 0)
  }
  grDevices::dev.off()
  g
}
