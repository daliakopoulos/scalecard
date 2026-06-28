#' @export
print.scalecard_detection <- function(x, ...) {
  cat("<scalecard detection>\n")
  cat(sprintf("  image      : %d x %d px\n", x$dim[["width"]], x$dim[["height"]]))
  cat(sprintf("  scale      : ~%.2f px/mm\n", x$ppm))
  cat(sprintf("  crosshairs : %d  (corner / horiz / vert)\n", nrow(x$crosshairs)))
  cat(sprintf("  patches    : %d  bar squares: %d\n", length(x$patches), length(x$bars)))
  found <- vapply(x$swatches, function(s) !any(is.na(s$rgb)), logical(1))
  cat(sprintf("  swatches   : %d / %d measured\n", sum(found), length(x$swatches)))
  np <- 3 + length(x$patches) + length(x$bars)
  cat(sprintf("  control pts: %d  (%s fit)\n", np, if (np >= 4) "perspective" else "affine"))
  invisible(x)
}

#' @export
plot.scalecard_detection <- function(x, image = NULL, ...) {
  if (is.null(image))
    stop("Pass the source image: plot(detection, image = \"photo.jpg\").", call. = FALSE)
  im <- .load_image(image)
  info <- magick::image_info(im); W <- info$width; Hh <- info$height
  op <- graphics::par(mar = c(0, 0, 0, 0)); on.exit(graphics::par(op))
  graphics::plot(NA, xlim = c(0, W), ylim = c(Hh, 0), xaxs = "i", yaxs = "i",
                 asp = 1, axes = FALSE, xlab = "", ylab = "")
  graphics::rasterImage(grDevices::as.raster(im), 0, Hh, W, 0)
  # perspective-correct card outline (follows the real skew), falling back to the
  # min-area rectangle if the full homography was unavailable
  ol <- if (!is.null(x$outline)) x$outline else x$quad
  if (!is.null(ol)) {
    q <- rbind(ol, ol[1, ])
    graphics::lines(q[, 1], q[, 2], col = "#4da3ff", lwd = 2)
  }
  # the 50 x 20 mm rectification frame: horiz -- corner -- vert
  cr <- x$crosshairs
  graphics::lines(cr[c(2, 1, 3), 1], cr[c(2, 1, 3), 2], col = "#37c871", lwd = 2)
  for (p in x$patches)
    graphics::points(p$x, p$y, pch = 19, col = "#ff3bd0", cex = 1.3)
  for (b in x$bars)
    graphics::points(b$x, b$y, pch = 15, col = "#00d0ff", cex = 1.2)
  graphics::points(cr[, 1], cr[, 2], pch = 3, col = "#ffd166", cex = 2.5, lwd = 2)
  graphics::text(cr[, 1], cr[, 2], c("corner", "50mm", "20mm"), pos = 4,
                 col = "#ffd166", cex = 1.1)
  invisible(x)
}

#' @export
print.scalecard_rectified <- function(x, ...) {
  oi <- magick::image_info(x$image)
  cat("<scalecard rectified>\n")
  cat(sprintf("  output  : %d x %d px\n", oi$width, oi$height))
  cat(sprintf("  scale   : %g px/mm (%.4f mm/px)\n", x$px_per_mm, x$mm_per_px))
  cat(sprintf("  fit     : %s from %d control points\n", x$method, x$n_points))
  if (!is.na(x$rms_mm)) cat(sprintf("  RMS err : %.3f mm\n", x$rms_mm))
  invisible(x)
}

#' @export
plot.scalecard_rectified <- function(x, grid = FALSE, ...) {
  img <- if (grid && !is.null(x$grid)) x$grid else x$image
  print(img)
  invisible(x)
}
