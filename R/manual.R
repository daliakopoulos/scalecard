#' Place the crosshair targets by hand (interactive fallback)
#'
#' When \code{\link{detect_card}} fails (poor lighting, busy background, card too
#' small), open the photo in a native window and click roughly on the three
#' crosshair targets. A matched filter snaps each click to the true ring centre.
#' Returns a \code{"scalecard_detection"} you can pass straight to
#' \code{\link{rectify_card}}. With only the three crosshairs this gives an
#' \emph{affine} rectification (tilt/scale/shear, not perspective).
#'
#' Must be run \strong{interactively} (select + Ctrl+Enter, or paste into the
#' console) -- \code{locator()} needs a native graphics device, not the
#' RStudio/VSCode plot pane. Adapts the workflow in \code{register_abfo.R}.
#'
#' @param image A file path or a \code{magick-image}.
#' @param spec Card geometry; defaults to \code{\link{card_spec}()}.
#' @param refine Snap each rough click to the ring centre by template matching
#'   (default \code{TRUE}).
#' @return A \code{"scalecard_detection"} (with the 3 crosshairs; no patches,
#'   bars or swatches), or \code{NULL} if clicks were not captured.
#' @export
manual_targets <- function(image, spec = card_spec(), refine = TRUE) {
  im   <- .load_image(image)
  info <- magick::image_info(im); W <- info$width; Hh <- info$height
  ras  <- grDevices::as.raster(im)

  if (.Platform$OS.type == "windows")      grDevices::windows(width = 12, height = 12 * Hh / W)
  else if (capabilities("X11"))            grDevices::x11(width = 12, height = 12 * Hh / W)
  else                                     grDevices::dev.new(width = 12, height = 12 * Hh / W,
                                                              noRStudioGD = TRUE)
  on.exit(if (grDevices::dev.cur() > 1) grDevices::dev.off(), add = TRUE)
  op <- graphics::par(mar = c(0, 0, 0, 0)); on.exit(graphics::par(op), add = TRUE)
  graphics::plot(NA, xlim = c(0, W), ylim = c(Hh, 0), xaxs = "i", yaxs = "i",
                 asp = 1, axes = FALSE, xlab = "", ylab = "")
  graphics::rasterImage(ras, 0, Hh, W, 0)
  cat("Click ROUGHLY on the 3 crosshair targets, IN ORDER:\n",
      " 1 = CORNER target (the L vertex, bottom-right of the 50x20 frame)\n",
      " 2 = target on the HORIZONTAL arm (50 mm from the corner)\n",
      " 3 = target on the VERTICAL arm (20 mm from the corner)\n", sep = "")
  p <- graphics::locator(n = 3, type = "p", pch = 3, col = "red", cex = 2, lwd = 2)
  if (is.null(p) || length(p$x) < 3) {
    message("Did not capture 3 clicks. Run interactively (Ctrl+Enter / console), not source().")
    return(invisible(NULL))
  }
  cross <- cbind(x = p$x, y = p$y)

  # approximate scale from the clicks: 50 mm and 20 mm arms
  d_h <- sqrt(sum((cross[2, ] - cross[1, ])^2))
  d_v <- sqrt(sum((cross[3, ] - cross[1, ])^2))
  ppm <- mean(c(d_h / 50, d_v / 20))

  if (refine) {
    g <- .gray_cimg(im)
    R_out <- max(8, spec$tgt_r_mm * ppm); R_in <- R_out * 0.55
    for (i in 1:3) cross[i, ] <- .refine_crosshair(g, cross[i, 1], cross[i, 2], R_out, R_in)
  }
  dimnames(cross) <- list(c("corner", "horiz", "vert"), c("x", "y"))

  det <- list(crosshairs = cross, patches = list(), bars = list(),
              swatches = lapply(spec$swatch, function(s)
                list(name = s$name, rgb = c(NA, NA, NA), ref = s$rgb)),
              ppm = ppm, quad = NULL, dim = c(width = W, height = Hh), spec = spec)
  class(det) <- "scalecard_detection"
  det
}
