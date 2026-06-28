#' Detect the calibration card and its ground-control points
#'
#' Locates the Credit Card Photography Scale in a photograph entirely
#' automatically: it finds the card by its red/yellow/blue colour signature,
#' places and ring-refines the three crosshair targets, finds the colour-patch
#' centres and the 8 cm bar squares, and measures the card's colour swatches (for
#' optional colour correction). The returned object is fed to
#' \code{\link{rectify_card}} (which calls this for you unless you pass a
#' pre-computed detection).
#'
#' This is the R port of the OpenCV.js \code{autoDetect()} routine, built on
#' \code{magick} + \code{imager} + \code{EBImage} instead of OpenCV.
#'
#' @param image A file path or a \code{magick-image}.
#' @param spec Card geometry; defaults to \code{\link{card_spec}()}.
#' @param max_dim Long-edge length (px) the image is downscaled to for the
#'   colour/segmentation search. Detection coordinates are mapped back to
#'   full resolution. Default 900 (as in the web app).
#' @param verbose Print progress, like the existing register_*.R scripts.
#' @return An object of class \code{"scalecard_detection"}: a list with
#'   \code{crosshairs} (3x2 full-res px, ordered corner/horiz/vert),
#'   \code{patches} and \code{bars} (extra control points with their mm coords),
#'   \code{swatches} (measured RGB per reference colour, \code{NA} if not found),
#'   \code{ppm} (approx px/mm), \code{quad} (card corners), \code{dim}, and
#'   \code{spec}. Returns \code{NULL} invisibly with a message if the card cannot
#'   be found (use \code{\link{manual_targets}} as a fallback).
#' @export
#' @examples
#' \dontrun{
#' det <- detect_card("photo.jpg")
#' plot(det)                 # overlay the detected points on the photo
#' rec <- rectify_card("photo.jpg", detection = det)
#' }
detect_card <- function(image, spec = card_spec(), max_dim = 900, verbose = TRUE) {
  im   <- .load_image(image)
  info <- magick::image_info(im); W0 <- info$width; H0 <- info$height
  scale <- min(max_dim / max(W0, H0), 1)
  sm <- if (scale < 1)
    magick::image_resize(im, paste0(round(W0 * scale), "x", round(H0 * scale), "!"))
  else im

  rgb <- .rgb_array(sm)
  W <- dim(rgb)[1]; H <- dim(rgb)[2]; N <- W * H
  hsv <- rgb_to_hsv_cv(rgb)
  Hm <- hsv$H; Sm <- hsv$S; Vm <- hsv$V

  # background hue = median hue around a border band
  bb <- max(6, round(0.04 * min(W, H)))
  bx <- (row(Hm) <= bb | row(Hm) > W - bb | col(Hm) <= bb | col(Hm) > H - bb)
  bgh <- stats::median(Hm[bx])

  # foreground (card) mask: anything whose hue differs from the background
  dh <- abs(Hm - bgh); dh[dh > 90] <- 180 - dh[dh > 90]
  card <- !(dh < 20 & Sm > 40)

  # colour masks used to recognise the card and place patch centres
  red <- (Hm < 10 | Hm > 170) & Sm > 110 & Vm > 80
  yel <- Hm > 18 & Hm < 35 & Sm > 110 & Vm > 80
  blu <- Hm > 105 & Hm < 135 & Sm > 90 & Vm > 60

  # clean up the card mask, then keep the blob carrying the R+Y+B signature
  card <- EBImage::opening(card * 1, EBImage::makeBrush(9, "disc")) > 0
  card <- EBImage::closing(card * 1, EBImage::makeBrush(21, "disc")) > 0
  lab <- EBImage::bwlabel(card * 1)
  nlab <- max(lab)
  if (nlab < 1) return(.detect_fail(verbose))
  cthr <- max(60, 0.0004 * N); amin <- 0.02 * N
  best <- -1L; bestA <- amin
  for (i in seq_len(nlab)) {
    fill <- lab == i
    a <- sum(fill)
    if (a <= amin) next
    if (sum(fill & red) > cthr && sum(fill & yel) > cthr &&
        sum(fill & blu) > cthr && a > bestA) { bestA <- a; best <- i }
  }
  if (best < 0) return(.detect_fail(verbose))
  cardFill <- lab == best

  cw <- spec$card[["width"]]; chh <- spec$card[["height"]]
  g <- .gray_cimg(im)

  # ---- patch centres (red / yellow / blue), restricted to the card.
  # These are detected by colour alone (no pose), so they are reliable and are
  # used below to resolve the card's orientation.
  colmask <- list(red = red, yellow = yel, blue = blu)
  patches <- list()
  for (k in seq_along(spec$patch)) {
    p <- spec$patch[[k]]
    m <- colmask[[p$name]] & cardFill
    m <- EBImage::opening(m * 1, EBImage::makeBrush(9, "disc")) > 0
    blob <- .largest_blob(m, min_area = 50)
    if (!is.null(blob))
      patches[[length(patches) + 1]] <-
        list(name = p$name, x = blob$x / scale, y = blob$y / scale, mm = p$mm)
  }

  # ---- card pose: fit the card's minimum-area rectangle (accurate; ignores the
  # rounded corners), then resolve which physical corner is the design's top-left
  # by trying all 4 rotations x 2 windings and keeping the ordering whose
  # mm->image homography best predicts the detected patches.
  ij <- which(cardFill, arr.ind = TRUE)
  quad0 <- .min_area_rect(ij[, 1], ij[, 2])
  if (is.null(quad0)) quad0 <- .extreme_quad(ij[, 1], ij[, 2])
  quad0 <- quad0 / scale                                       # full-res px
  card_mm <- rbind(c(0, 0), c(cw, 0), c(cw, chh), c(0, chh))   # design corners
  candidates <- c(
    lapply(0:3, function(r) ((0:3) + r) %% 4 + 1),             # rotations
    lapply(0:3, function(r) (((3:0) + r) %% 4) + 1)            # + reversed winding
  )
  Hpose <- NULL; quad <- quad0; best_err <- Inf
  if (length(patches) >= 2) {
    pimg <- do.call(rbind, lapply(patches, function(p) c(p$x, p$y)))
    pmm  <- do.call(rbind, lapply(patches, function(p) p$mm))
    for (perm in candidates) {
      dst <- quad0[perm, , drop = FALSE]
      Hc  <- tryCatch(.fit_homography(card_mm, dst), error = function(e) NULL)
      if (is.null(Hc)) next
      err <- mean(sqrt(rowSums((.apply_h(Hc, pmm) - pimg)^2)))
      if (err < best_err) { best_err <- err; Hpose <- Hc; quad <- dst }
    }
  }
  if (is.null(Hpose)) Hpose <- .fit_homography(card_mm, quad0)  # fallback
  ppm <- mean(c(sqrt(sum((quad[2, ] - quad[1, ])^2)) / cw,
                sqrt(sum((quad[3, ] - quad[2, ])^2)) / chh))

  # ---- crosshairs: refine in flat card space. The pose flattens the card so each
  # target sits at a known mm position with a canonical, upright appearance; a
  # box+ring+crosshair template located by normalised cross-correlation locks onto
  # the printed target sub-pixel, then the centre is mapped back to image px.
  guess <- .apply_h(Hpose, spec$tgt)
  G <- g[, , 1, 1]
  cross <- .refine_targets_flat(G, Hpose, spec$tgt, ppm)
  dimnames(cross) <- list(c("corner", "horiz", "vert"), c("x", "y"))

  # ---- 8 cm bar squares: predict from crosshairs+patches, snap to dark blobs
  bars <- list()
  if (length(patches) >= 2) {
    sa <- cross; da <- spec$tgt
    for (pp in patches) { sa <- rbind(sa, c(pp$x, pp$y)); da <- rbind(da, pp$mm) }
    Hpix2mm <- tryCatch(.fit_homography(sa, da), error = function(e) NULL)
    if (!is.null(Hpix2mm)) {
      Hmm2pix <- solve(Hpix2mm)
      hw <- round(6 * ppm)
      for (r in seq_len(nrow(spec$bar))) {
        e <- .apply_h(Hmm2pix, spec$bar[r, , drop = FALSE])
        d <- .detect_dark_box(g, e[1], e[2], hw)
        if (!is.null(d))
          bars[[length(bars) + 1]] <- list(x = d[["x"]], y = d[["y"]], mm = spec$bar[r, ])
      }
    }
  }

  # ---- measure each reference swatch's mean RGB (for colour correction)
  swatches <- lapply(spec$swatch, function(sw) {
    m <- sw$test(Hm, Sm, Vm) & cardFill
    m <- EBImage::opening(m * 1, EBImage::makeBrush(9, "disc")) > 0
    blob <- .largest_blob(m, min_area = 80)
    val <- c(NA_real_, NA_real_, NA_real_)
    if (!is.null(blob))
      val <- c(mean(rgb[, , 1][blob$mask]),
               mean(rgb[, , 2][blob$mask]),
               mean(rgb[, , 3][blob$mask]))
    list(name = sw$name, rgb = val, ref = sw$rgb)
  })

  # ---- perspective-correct card outline (for overlays). Fit the full homography
  # from all control points (mm -> image) and map the card's mm corners back, so
  # the drawn outline follows the real skew (not the min-area rectangle).
  src <- cross; cmm <- spec$tgt
  for (p in patches) { src <- rbind(src, c(p$x, p$y)); cmm <- rbind(cmm, p$mm) }
  for (b in bars)    { src <- rbind(src, c(b$x, b$y)); cmm <- rbind(cmm, b$mm) }
  outline <- quad
  Hfull <- if (nrow(src) >= 4) tryCatch(.fit_homography(cmm, src), error = function(e) NULL) else NULL
  if (!is.null(Hfull)) outline <- .apply_h(Hfull, card_mm)

  det <- list(crosshairs = cross, patches = patches, bars = bars,
              swatches = swatches, ppm = ppm, quad = quad, outline = outline,
              pose = Hpose, homography = Hfull, guesses = guess,
              dim = c(width = W0, height = H0), spec = spec)
  class(det) <- "scalecard_detection"
  if (verbose) {
    cat(sprintf("Card found in %d x %d px image (~%.2f px/mm).\n", W0, H0, ppm))
    cat(sprintf("  3 crosshairs + %d colour patches + %d bar squares = %d control points.\n",
                length(patches), length(bars), 3 + length(patches) + length(bars)))
    found <- vapply(swatches, function(s) !any(is.na(s$rgb)), logical(1))
    nm <- vapply(swatches, function(s) s$name, "")
    cat(sprintf("  swatches measured: %s\n",
                if (any(found)) paste(nm[found], collapse = ", ") else "(none)"))
  }
  det
}

.detect_fail <- function(verbose) {
  if (verbose)
    message("Could not find the card by its colour patches. ",
            "Try manual_targets() to place the crosshairs by hand.")
  invisible(NULL)
}
