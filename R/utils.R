# Internal helpers. Only rgb_to_hsv_cv is documented/exported-ish (kept internal).

# ---- image loading -----------------------------------------------------------

# Accept a file path or an existing magick image; always return a magick image.
.load_image <- function(image) {
  if (inherits(image, "magick-image")) return(image)
  if (is.character(image) && length(image) == 1L) return(magick::image_read(image))
  stop("`image` must be a file path or a 'magick-image' object.", call. = FALSE)
}

# RGB pixel array [W, H, 3] in 0..255 from a magick image (via imager, which
# matches the coordinate convention used in the existing register_*.R scripts).
.rgb_array <- function(im) {
  ci <- imager::magick2cimg(im)          # dims (W, H, 1, C), values 0..1
  a  <- as.array(ci)
  nc <- dim(a)[4]
  if (nc < 3) {                          # grayscale -> replicate to 3 channels
    g <- a[, , 1, 1]
    return(array(c(g, g, g), dim = c(dim(g), 3)) * 255)
  }
  rgb <- a[, , 1, 1:3, drop = FALSE]     # (W, H, 1, 3)
  dim(rgb) <- dim(rgb)[c(1, 2, 4)]       # -> (W, H, 3)
  rgb * 255
}

# Grayscale imager cimg (0..1) from a magick image, for template matching.
.gray_cimg <- function(im) imager::grayscale(imager::magick2cimg(im))

# cimg (0..1) -> magick image, without a round-trip through disk.
.cimg_to_magick <- function(ci) magick::image_read(grDevices::as.raster(ci))

# ---- colour ------------------------------------------------------------------

#' Convert an RGB array to HSV using the OpenCV convention
#'
#' Produces Hue in 0..180, Saturation and Value in 0..255, matching the ranges
#' the card's HSV test functions (see \code{\link{card_spec}}) expect.
#'
#' @param rgb numeric array \code{[W, H, 3]} with values in 0..255.
#' @return list with matrices \code{H}, \code{S}, \code{V} (each \code{[W, H]}).
#' @keywords internal
rgb_to_hsv_cv <- function(rgb) {
  r <- rgb[, , 1]; g <- rgb[, , 2]; b <- rgb[, , 3]
  mx <- pmax(r, g, b); mn <- pmin(r, g, b); df <- mx - mn
  H <- matrix(0, nrow(r), ncol(r))
  pos <- df > 0
  is_r <- pos & mx == r
  is_g <- pos & mx == g & !is_r
  is_b <- pos & mx == b & !is_r & !is_g
  H[is_r] <- 60 * ((g[is_r] - b[is_r]) / df[is_r])
  H[is_g] <- 60 * ((b[is_g] - r[is_g]) / df[is_g]) + 120
  H[is_b] <- 60 * ((r[is_b] - g[is_b]) / df[is_b]) + 240
  H <- H %% 360
  S <- ifelse(mx > 0, df / mx, 0) * 255
  list(H = H / 2, S = S, V = mx)         # OpenCV: H is degrees / 2
}

# ---- homography (DLT) --------------------------------------------------------

# Fit a 3x3 homography mapping src (n x 2) -> dst (n x 2). Exact for n == 4,
# least-squares for n > 4 (averages out per-point pixel noise). Same DLT used in
# register_image.R / register_abfo.R.
.fit_homography <- function(src, dst) {
  n <- nrow(src)
  if (n < 4) stop("A homography needs at least 4 point correspondences.", call. = FALSE)
  A <- matrix(0, 2 * n, 8); b <- numeric(2 * n)
  for (i in seq_len(n)) {
    X <- src[i, 1]; Y <- src[i, 2]; u <- dst[i, 1]; v <- dst[i, 2]
    A[2 * i - 1, ] <- c(X, Y, 1, 0, 0, 0, -u * X, -u * Y); b[2 * i - 1] <- u
    A[2 * i,     ] <- c(0, 0, 0, X, Y, 1, -v * X, -v * Y); b[2 * i]     <- v
  }
  matrix(c(qr.solve(A, b), 1), 3, 3, byrow = TRUE)
}

# Apply a homography to points p (n x 2) -> (n x 2).
.apply_h <- function(H, p) {
  q <- cbind(p, 1) %*% t(H)
  q[, 1:2, drop = FALSE] / q[, 3]
}

# ---- blob analysis (EBImage stands in for OpenCV findContours/moments) -------

# Largest connected component of a logical mask. Returns its area (px), centroid
# (x, y) and a logical mask of just that blob, or NULL if none exceeds min_area.
.largest_blob <- function(mask, min_area = 1) {
  lab <- EBImage::bwlabel(mask * 1)
  n <- max(lab)
  if (n < 1) return(NULL)
  sizes <- tabulate(lab[lab > 0], nbins = n)
  id <- which.max(sizes)
  if (sizes[id] < min_area) return(NULL)
  blob <- lab == id
  ij <- which(blob, arr.ind = TRUE)
  list(area = sizes[id],
       x = mean(ij[, 1]), y = mean(ij[, 2]),
       mask = blob)
}

# Four corners of a blob from its extreme points (TL, TR, BR, BL), robust to the
# card's rounded corners. xs/ys are pixel coords of the blob.
.extreme_quad <- function(xs, ys) {
  s <- xs + ys; d <- xs - ys
  rbind(
    TL = c(xs[which.min(s)], ys[which.min(s)]),
    TR = c(xs[which.max(d)], ys[which.max(d)]),
    BR = c(xs[which.max(s)], ys[which.max(s)]),
    BL = c(xs[which.min(d)], ys[which.min(d)])
  )
}

# Minimum-area bounding rectangle of a blob (rotating calipers over the convex
# hull) -- a far more accurate card outline than .extreme_quad(), since it fits
# the straight edges and ignores the rounded corners. Returns 4 corners (4x2),
# consistently wound, or NULL on failure (caller falls back to .extreme_quad()).
.min_area_rect <- function(xs, ys) {
  pts <- cbind(xs, ys)
  h <- tryCatch(grDevices::chull(pts), error = function(e) NULL)
  if (is.null(h) || length(h) < 3) return(NULL)
  hp <- pts[h, , drop = FALSE]; nh <- nrow(hp)
  best_area <- Inf; best <- NULL
  for (i in seq_len(nh)) {
    p1 <- hp[i, ]; p2 <- hp[i %% nh + 1, ]
    e <- p2 - p1; L <- sqrt(sum(e^2)); if (L < 1e-9) next
    u <- e / L; v <- c(-u[2], u[1])              # edge direction + normal
    pu <- hp %*% u; pv <- hp %*% v
    area <- (max(pu) - min(pu)) * (max(pv) - min(pv))
    if (area < best_area) {
      best_area <- area
      uv <- rbind(c(min(pu), min(pv)), c(max(pu), min(pv)),
                  c(max(pu), max(pv)), c(min(pu), max(pv)))
      best <- uv %*% rbind(u, v)                  # (u,v) coords -> xy
    }
  }
  best
}

# ---- crosshair refinement (matched filter) -----------------------------------

# Dark ring + crosshair template, zero-mean so correlation ignores local
# brightness. Mirrors make_template() in register_abfo.R.
.make_target_template <- function(R_out, R_in) {
  s  <- 2 * round(R_out) + 1
  c0 <- round(R_out) + 1
  xs <- matrix(rep(1:s, times = s), nrow = s)
  ys <- matrix(rep(1:s, each  = s), nrow = s)
  rho <- sqrt((xs - c0)^2 + (ys - c0)^2)
  T <- matrix(0, s, s)
  T[rho >= R_in & rho <= R_out] <- 1                              # the ring
  T[(abs(xs - c0) <= 1 | abs(ys - c0) <= 1) & rho <= R_out] <- 1  # crosshair
  imager::as.cimg(T - mean(T))
}

# Snap an approximate crosshair position to the true target centre, via local
# template correlation + sub-pixel centroid. g = grayscale cimg (0..1).
.refine_crosshair <- function(g, cx, cy, R_out, R_in) {
  win <- round(R_out * 1.6)
  x0 <- max(1, round(cx) - win); x1 <- min(imager::width(g),  round(cx) + win)
  y0 <- max(1, round(cy) - win); y1 <- min(imager::height(g), round(cy) + win)
  if (x1 - x0 < 8 || y1 - y0 < 8) return(c(x = cx, y = cy))
  sub <- g[x0:x1, y0:y1, 1, 1]
  d   <- imager::as.cimg(1 - sub)                  # invert: dark features -> high
  Tm  <- .make_target_template(R_out, R_in)
  if (imager::width(Tm) >= nrow(sub) || imager::height(Tm) >= ncol(sub))
    return(c(x = cx, y = cy))
  cc  <- as.matrix(imager::correlate(d, Tm))
  if (is.null(dim(cc))) cc <- matrix(cc, nrow = nrow(sub))
  pk  <- arrayInd(which.max(cc), dim(cc))
  rad <- 3
  ix  <- max(1, pk[1] - rad):min(nrow(cc), pk[1] + rad)
  iy  <- max(1, pk[2] - rad):min(ncol(cc), pk[2] + rad)
  blk <- cc[ix, iy]; blk <- blk - min(blk)
  if (sum(blk) > 0) {
    sx <- sum(outer(ix, rep(1, length(iy))) * blk) / sum(blk)
    sy <- sum(outer(rep(1, length(ix)), iy) * blk) / sum(blk)
  } else { sx <- pk[1]; sy <- pk[2] }
  c(x = x0 + sx - 1, y = y0 + sy - 1)
}

# ---- flat-space crosshair refinement (template + normalised cross-correlation)

# Bilinear sample of a grayscale matrix G [W, H] at (possibly fractional) pixel
# coordinates x, y (1-indexed, vectorised).
.bilinear <- function(G, x, y) {
  W <- nrow(G); H <- ncol(G)
  x <- pmin(pmax(x, 1), W - 1e-3); y <- pmin(pmax(y, 1), H - 1e-3)
  x0 <- floor(x); y0 <- floor(y); fx <- x - x0; fy <- y - y0; x1 <- x0 + 1; y1 <- y0 + 1
  G[cbind(x0, y0)] * (1 - fx) * (1 - fy) + G[cbind(x1, y0)] * fx * (1 - fy) +
  G[cbind(x0, y1)] * (1 - fx) * fy       + G[cbind(x1, y1)] * fx * fy
}

# Sample a canonical (axis-aligned, unrotated) window of the card around a known
# mm position, by mapping each flat-grid pixel into the original image via the
# pose homography H (mm -> image) and bilinear-sampling the grayscale G.
.sample_flat_window <- function(G, H, mx, my, ppm, half) {
  ii <- -half:half
  grid <- expand.grid(dx = ii, dy = ii)
  mm <- cbind(mx + grid$dx / ppm, my + grid$dy / ppm)
  img <- .apply_h(H, mm)
  matrix(.bilinear(G, img[, 1], img[, 2]), length(ii), length(ii))   # [dx, dy]
}

# Synthetic target template at a given px/mm: white card with a dark ring, a
# square box outline and a crosshair -- the printed rectification target design.
.make_box_target <- function(ppm, R_mm = 4.8, hb_mm = 5.5, th_mm = 0.4) {
  hb <- hb_mm * ppm; R <- R_mm * ppm; th <- max(1, th_mm * ppm)
  pad <- round(0.1 * ppm) + 2
  s <- 2 * round(hb + pad) + 1; c0 <- (s + 1) / 2
  X <- matrix(rep(1:s, times = s), s, s); Y <- matrix(rep(1:s, each = s), s, s)
  dx <- X - c0; dy <- Y - c0; rho <- sqrt(dx^2 + dy^2); cheb <- pmax(abs(dx), abs(dy))
  T <- matrix(1, s, s)
  T[abs(rho - R) <= th] <- 0                          # ring
  T[abs(cheb - hb) <= th] <- 0                        # square box outline
  T[(abs(dx) <= th | abs(dy) <= th) & cheb <= hb] <- 0  # crosshair
  T
}

# Normalised cross-correlation of template T0 (already zero-mean) over window
# `win`; returns the sub-pixel (x, y) of the best match in window coords, or NULL.
.ncc_peak <- function(win, T0) {
  n <- length(T0); Tn <- sqrt(sum(T0^2)); if (Tn < 1e-9) return(NULL)
  if (nrow(T0) >= nrow(win) || ncol(T0) >= ncol(win)) return(NULL)
  Wc <- imager::as.cimg(win); ones <- imager::as.cimg(matrix(1, nrow(T0), ncol(T0)))
  num <- as.matrix(imager::correlate(Wc, imager::as.cimg(T0)))
  s1  <- as.matrix(imager::correlate(Wc, ones))
  s2  <- as.matrix(imager::correlate(imager::as.cimg(win^2), ones))
  v <- s2 - s1^2 / n; v[v < 1e-9] <- 1e-9
  ncc <- num / (sqrt(v) * Tn)
  m <- floor(nrow(T0) / 2) + 1                         # valid (full-overlap) region
  valid <- matrix(-Inf, nrow(win), ncol(win))
  xi <- m:(nrow(win) - m + 1); yi <- m:(ncol(win) - m + 1)
  if (length(xi) < 1 || length(yi) < 1) return(NULL)
  valid[xi, yi] <- ncc[xi, yi]
  pk <- arrayInd(which.max(valid), dim(valid))
  rad <- 2
  ix <- max(1, pk[1] - rad):min(nrow(ncc), pk[1] + rad)
  iy <- max(1, pk[2] - rad):min(ncol(ncc), pk[2] + rad)
  blk <- ncc[ix, iy]; blk <- blk - min(blk)
  if (sum(blk) > 0) {
    sx <- sum(outer(ix, rep(1, length(iy))) * blk) / sum(blk)
    sy <- sum(outer(rep(1, length(ix)), iy) * blk) / sum(blk)
  } else { sx <- pk[1]; sy <- pk[2] }
  c(sx, sy)
}

# Refine the crosshair targets in flat card space: for each nominal mm target,
# sample a canonical window via the pose, NCC-match the printed-target template,
# and map the sub-pixel centre back to image px. Returns image px (n x 2). The
# nominal pose mapping is kept for any target whose match is implausible.
.refine_targets_flat <- function(G, H, tgt_mm, ppm, win_mm = 8) {
  half <- round(win_mm * ppm)
  T  <- .make_box_target(ppm); T0 <- T - mean(T)
  cen <- half + 1
  out <- .apply_h(H, tgt_mm)                           # nominal fallback
  for (i in seq_len(nrow(tgt_mm))) {
    win <- .sample_flat_window(G, H, tgt_mm[i, 1], tgt_mm[i, 2], ppm, half)
    pk <- .ncc_peak(win, T0)
    if (is.null(pk)) next
    du <- (pk[1] - cen) / ppm; dv <- (pk[2] - cen) / ppm
    if (sqrt(du^2 + dv^2) > 0.5 * win_mm) next          # match too far -> keep nominal
    out[i, ] <- .apply_h(H, matrix(c(tgt_mm[i, 1] + du, tgt_mm[i, 2] + dv), 1))
  }
  out
}

# Largest dark-blob centroid in a local window of a grayscale cimg (0..1), used
# to lock the 8 cm bar squares. Returns c(x, y) full-image px or NULL.
.detect_dark_box <- function(g, ex, ey, hw, thr = 0.27) {
  x0 <- max(1, round(ex - hw)); x1 <- min(imager::width(g),  round(ex + hw))
  y0 <- max(1, round(ey - hw)); y1 <- min(imager::height(g), round(ey + hw))
  if (x1 - x0 < 5 || y1 - y0 < 5) return(NULL)
  sub <- g[x0:x1, y0:y1, 1, 1]
  mask <- sub < thr
  br <- EBImage::makeBrush(3, "disc")
  mask <- EBImage::opening(mask * 1, br) > 0
  blob <- .largest_blob(mask, min_area = 40)
  if (is.null(blob)) return(NULL)
  c(x = x0 + blob$x - 1, y = y0 + blob$y - 1)
}
