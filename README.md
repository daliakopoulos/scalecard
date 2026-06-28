# scalecard

Rectify (perspective-correct) and scale photographs taken with the **Credit Card
Photography Scale** (Past Horizons) calibration card, in R. It is an R port of the
browser/OpenCV.js [card rectifier](../rectify_app/index.html), built on
`magick` + `imager` + `EBImage` instead of OpenCV.

The card's three crosshair *rectification targets* (on a precise 50 × 20 mm
frame), its red/yellow/blue colour patches and its 8 cm scale-bar squares are
detected automatically and used as ground-control points for a projective
(homography) transform. The output is a flat image at a known pixels-per-mm
scale, so any feature can be measured in millimetres.

## Install

```r
# needs: magick, imager, EBImage (Bioconductor)
# install.packages(c("magick", "imager"))
# BiocManager::install("EBImage")
install.packages("path/to/scalecard", repos = NULL, type = "source")
# or, during development:
pkgload::load_all("scalecard")
```

## Quick start

```r
library(scalecard)

# one call: auto-detect the card, rectify, white-balance, write a PNG
rec <- rectify_card("photo.jpg", output = "photo_rectified.png")
rec$px_per_mm            # 10  -> 0.1 mm per pixel
measure_mm(rec, 250)     # 250 px on the output = 25 mm

# colour-correct to the card's reference swatches + a QC mm grid
rectify_card("photo.jpg", color_correct = TRUE, grid = TRUE,
             output = "photo_rectified.png")
```

## How it works

`detect_card()`:

1. finds the card by its red/yellow/blue colour signature (HSV masks + connected
   components via `EBImage`);
2. fits the card's **minimum-area rectangle** for an accurate pose, and resolves
   its orientation using the detected colour patches;
3. refines the three crosshair targets in **flattened card space** — each target
   is sampled at its known mm position with an upright, canonical appearance and
   located sub-pixel by normalised cross-correlation against a synthetic
   box+ring+crosshair template (the printed target design) — then maps the
   centres back to image pixels;
4. locates the colour-patch centres and the 8 cm bar squares as extra control
   points, and measures the six reference swatches for colour correction.

`rectify_card()` builds a homography from all the control points and warps the
photo flat at `px_per_mm`. Typical registration RMS error is well under 0.5 mm.
The **full scene is always kept (never cropped)**: the canvas is sized to contain
every original pixel. For steeply-angled photos the rectified plane can balloon,
so `max_px` (default 12000) caps the long edge by lowering the effective
resolution rather than cropping. Areas that fall **outside the original photo**
(or that would be smeared at a grazing angle) are filled with **white** rather
than streaked edge pixels (`trim_stretch = TRUE`, `max_stretch = 4`).

If auto-detection fails (poor lighting, busy background), fall back to
`manual_targets("photo.jpg")` — click the three crosshairs in a native window.

## Important: it rectifies ONE plane

The card fixes a single plane (the surface it lies on). Consequences for
measuring objects placed around the card:

- **Flat objects on that surface** (e.g. compost impurities — film, fragments)
  rectify to their **true size and shape**, anywhere in the frame.
- **Objects with height / 3-D shapes** distort by parallax. A single photo cannot
  correct this — the card is flat, so it carries no depth information.
- The farther from the card and the **steeper the camera angle**, the more the
  surrounding plane is stretched when flattened.

**For best results:** lay the card flat *in the same plane* as the objects and
*close to them*; keep objects flat; shoot as square-on (top-down) as you can with
the whole working area in frame. Then everything on that plane measures correctly.

## Functions

| function | purpose |
|---|---|
| `rectify_card()` | detect + rectify + (optional) colour fix + grid; returns an object, writes a PNG if `output=` is given |
| `detect_card()`  | automatic detection of all ground-control points |
| `manual_targets()` | interactive click-based fallback for the crosshairs |
| `white_balance()` | gray-world white balance off the card |
| `color_correct()` | 3×4 colour transform to the card's reference swatches |
| `measure_mm()` | convert a pixel distance on a rectified image to mm |
| `card_spec()` | the fixed card geometry (mm coords, swatch colours) |

The geometry in `card_spec()` was measured from the physical card; edit it there
if your card differs.

## Vignette

A worked walkthrough lives at [vignettes/scalecard.Rmd](vignettes/scalecard.Rmd)
(an R Markdown `html_vignette`, following the
[r-pkgs.org](https://r-pkgs.org/vignettes.html) conventions). After installing
with vignettes, open it with:

```r
vignette("scalecard")          # or: browseVignettes("scalecard")
```

Build/install it yourself with:

```r
devtools::install("scalecard", build_vignettes = TRUE)
```

Note: building the vignette needs **pandoc**. If you don't have a standalone
pandoc but do have Quarto, point R at Quarto's bundled copy first:

```r
Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/Quarto/bin/tools")  # adjust path
```

