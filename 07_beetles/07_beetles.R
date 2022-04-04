# Running example F: different types & numbers of colors for a set of images

# Despite coming last, this is actually the easiest batch-processing example of
# the lot, because we don't have to generate a color palette to map everything
# else to (and we're not aligning anything with patternize). We're just going to
# run every image in this folder through the same set of steps.

# load library
library(recolorize)

# get image paths
beetles <- dir("07_beetles/", "png", full.names = TRUE)

# define a custom recolorize function
custom_recolorize_function <- function(img) {
  # blur image:
  rc0 <- blurImage(readImage(img, resize = 0.5),
                   blur_function = "medianblur", n = 3, plotting = FALSE)
  # basic recolorize2 step:
  rc1 <- recolorize2(rc0, bins = 3, cutoff = 45, plotting = FALSE)
  # drop minor colors
  rc2 <- thresholdRecolor(rc1, plotting = FALSE)
  # and recluster again
  rc3 <- recluster(rc2, cutoff = 35, plot_final = FALSE, plot_hclust = FALSE)
  plot(rc3)
  return(rc3)
}

# and run it
beetles_rc <- lapply(beetles, custom_recolorize_function)

# these color maps aren't perfect - I would go through and tweak them if I were
# going to use them for anything, but hopefully this illustrates how you can
# work with a more comparative (e.g. multiple lineages) dataset without having
# to make more subjective calls about number and type of colors

# BONUS -- making two color maps ####
img <- blurImage(readImage(beetles[4], resize = 0.5), n = 4)

# version 1
rc <- recolorize2(img, bins = 3,
                  color_space = "Lab",
                  cutoff = 0)
rc2 <- thresholdRecolor(rc)
rc3 <- recluster(rc2, cutoff = 30)
plot(rc3)

# version 2
rc4 <- mergeLayers(rc3, list(c(1, 3, 5, 6, 8),
                             c(2, 4, 7)))
rc5 <- absorbLayer(rc4, 2, 
                   function(s) s <= 2000,
                   y_range = c(0, 0.8))
