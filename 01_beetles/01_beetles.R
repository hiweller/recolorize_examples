# Run the 00_installation_RUN_ME_FIRST.R script first if you haven't already to
# install the packages:
library(recolorize)

# Chrysochroa corbetti (example from Fig. 1 A-E) ####
# This image (by Nathan P. Lord) comes with the package (down-sampled for size):
img <- system.file("extdata/corbetti.png", package = "recolorize")

# We can do this in one line because it's a pretty straightforward segmentation:
rc <- recolorize2(img, n_final = 5)

# We can also despeckle (this is almost entirely for visual appeal):
for (i in 1:length(rc$centers)) {
  rc <- absorbLayer(rc, i, 
                    size_condition <- \(x) x <= prod(dim(rc$original_img)) * 0.001)
}

# Or convert to a vector (again, more useful for creating a scale-free 
# visualization than having any real scientific value):
rc_vec <- recolorizeVector(rc, smoothness = 5, size_filter = 0)
plot(rc_vec)

# The colors of the zone map are useful for referring to the original image, 
# but they are not suitable for chromatic analysis (see paper),
# so you may want to display these with greyscale values:
rc$centers <- t(col2rgb(grey(seq(0, 1, length.out = length(rc$sizes))))) / 255
plot(rc)

# Lighting variation (Fig. 1F, Chrysochroa mnischezii) ####
diffuse <- "01_beetles/mniszechii_diffuse.png"
direct <- "01_beetles/mniszechii_direct.png"

# compare the images of the same beetle under different lighting:
layout(matrix(1:2, nrow = 2)); par(mar = rep(0, 4))
plotImageArray(readImage(diffuse))
plotImageArray(readImage(direct))

# zone map for diffuse light:
# turn plotting to TRUE to see intermediate steps - they're off here to speed
# up the code
diffuse_init <- recolorize2(diffuse, bins = 3, cutoff = 40, rotate = 90, 
                            plotting = FALSE)
diffuse_merge <- mergeLayers(diffuse_init, list(c(1, 4), c(2, 3)),
                             plotting = FALSE)
diffuse_final <- absorbLayer(diffuse_merge, 2, size_condition = function(s) s <= 1000,
                             plotting = FALSE)

# note that the recolorize objects record the calls used to create them:
diffuse_final$call

# CIE Lab space worked better for this image than sRGB did
direct_init <- recolorize2(direct, bins = 4, cutoff = 40, rotate = 90,
                           color_space = "Lab",
                           plotting = FALSE)
direct_merge <- mergeLayers(direct_init, list(c(1, 2, 5), c(3, 4)),
                            plotting = FALSE)
direct_edit <- editLayer(direct_merge, 1, "fill", 3, 
                         plotting = FALSE)
direct_final <- absorbLayer(direct_edit, 2, size_condition = function(s) s <= 1000,
                            plotting = FALSE)

# Comparing the two maps
layout(matrix(1:2, nrow = 1)); par(mar = rep(0, 4))
plotImageArray(recoloredImage(diffuse_final)) 
plotImageArray(recoloredImage(direct_final))

# We can set the zone maps to have the same greyscale palette:
grey_palette <- t(col2rgb(grey(c(0.2, 0.8)))) / 255
diffuse_final$centers <- grey_palette
direct_final$centers <- grey_palette

# and plot again:
layout(matrix(1:2, nrow = 1)); par(mar = rep(0, 4))
plotImageArray(recoloredImage(diffuse_final)) 
plotImageArray(recoloredImage(direct_final))

# and we can compare them to see how different they are:
image_distance <- imDist(recoloredImage(direct_final),
       recoloredImage(diffuse_final))

# proportion of pixels with a non-zero distance (1.5%):
sum(test > 0, na.rm = T) / prod(dim(direct_final$pixel_assignments))

