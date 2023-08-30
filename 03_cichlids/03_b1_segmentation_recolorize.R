# Step 2: color segmentation of aligned images with recolorize

# load library
library(recolorize)
library(raster)

# read in aligned images:
imageList_aligned <- readRDS("03_cichlids/rds_files/imageList_aligned.rds")

# convert from RasterBricks to image arrays using the brick_to_array function:
imgs <- lapply(imageList_aligned, recolorize::brick_to_array)
names(imgs) <- names(imageList_aligned)

# save so we can load the images:
saveRDS(imgs, "03_cichlids/rds_files/image_arrays.rds")

# save raster extents for later conversion:
extent_list <- lapply(imageList_aligned, extent)
saveRDS(extent_list, "03_cichlids/rds_files/image_extents.rds")

# we can now drop these objects from the environment:
rm(imageList_aligned, extent_list, imgs)

# read in images
imgs <- readRDS("03_cichlids/rds_files/image_arrays.rds")

# make an empty list for recolorize objects
rc_list <- setNames(vector("list", length(imgs)), names(imgs))

# set the color palette we will use for visualization (NOT the centers that we
# force all the images to align to; the images have too much color variation for 
# a single set of centers to work for each image)
color_palette <- setNames(c("#FFB900", "#9DFFFB", "#BFA78A", "#3C1000"),
                          nm = c("yellow", "blue",    "tan",     "brown"))
color_palette_rgb <- t(col2rgb(color_palette) / 255)

# color segmentation with recolorize ####
# In many cases, images can be fit with a single color palette using a simple
# for loop (see example 2); here we chose an example with extreme lighting
# variation to illustrate a case where each image cannot be segmented using the
# exact same set of steps to illustrate the full utility of the package.

# A relatively simple example image ####
img_1 <- imgs[[1]]

# A single function produces good results:
rc <- recolorize2(img_1, 
                  color_space = "Lab", 
                  bins = 4, cutoff = 20)

# notice we can recreate this map from the original using the 'call' function:
rc$call

# We can assert the color palette for visualization (note that this DOES NOT
# affect the analysis, which is concerned with the shape of the color patch
# and not its hue in the image!)
layout(matrix(1:2, nrow = 2)); par(mar = rep(1, 4))

# compare the original orders:
plotColorPalette(rc$centers)
plotColorPalette(color_palette)

# reorder:
col_order <- match_colors(reference_palette = color_palette, 
                          match_palette = rc$centers, plotting = TRUE)
rc <- reorder_colors(rc, col_order, plotting = TRUE)

# switch palette for visual consistency:
rc$centers <- color_palette_rgb

# the resulting color map:
plot(rc)

# we would then add this to the list:
rc_list[[1]] <- rc




# A more complex image ####
img_2 <- imgs[[2]]

# This gets us 8 colors: 
rc <- recolorize2(img_2, 
                  color_space = "Lab",
                  bins = 4, cutoff = 30)
# (^ see what happens if you increase the cutoff to 35)

# Drop minor colors (< 1% of the image): 
rc2 <- thresholdRecolor(rc, pct = 0.01)

# The most direct way to get down to the right number of colors is to specify merges:
plot(rc2)
rc3 <- mergeLayers(rc2, list(yellow = c(6),
                             blue = c(2, 5),
                             tan = c(1, 4),
                             brown = c(3)),
                   color_to = color_palette)
plot(rc3)
#^ This is sufficient for color segmentation, but to illustrate a couple of 
# other functions:

# Removing speckles:
rc_absorb <- absorbLayer(rc3, 2, function(s) s < 100)

# Filling holes:
rc_fill <- editLayer(rc3, layer_idx = 2, operation = "fill", px_size = 3)

# This object has now been iterated on 4 times, which we can see in the call:
rc_fill$call

# We can even recreate them on the original image:
repeated_rc <- rerun_recolorize(rc_fill, img = img_2)

# check that the palettes are in the same order:
layout(matrix(1:2, nrow = 2)); par(mar = rep(1, 4))
plotColorPalette(color_palette)
plotColorPalette(rc3$centers)

# keep and save:
rc_list[[2]] <- rc3

# You would do some combination of 1 to ~3-4 functions per image and store them
# in a list:
# saveRDS(rc_list, "03_cichlids/rds_files/recolorize_list_practice.rds")
# We're saving it as 'practice' here to avoid overwriting the original in this case
# (so that the downstream examples will always run)

# This example is the most complex of the 3, which is why batch processing
# doesn't work (see the kmeans clustering file), and each image gets its own
# fit. For examples 2 and 3, recolorize runs much faster because we are batch 
# processing the entire set of images at once.

# Load the entire list of recolorize objects (cooking-show-style)
rc_list <- readRDS("03_cichlids/rds_files/recolorize_list.rds")
layout(matrix(1:24, nrow = 6)); par(mar = rep(0, 4))
for (j in rc_list) {
  plotImageArray(recoloredImage(j))
}
# N. brichardi are the left columns, N. pulcher are the right two columns

# On average, each image was processed with 3.3 functions:
mean(unlist(lapply(rc_list, \(x) length(x$call))))
