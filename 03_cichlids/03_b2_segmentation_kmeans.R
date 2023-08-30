# Step 2b: color segmentation of aligned images with kmeans

# load library
library(recolorize)

# read in images
imgs <- readRDS("03_cichlids/rds_files/image_arrays.rds")

# we'll use the recolorize implementation of kmeans (to keep the code
# more consistent), but it uses the same kmeans algorithm as most other
# R packages (see ?kmeans::stats)

# standard k-means ####

# fit a kmeans object for every image
kmeans_std <- setNames(vector("list", length(imgs)), names(imgs))
for (i in 1:length(imgs)) {
  kmeans_std[[i]] <- recolorize(imgs[[i]], method = "kmeans", n = 4)
}

# we can quickly examine these:
layout(matrix(1:24, nrow = 6)); par(mar = rep(0, 4))
for (j in 1:i) {
  plotImageArray(recoloredImage(kmeans_std[[j]]))
}
# Note that these will probably look slightly different than the examples
# in the paper-- this is because k-means is not deterministic

# the assigned color centers don't always map to our color classes (brown, 
# yellow, blue, and beige), but we can try to force them:
color_palette <- setNames(c("#FFB900", "#9DFFFB", "#BFA78A", "#3C1000"),
                          nm = c("yellow", "blue",    "tan",     "brown"))
color_palette_rgb <- t(col2rgb(color_palette) / 255)
for (j in 1:i) {
  km_rc <- kmeans_std[[j]]
  o <- match_colors(color_palette_rgb,
               km_rc$centers)
  km_rc <- reorder_colors(km_rc, o)
  km_rc$centers <- color_palette_rgb
  plotImageArray(recoloredImage(km_rc))
}
# this makes it a bit more obvious how it's not really working!

# and save:
saveRDS(kmeans_std, 
        "03_cichlids/rds_files/kmeans_standard.rds")

# aggregate kmeans ####

# A common way to try to solve the problem that kmeans gives you different
# colors in a different order for each image is to combine all the images into a
# single image, then use that color palette
mega_image <- abind::abind(imgs, along = 1) # mega image (this will be extremely slow with large datasets)
kmeans_mega <- recolorize(mega_image, method = "kmeans", n = 4)
impose_colors <- kmeans_mega$centers

# you could write code to separate the above "mega-image" back into 
# individual objects, or use e.g. patternize::patK; we'll use
# imposeColors from recolorize to keep the syntax consistent
kmeans_bulk <- setNames(vector("list", length(imgs)), names(imgs))
for (i in 1:length(imgs)) {
  kmeans_bulk[[i]] <- imposeColors(imgs[[i]], 
                                   centers = impose_colors, 
                                   adjust_centers = FALSE)
}

# examine visually: 
layout(matrix(1:24, nrow = 6)); par(mar = rep(0, 4))
o <- match_colors(kmeans_bulk[[1]]$centers,
                  color_palette_rgb)
for (j in 1:i) {
  plotImageArray(constructImage(kmeans_bulk[[j]]$pixel_assignments,
                                color_palette_rgb[o, ]))
}
# As above, these will not necessarily be identical to the paper because k-means is 
# not deterministic

# and save:
saveRDS(kmeans_bulk, 
        "03_cichlids/rds_files/kmeans_aggregate.rds")
