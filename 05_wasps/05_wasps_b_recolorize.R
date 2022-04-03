# load libraries
library(patternize)
library(raster)
library(recolorize)

# load the imageList_aligned object:
imageList_aligned <- readRDS("05_wasps/rds_files/imageList_aligned.rds")

# convert from RasterBricks to image arrays:
imgs <- lapply(imageList_aligned, brick_to_array)
names(imgs) <- names(imageList_aligned)

# save raster extents:
extent_list <- lapply(imageList_aligned, extent)

# fit initial recolorize fits
rc_list <- lapply(imgs, 
                  function(i) recolorize2(i, bins = 3,
                                          cutoff = 35,
                                          plotting = FALSE))

# optional: write the list as an RDS object so we don't have to rerun this step:
saveRDS(rc_list, "05_wasps/rds_files/recolorize_fits_original.rds")

# get all palettes and sizes
all_palettes <- do.call(rbind, lapply(rc_list, function(i) i$centers))
all_sizes <- do.call(c, lapply(rc_list, function(i) i$sizes / sum(i$sizes)))

# cluster colors
cluster_list <- hclust_color(all_palettes, n_final = 3)

# make an empty matrix for storing the new palette
wasp_palette <- matrix(NA, ncol = 3, nrow = length(cluster_list))

# for every color in cluster_list...
for (i in 1:length(cluster_list)) {
  
  # get the center indices
  idx <- cluster_list[[i]]
  
  # get the average value for each channel, using cluster size to get a weighted average
  ctr <- apply(all_palettes, 2, 
               function(j) weighted.mean(j[idx], 
                                         w = all_sizes[idx]))
  
  # store in the palette matrix
  wasp_palette[i, ] <- ctr
}

saveRDS(wasp_palette, "05_wasps/rds_files/palette.rds")

# and apply
impose_list <- lapply(imgs, function(i) imposeColors(i, wasp_palette, 
                                                     adjust_centers = FALSE))

saveRDS(impose_list, "05_wasps/rds_files/recolorize_fits.rds")

# convert back to patternize (including extent)
patternize_list <- lapply(impose_list, recolorize_to_patternize)
for (i in 1:length(patternize_list)) {
  for (j in 1:length(patternize_list[[1]])) {
    raster::extent(patternize_list[[i]][[j]]) <- extent_list[[i]]
  }
}

# and save
saveRDS(patternize_list, "05_wasps/rds_files/patternize_list.rds")
