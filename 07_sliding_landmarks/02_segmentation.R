# libraries
library(patternize)
library(recolorize)

# read in aligned images
aligned_images <- readRDS("07_sliding_landmarks/rds_files/aligned_sliding_no_head.rds")

# create an empty vector
rc_list <- vector("list", length(aligned_images))
names(rc_list) <- names(aligned_images)

# make centers
centers <- rbind(rep(0.1, 3), rep(0.9, 3))

# segmentation code
# in this case - just use kmeans
for (i in 1:length(aligned_images)) {
  
  # fit 2 colors
  im <- brick_to_array(aligned_images[[i]])
  
  # I found k-means clustering in CIELAB space to work fine
  rc_obj <- recolorize(im, method = "k", n = 2, 
                       color_space = "Lab",
                       plotting = F)
   
  # darkest color first (k-means = colors are random order every time otherwise)
  rc_obj <- reorder_colors(rc_obj, order(rowMeans(rc_obj$centers)))
  rc_obj$centers <- centers

  # save 
  rc_list[[i]] <- rc_obj
  
  # uncomment to plot:
  # plot(rc_obj) 
}

# diagnostic plot
layout(matrix(1:40, nrow = 10)); par(mar = rep(1, 4))
for (i in 1:length(rc_list)) {
  par(mar = c(0, 1, 2, 1))
  plotImageArray(brick_to_array(aligned_images[[i]]))
  title(names(rc_list)[i], line = 0)
  par(mar = c(2, 1, 0, 1))
  plot(recoloredImage(rc_list[[i]], "raster"))
}

# save the whole list of recolorize objects:
saveRDS(rc_list, file = paste0("07_sliding_landmarks/rds_files/segmented_aligned_images.rds"))
