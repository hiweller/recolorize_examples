# libraries
library(patternize)
library(raster)

# read in and convert recolorize list
rc_list <- readRDS("07_sliding_landmarks/rds_files/kmeans_segmented.rds")

pat_list <- lapply(rc_list, recolorize::recolorize_to_patternize)
names(pat_list) <- names(rc_list)

# heatmap ####
x <- sumRaster(pat_list, names(pat_list), type = 'k')

# fish colors:
plot(x[[1]], asp = 2/5, col = c("white", colorRampPalette(c("beige", "black"))(100)))
# we can see that the stripe locations are quite conserved!

# more traditional palette:
plot(x[[1]], asp = 2/5, col = c("white", viridisLite::viridis(100, direction = -1)))
