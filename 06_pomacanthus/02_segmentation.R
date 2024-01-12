# load libraries
library(patternize)
library(raster)
library(recolorize)

# load the image arrays:
imgs <- readRDS("06_pomacanthus/rds_files/image_arrays.rds")

# first pass (overcluster)
rc_list <- lapply(imgs, 
                  function(i) recolorize2(i, bins = 3,
                                          color_space = "Lab",
                                          cutoff = 30, plotting = F))

# save results to avoid running that again
saveRDS(rc_list, "06_pomacanthus/rds_files/rc_list_init_fit.rds")

# load in:
rc_list <- readRDS("06_pomacanthus/rds_files/rc_list_init_fit.rds")

# make an aggregate palette:
all_palettes <- do.call(rbind, lapply(rc_list, function(i) i$centers))
all_sizes <- do.call(c, lapply(rc_list, function(i) i$sizes / sum(i$sizes)))

# cluster colors
cluster_list <- hclust_color(all_palettes, n_final = 5)

# make an empty matrix for storing the new palette
pom_palette <- matrix(NA, ncol = 3, nrow = length(cluster_list))

# for every color in cluster_list...
for (i in 1:length(cluster_list)) {
  
  # get the center indices
  idx <- cluster_list[[i]]
  
  # get the average value for each channel, using cluster size to get a weighted average
  ctr <- apply(all_palettes, 2, 
               function(j) weighted.mean(j[idx], 
                                         w = all_sizes[idx]))
  
  # store in the palette matrix
  pom_palette[i, ] <- ctr
}

# see how well we like this:
plotColorPalette(pom_palette)

# tbh...
# problem children: 1
rc_list <- setNames(vector("list", length(imgs)), nm = names(imgs))

i <- 1
rc <- imposeColors(imgs[[i]],
                     pom_palette, plotting = FALSE); plot(rc)
rc <- mergeLayers(rc, 
                    merge_list = list(black = c(2, 5),
                                      other = c(1, 3, 4)),
                    color_to = c("#0F1116",
                                 "#b9b9b9"))

plot(rc)
rc2 <- absorbLayer(rc, 2, function(s) s <= 50)
rc2 <- editLayer(rc, 1, "fill", 3)
rc3 <- editLayer(rc2, 1, "fill", 2)
rc_list[[i]] <- rc2

saveRDS(rc_list, "06_pomacanthus/rds_files/rc_list_temporary.rds")

# convert back to patternize (including extent)
rc_list <- readRDS("rds_files/rc_list_final.rds")
patternize_list <- lapply(rc_list, recolorize_to_patternize)
extent_list <- readRDS("rds_files/extent_list.rds")
for (i in 1:length(patternize_list)) {
  for (j in 1:length(patternize_list[[1]])) {
    raster::extent(patternize_list[[i]][[j]]) <- extent_list[[i]]
  }
}
saveRDS(patternize_list, "rds_files/patternize_list.rds")
