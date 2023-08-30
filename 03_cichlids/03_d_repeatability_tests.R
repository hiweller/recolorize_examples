# Part 4: repeatability of results across recolorize users and
# runs of k-means clustering

# bookkeeping ####
library(patternize)
library(recolorize)
source("patPCA_total.R")

# read in all the zone map lists:
zone_map_files <- c("03_cichlids/rds_files/kmeans_standard.rds", # from the 03_b2 script
                    "03_cichlids/rds_files/kmeans_standard_2.rds", # same code from 03_b2 run again
                    "03_cichlids/rds_files/kmeans_aggregate.rds", # from the 03_b2 script
                    "03_cichlids/rds_files/kmeans_aggregate_2.rds", # same code from 03_b2 run again
                    "03_cichlids/rds_files/recolorize_list.rds", # from the 03_b1 script
                    "03_cichlids/rds_files/recolorize_list_user1.rds", # same user, re-segmented
                    "03_cichlids/rds_files/recolorize_list_user2.rds") # different user

# read into a list
zone_map_list <- lapply(zone_map_files, readRDS)

# rename to keep track
names(zone_map_list) <- c("km_std_1", "km_std_2",
                          "km_agg_1", "km_agg_2",
                          "rc_user1a", "rc_user1b", "rc_user2")

# load extents
extent_list <- readRDS("03_cichlids/rds_files/image_extents.rds")

# convert to patternize:
patternize_list <- zone_map_list
for (i in 1:length(zone_map_list)) {
  patternize_list[[i]] <- lapply(zone_map_list[[i]], recolorize_to_patternize)
}

# and set extents again:
for (k in 1:length(patternize_list)) {
  for (i in 1:length(patternize_list[[1]])) {
    for (j in 1:length(patternize_list[[1]][[1]]))
      raster::extent(patternize_list[[k]][[i]][[j]]) <- extent_list[[i]]
  }
}

# Run PCAs ####
pca_list <- vector("list", length = length(patternize_list))
names(pca_list) <- names(patternize_list)

# Run a color pattern PCA for each set of zone maps:
for (i in 1:length(patternize_list)) {
  pca_list[[i]] <- patPCA_total(patternize_list[[i]],
                                quietly = FALSE, plotting = FALSE)
}

# plot the PCAs:
for (i in 1:length(pca_list)) {
  neolamp_pca <- pca_list[[i]]
  sp <- stringr::str_split(rownames(neolamp_pca$x), "_", simplify = TRUE)[ , 2]
  par(mfrow = c(1, 1), mar = rep(4, 4))
  summ <- summary(neolamp_pca)
  plot(neolamp_pca$x[, 1:2], asp = 1, pch = 21,
       xlab = paste0("PC1 (", round(summ$importance[2, 1] * 100), "% var.)"),
       ylab = paste0("PC2 (", round(summ$importance[2, 2] * 100), "% var.)"),
       bg = ggplot2::alpha(c("gold", "purple")[factor(sp)], 0.5),
       col = c("gold", "purple")[factor(sp)],
       cex = 6, lwd = 2,
       cex.lab = 1.5, cex.axis = 1.5,
       main = names(patternize_list)[i])
}

# Clustering metrics ####
library(fossil)
library(cluster)

# calculate distance matrices
rc_dists <- lapply(pca_list, \(x) dist(x$x))

# assign groups
rc_assigns <- lapply(rc_dists, \(x) cutree(hclust(x), k = 2))

# 'ground truth' groups (e.g. species IDs) - this is what we're comparing to
sp_assign <- as.numeric(factor(sp))

# adjusted rand index to compare clustering (1 = identical group assignments, 0 = maximally dissimilar)
lapply(rc_assigns, \(x) adj.rand.index(sp_assign, x))


# calculate silhouette scores (higher is better)
sp_assign <- as.numeric(factor(sp))
rc_silhouettes <- lapply(rc_dists, \(x) summary(silhouette(sp_assign, x)))
setNames(lapply(rc_silhouettes, \(x) x$avg.width),
                names(rc_silhouettes))

# Comparing segmentation results ####

# We can make an absolutely massive comparison matrix (you may want to 
# print this to a PDF instead):
layout(matrix(1:(23*8), nrow = 23, ncol = 8))
par(mar = rep(0, 4))

# Plot original images:
for (i in 1:length(zone_map_list[[1]])) {
  plot(zone_map_list[[1]][[i]]$original_img)
}

# plot zone maps:
for (i in 1:length(zone_map_list)) {
  pal <- zone_map_list$rc_user1a[[1]]$centers
  for (j in 1:length(zone_map_list[[i]])) {
    o <- recolorize::match_colors(zone_map_list[[i]][[j]]$centers,
                                  pal)
    plotImageArray(constructImage(zone_map_list[[i]][[j]]$pixel_assignments,
                                  centers = pal[o, ]))
  }
}
# from left to right, the columns are:
# original images
# kmeans 1
# kmeans 2
# kmeans aggregate 1
# kmeans aggregate 2
# recolorize user 1a
# recolorize user 1b
# recolorizer user 2

# Differences between zone maps

# we'll focus on the four images from Fig. 3 of the paper:
idx <- c(2, 6, 19, 23)

layout(matrix(1:12, nrow = 3, byrow = T)); par(mar = rep(1, 4))
imdist_function <- function(list1, list2, main = "") {
  for (i in 1:length(idx)) {
    imd <- imDist(recoloredImage(list1[[i]]),
                  recoloredImage(list2[[i]]), plotting = F)
    
    imd[which(imd > 0)] <- 1
    
    if (i == 1) {
      m <- main
    } else { m <- "" }
    
    imHeatmap(imd, palette = c(grey(0.8), grey(0.4)), 
              range = c(0, 1), legend = F, main = m)
    
    pct <- sum(imd==1, na.rm = T) / sum(!is.na(imd))
    
    text(0.2, 0.05, paste0(round(pct, digits = 3)*100, "%"), cex = 1)
  }
}

# user 1a vs. 1b
imdist_function(zone_map_list$rc_user1a,
                zone_map_list$rc_user1b,
                main = "user 1a vs. user 1b")

# user 1a vs. user 2
imdist_function(zone_map_list$rc_user1a,
                zone_map_list$rc_user2,
                main = "user 1a vs. user 2")

# user 1b vs. user 2
imdist_function(zone_map_list$rc_user1b,
                zone_map_list$rc_user2,
                main = "user 1b vs. user 2")

# for the kmeans zone maps, we have to define
# another image distance function that tries to match up the 
# color centers as best as possible before comparing:

imdist_function2 <- function(list1, list2,
                             method = "reorder",
                             pal = "",
                             main = "") {
  pct_vec <- c()
  
  for (i in 1:length(list1)) {
    
    if (method == "reorder") {
      pal <- list1[[1]]$centers
      o1 <- match_colors(list1[[i]]$centers, pal)
      o2 <- match_colors(list2[[i]]$centers, pal)
      im1 <- reorder_colors(list1[[i]], o1)
      im2 <- reorder_colors(list2[[i]], o2)
      imd <- imDist(constructImage(im1$pixel_assignments,
                                   pal),
                    constructImage(im2$pixel_assignments,
                                   pal),
                    plotting = F)
    } else {
      imd <- imDist(recoloredImage(list1[[i]]),
                    recoloredImage(list2[[i]]), plotting = F)
    }
    
    imd[which(imd > 0)] <- 1
    
    pct <- sum(imd==1, na.rm = T) / sum(!is.na(imd))
    pct_vec <- c(pct_vec, pct)
  }
  return(pct_vec)
}

# reference palette
pal <- zone_map_list$rc_user1a[[1]]$centers

# calculate differences in zone maps across combinations that can be compared:
u1_u1p <- imdist_function2(zone_map_list$rc_user1a,
                           zone_map_list$rc_user1b,
                           pal = pal)
u1_u2 <- imdist_function2(zone_map_list$rc_user1a,
                          zone_map_list$rc_user2,
                          pal = pal)
u1p_u2 <- imdist_function2(zone_map_list$rc_user1b,
                           zone_map_list$rc_user2,
                           pal = pal)
k1_k2 <- imdist_function2(zone_map_list$km_std_1,
                          zone_map_list$km_std_2,
                          pal = pal)
ka1_ka2 <- imdist_function2(zone_map_list$km_agg_1,
                            zone_map_list$km_agg_2,
                            pal = pal)
u1_ka1 <- imdist_function2(zone_map_list$rc_user1a,
                           zone_map_list$km_agg_1,
                           pal = pal)

# make a list and calculate some summary metrics:
distlist <- list(u1_u1p = u1_u1p,
                 u1_u2 = u1_u2,
                 u1p_u2 = u1p_u2,
                 k1_k2 = k1_k2,
                 ka1_ka2 = ka1_ka2,
                 u1_ka1 = u1_ka1)
lapply(distlist, mean)
lapply(distlist, sd)

