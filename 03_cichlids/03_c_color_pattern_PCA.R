# Step 3: run a whole-color-pattern PCA

# bookkeeping ####
library(patternize)
library(recolorize)
library(raster)
source("patPCA_total.R")

# read in the different color segmentation solutions:
rc_list <- readRDS("03_cichlids/rds_files/recolorize_list.rds")
kmeans_std <- readRDS("03_cichlids/rds_files/kmeans_standard.rds")
kmeans_aggregate <- readRDS("03_cichlids/rds_files/kmeans_aggregate.rds")
extent_list <- readRDS("03_cichlids/rds_files/image_extents.rds")

# convert to patternize:
patternize_list <- lapply(rc_list, recolorize_to_patternize)
patternize_kstd <- lapply(kmeans_std, recolorize_to_patternize)
patternize_kagg <- lapply(kmeans_aggregate, recolorize_to_patternize)

# and set extents again:
for (i in 1:length(patternize_list)) {
  for (j in 1:length(patternize_list[[1]])) {
    raster::extent(patternize_list[[i]][[j]]) <- extent_list[[i]]
    raster::extent(patternize_kstd[[i]][[j]]) <- extent_list[[i]]
    raster::extent(patternize_kagg[[i]][[j]]) <- extent_list[[i]]
  }
}

# PCAs:

# For recolorize and aggregate kmeans, you can optionally specify to only
# analyze color patches corresponding to the three pigment cell types (yellow,
# blue, brown) by uncommenting the 'which_colors' line. We can't do this for
# standard kmeans because the colors are in a random order.

# recolorize:
neolamp_pca <- patPCA_total(patternize_list, 
                            # which_colors = c(1, 2, 4), 
                            quietly = FALSE, plotting = FALSE)

# kmeans standard:
neolamp_pca_ks <- patPCA_total(patternize_kstd,
                               quietly = FALSE, plotting = FALSE)

# kmeans aggregate:
neolamp_pca_ka <- patPCA_total(patternize_kagg, 
                               #which_colors = c(2, 3, 4), 
                               quietly = FALSE, plotting = FALSE)

# Plotting PCAs ####

# Recolorize:
sp <- stringr::str_split(rownames(neolamp_pca$x), "_", simplify = TRUE)[ , 2]
par(mfrow = c(1, 1), mar = rep(4, 4))
summ <- summary(neolamp_pca)
plot(neolamp_pca$x[, 1:2], asp = 1,
     xlab = paste0("PC1 (", round(summ$importance[2, 1] * 100), "% var.)"),
     ylab = paste0("PC2 (", round(summ$importance[2, 2] * 100), "% var.)"),
     col = c("gold", "purple")[factor(sp)],
     #ylim = c(-90, 70), xlim = c(-80, 110),
     cex = 6, lwd = 2,
     main = "Recolorize")

# Add images:
for(i in 1:nrow(neolamp_pca$x)) {
  add_image(recoloredImage(rc_list[[i]]),
            neolamp_pca$x[i, 1], neolamp_pca$x[i, 2],
            width = 25)
}

# kmeans standard:
summ <- summary(neolamp_pca_ks)
plot(neolamp_pca_ks$x[, 1:2],
     xlab = paste0("PC1 (", round(summ$importance[2, 1] * 100), "% var.)"),
     ylab = paste0("PC2 (", round(summ$importance[2, 2] * 100), "% var.)"),
     col = c("gold", "purple")[factor(sp)],
     cex = 6, lwd = 2,
     main = "K-means (original implementation)")

for(i in 1:nrow(neolamp_pca$x)) {
  add_image(recoloredImage(kmeans_std[[i]]),
            neolamp_pca_ks$x[i, 1], neolamp_pca_ks$x[i, 2],
            width = 20)
}

# kmeans aggregate:
summ <- summary(neolamp_pca_ka)
plot(neolamp_pca_ka$x[, 1:2],
     xlab = paste0("PC1 (", round(summ$importance[2, 1] * 100), "% var.)"),
     ylab = paste0("PC2 (", round(summ$importance[2, 2] * 100), "% var.)"),
     col = c("gold", "purple")[factor(sp)],
     cex = 6, lwd = 2,
     main = "K-means (aggregate)")

ref_palette <- rc_list[[1]]$centers
col_order <- match_colors(kmeans_aggregate[[1]]$centers,
                          ref_palette)

for(i in 1:nrow(neolamp_pca$x)) {
  add_image(constructImage(kmeans_aggregate[[i]]$pixel_assignments,
                           centers = ref_palette[col_order, ]),
            neolamp_pca_ka$x[i, 1], neolamp_pca_ka$x[i, 2],
            width = 20)
}

# calculating rand indices ####
library(fossil)

# calculate distance matrices
rc_dist <- dist(neolamp_pca$x)
ks_dist <- dist(neolamp_pca_ks$x)
ka_dist <- dist(neolamp_pca_ka$x)

# simple hierarchical clustering to assign groups:
rc_assign <- cutree(hclust(rc_dist), k = 2)
ks_assign <- cutree(hclust(ks_dist), k = 2)
ka_assign <- cutree(hclust(ka_dist), k = 2)

# 'ground truth' groups (e.g. species IDs) - this is what we're comparing to
sp_assign <- as.numeric(factor(sp))

# adjusted rand index to compare clustering (1 = identical group assignments, 0 = maximally dissimilar)
adj.rand.index(sp_assign, rc_assign) # identical clustering
adj.rand.index(sp_assign, ks_assign) # near-0
adj.rand.index(sp_assign, ka_assign) # better, but still lower than rc_assign
# The exact values of the kmeans values will differ