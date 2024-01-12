# bookkeeping
library(patternize)
patternize_list <- readRDS("06_pomacanthus/rds_files/patternize_list.rds")
rm_idx <- c(3, 14)
patternize_list <- patternize_list[-rm_idx]
source("patPCA_total.R")

# run the PCA
wasp_pca <- patPCA_total(patternize_list, quietly = FALSE)

# plot images instead of points:
PCx <- 1; PCy <- 2
pca_summary <- summary(wasp_pca)
limits <- apply(wasp_pca$x[ , c(PCx, PCy)], 2, range)
rc_list <- readRDS("rds_files/rc_list_final.rds")[-rm_idx]

# metadata
names(rc_list)
sp <- substr(names(rc_list), 1, 7)
plot(wasp_pca$x[ , c(PCx, PCy)],
     asp = 1,
     col = viridis::viridis(4)[factor(sp)],
     bg = viridis::viridis(4, alpha = 0.5)[factor(sp)],
     cex = 5, lwd = 4, pch = 21,
     xlim = limits[ , 1] + c(-5, 5), 
     ylim = limits[ , 2] + c(-10, 10),
     xlab=paste0('PC1 (', round(pca_summary$importance[2, PCx]*100, 1), ' %)'),
     ylab=paste0('PC2 (', round(pca_summary$importance[2, PCy]*100, 1), ' %)'))

png("pca.png",
    width = 7, height = 6, unit = "in",
    res = 600)
plot(wasp_pca$x[ , c(PCx, PCy)],
     asp = 1,
     col = viridis::viridis(4)[factor(sp)],
     bg = viridis::viridis(4, alpha = 0.5)[factor(sp)],
     cex = 10, lwd = 4, pch = 21,
     cex.lab = 1.5, cex.axis = 1.5,
     xlim = limits[ , 1] + c(-5, 5), 
     ylim = limits[ , 2] + c(-10, 10),
     xlab=paste0('PC1 (', round(pca_summary$importance[2, PCx]*100, 1), ' %)'),
     ylab=paste0('PC2 (', round(pca_summary$importance[2, PCy]*100, 1), ' %)'))
for (i in 1:length(rc_list)) {
  add_image(rc_list[[i]]$original_img, 
            x = wasp_pca$x[i, PCx],
            y = wasp_pca$x[i, PCy],
            width = 60)
}
dev.off()
# plot color maps instead of original images:
plot(wasp_pca$x[ , c(PCx, PCy)], type = "n",
     asp = 1,
     xlim = limits[ , 1] + c(-5, 5), 
     ylim = limits[ , 2] + c(-10, 10),
     xlab=paste0('PC1 (', round(pca_summary$importance[2, PCx]*100, 1), ' %)'),
     ylab=paste0('PC2 (', round(pca_summary$importance[2, PCy]*100, 1), ' %)'))
for (i in 1:length(imgs)) {
  add_image(recoloredImage(rc_list[[i]]), 
            x = wasp_pca$x[i, PCx],
            y = wasp_pca$x[i, PCy],
            width = 15)
}
