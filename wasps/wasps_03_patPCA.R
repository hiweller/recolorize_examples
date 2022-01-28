# bookkeeping
library(patternize)
patternize_list <- readRDS("rds_files/patternize_list.rds")
source("patPCA_total.R")

# run the PCA
wasp_pca <- patPCA_total(patternize_list, quietly = FALSE)

# plot images instead of points:
PCx <- 1; PCy <- 2
pca_summary <- summary(wasp_pca)
limits <- apply(wasp_pca$x[ , c(PCx, PCy)], 2, range)
images <- lapply(dir("original_images/", full.names = TRUE),
                 readImage)
plot(wasp_pca$x[ , c(PCx, PCy)], type = "n",
     asp = 1,
     xlim = limits[ , 1] + c(-5, 5), 
     ylim = limits[ , 2] + c(-10, 10),
     xlab=paste0('PC1 (', round(pca_summary$importance[2, PCx]*100, 1), ' %)'),
     ylab=paste0('PC2 (', round(pca_summary$importance[2, PCy]*100, 1), ' %)'))
for (i in 1:length(images)) {
  add_image(images[[i]], 
            x = wasp_pca$x[i, PCx],
            y = wasp_pca$x[i, PCy],
            width = 15)
}

# plot color maps instead of points:
plot(wasp_pca$x[ , c(PCx, PCy)], type = "n",
     asp = 1,
     xlim = limits[ , 1] + c(-5, 5), 
     ylim = limits[ , 2] + c(-10, 10),
     xlab=paste0('PC1 (', round(pca_summary$importance[2, PCx]*100, 1), ' %)'),
     ylab=paste0('PC2 (', round(pca_summary$importance[2, PCy]*100, 1), ' %)'))
for (i in 1:length(imgs)) {
  add_image(recoloredImage(impose_list[[i]]), 
            x = wasp_pca$x[i, PCx],
            y = wasp_pca$x[i, PCy],
            width = 15)
}
