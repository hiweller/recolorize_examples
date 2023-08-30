# bookkeeping
library(patternize)
patternize_list <- readRDS("04_wasps/rds_files/patternize_list.rds")
metadata <- read.csv("04_wasps/wasp_metadata.csv")
source("patPCA_total.R")

# run the PCA
wasp_pca <- patPCA_total(patternize_list, quietly = FALSE)

# plot images instead of points:
PCx <- 1; PCy <- 2
pca_summary <- summary(wasp_pca)
rc_list <- readRDS("04_wasps/rds_files/recolorize_fits.rds")

# plot zone maps instead of original images:
pal <- RColorBrewer::brewer.pal(11, "RdYlBu")[c(3, 4, 8, 11)]

limits <- apply(wasp_pca$x[ , c(PCx, PCy)], 2, range)

plot(wasp_pca$x[ , c(PCx, PCy)],
     col = pal[factor(round(metadata$Lat, digits = 1))],
     bg = ggplot2::alpha(pal[factor(round(metadata$Lat, digits = 1))], 0.25),
     pch = 21,
     cex = 5, lwd = 1,
     xlim = limits[ , 1] + c(-5, 5), 
     ylim = limits[ , 2] + c(-10, 10),
     xlab = paste0('PC1 (', round(pca_summary$importance[2, PCx]*100, 2), '% var.)'),
      ylab=paste0('PC2 (', round(pca_summary$importance[2, PCy]*100, 2), '% var.)'))

# you might have to scooch the legend coordinates depending on your
# plotting window:
legend(55, 105, fill = pal, 
       legend = paste0(levels(factor(round(metadata$Lat, digits = 1))),
                       "° N"), bty = "n")
for (i in 1:length(rc_list)) {
  fit <- rc_list[[i]]
  fit$centers <- t(col2rgb(c("#312718",
                             "#915322",
                             "#DDB326"))/255)
  add_image(recoloredImage(fit), 
            x = wasp_pca$x[i, PCx],
            y = wasp_pca$x[i, PCy],
            width = 15)
}

