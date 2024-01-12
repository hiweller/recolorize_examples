original_images <- dir("images/", full.names = T)
rm_idx <- c(3, 14)
original_images <- original_images[-rm_idx]
landmarks <- dir("txt/", full.names = T)[-rm_idx]
lmk_list <- lapply(landmarks, read.table)
mask_list <- lapply(dir("masks/", full.names = T), read.table)
landmarks
original_images

rc_list <- readRDS("rds_files/rc_list_final.rds")[-rm_idx]
rc_list <- rc_list[c(1:6, 9:12, 7:8)]
x <- recolorize2(raster_to_array(rc_list$`pom_sem_12-2SL`$original_img), 
                bins = 3, cutoff = 45)
source("pat_heat.R")
png("heatmap.png", width = 8, height = 5,
    res = 300, units = "in")
par(mar = c(0, 0, 0, 4))
library(paletteer)
cp <- colorRampPalette(
  rev(paletteer_d("colorBlindness::Blue2Orange12Steps"))
)(100)
pat_heat(patternize_list, 1, 
         colpalette = cp)
dev.off()

pdf("landmark_images.pdf",
    width = 8.39, height = 4.82)
layout(matrix(1:12, nrow = 3, byrow = T)); par(mar = rep(0, 4))
for (i in 1:length(original_images)){
  img <- readImage(original_images[i])
  plotImageArray(img)
  dims <- dim(img)
  points(lmk_list[[i]][ , 1]/dims[2],
         (dims[1] - lmk_list[[i]][ , 2])/dims[1],
         cex = 1, pch = 21, col = "black", bg = "cyan")
}
dev.off()

png("aligned_images.png",
    width = 8.5, height = 5, units = "in", res = 300)
layout(matrix(1:12, nrow = 3, byrow = T)); par(mar = rep(1, 4))
for (i in 1:length(rc_list)) {
  plot(rc_list[[i]]$original_img)
}
dev.off()

png("segmented_images.png",
    width = 8.5, height = 5, units = "in", res = 300)
layout(matrix(1:12, nrow = 3, byrow = T)); par(mar = rep(0, 4))
for (i in 1:length(rc_list)) {
  plotImageArray(constructImage(rc_list[[i]]$pixel_assignments,
                 centers = matrix(c(rep(0.1, 3),
                                    rep(0.8, 3)),
                                  nrow = 2, byrow = T)))
}
dev.off()

plot(rc_list$`pom_imp_4-8cmSL`)
kt <- recolorize(raster_to_array(rc_list$`pom_imp_4-8cmSL`$original_img),
                 method = "k", n= 2)
recolorize_to_png(kt, "kmeans_imperator.png")
kt <- recolorize(raster_to_array(rc_list$`pom_sem_12-2SL`$original_img),
                 method = "k", n= 2)
recolorize_to_png(kt, "kmeans_semicirculatus.png")
png::writePNG(constructImage(rc_list$`pom_sem_12-2SL`$pixel_assignments,
                             centers = matrix(c(rep(0.1, 3),
                                                rep(0.8, 3)),
                                              nrow = 2, byrow = T)),
              target = "recolorize_semicirculatus.png")

rc_ex <- rc_list$`pom_imp_4-8cmSL`
rc_ex$centers <- matrix(c(rep(0.1, 3),
                          rep(0.9, 3)),
                        nrow = 2, byrow = T)
plotImageArray(recoloredImage(rc_ex))

plot(mask_list[[1]][,1],
        -mask_list[[1]][,2], type = "n",
     ann = F, axes = F, asp = 1)
polygon(mask_list[[1]][,1],
     -mask_list[[1]][,2])
polygon(mask_list[[2]][,1],
       -mask_list[[2]][,2])

png("mask_outline.png", 
    width = 1428, height = 1200)
img <- readImage(original_images[10])
par(mar = rep(0, 4))
plotImageArray(img)
dims <- dim(img)
polygon(mask_list[[1]][ , 1]/dims[2],
       (dims[1] - mask_list[[1]][ , 2])/dims[1],
       border = "cyan", lwd = 16)
polygon(mask_list[[2]][ , 1]/dims[2],
        (dims[1] - mask_list[[2]][ , 2])/dims[1],
        border = "cyan", lwd = 10)
dev.off()
i <- 5
plotImageArray(jpeg::readJPEG(original_images[i]))
plot(rc_list$`pom_mac_4-0SL`$original_img)
plotImageArray(constructImage(rc_list$`pom_mac_4-0SL`$pixel_assignments, 
                              centers = matrix(c(rep(0.1, 3),
                                                 rep(0.9, 3)),
                                               nrow = 2, byrow = T)))

# spectra ####
library(pavo)
bug <- getspec("spectra/")
bug <- bug[ , c(1, 5:10)]
bug <- aggspec(bug, by = 3)
bug <- procspec(bug, "smooth")

# cream: 5-10
library(ggplot2)
# png("neolamp_reflectance_spectra.png",
#     res = 300, 
#     width = 5, height = 5, units = "in",
#     bg = "transparent")
x <- reshape2::melt(bug, id.vars = "wl", variable.name = "source", value.name = "reflectance")
p <- ggplot(x, aes(x = wl, y = reflectance, color = source)) +
  geom_line(lwd = 3) +
  scale_color_manual(values = c("#463F66",
                                "#E7D391")) +
  theme_classic(base_size = 20, base_family = "Arial") +
  guides(color = F) +
  xlab("Wavelength (nm)") + ylab("Reflectance (%)")
ggsave(p, 
       filename = "chrysochroa_spec.png",
       width = 5, height = 4,
      dpi = 300)

plot(bug, col = c("#463F66",
                  "#E7D391"),
     lwd = 6,
    ann = F)
title(xlab = "Wavelength (nm)", 
      ylab = "Reflectance (%)",
      line = 2.5, cex.lab = 1.5)
dev.off()