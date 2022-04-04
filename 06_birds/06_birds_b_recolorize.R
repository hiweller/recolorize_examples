# bookkeeping ####
library(recolorize)
library(pavo)

# I saved each of the output patternize RasterStacks as a PNG so we don't have
# to load them all at once:
birds <- dir("06_birds/patternize_images/", full.names = TRUE)

# initial segmentation ####
init_list <- vector("list", length(birds)) 
for (i in 1:length(init_list)) {
  rc <- recolorize2(birds[i], bins = 4, cutoff = 25,
                    plotting = FALSE)
  init_list[[i]] <- thresholdRecolor(rc, plotting = FALSE)
}

# aggregate palette ####
agg_palette <- do.call(rbind, lapply(init_list, function(i) i$centers))
agg_sizes <- do.call(c, lapply(init_list, function(i) i$sizes / sum(i$sizes)))

# get list of which colors to aggregate
l <- hclust_color(agg_palette, cutoff = 20,
                  channels = 1:3)

# calculate their weighted averages
pal_out <- matrix(NA, nrow = length(l), ncol = 3)
for (i in 1:length(l)) {
  if (length(l[[i]]) == 1) {
    pal_out[i, ] <- agg_palette[l[[i]], ]
  } else {
    pal_out[i, ] <- apply(agg_palette[l[[i]], ], 2, 
                          function(j) weighted.mean(j, w = agg_sizes[l[[i]]]))
  }
}

# drop color 5 - we determined from spectra that we wanted to segment into
# black, grey, yellow, brown, and navy blue:
impose_pal <- pal_out[-5, ]
plotColorPalette(impose_pal)

# imposeColors ####
impose_list <- vector("list", length = length(birds)) 
for (i in 1:length(impose_list)) {
  img <- blurImage(readImage(birds[i]), "blur_anisotropic", 
                   amplitude = 50, sharpness = 0.3, 
                   plotting = F)
  impose_list[[i]]  <- imposeColors(img, impose_pal, 
                                    adjust_centers = FALSE, 
                                    plotting = FALSE)
}
names(impose_list) <- stringr::str_extract(birds, "[0-9]{5,7}")

#### COOKING SHOW INTERLUDE ####

# The maps as output are pretty good, but they required some manual tweaking since
# the feathers etc created texture which would interfere with our adjacency/boundary
# strength calculations.

# To address this, I just iterated through all of the maps  and made some edits
# (a combination of the lines commented out below to individual maps where
# ruffled feathers, etc caused problems) and saved the edited map each time; you
# can see the calls used to recreate each map by loading the impose_list.rds
# file (as in 04b)
i <- 1
rc <- impose_list[[i]]
plot(rc); print(names(impose_list)[i])

# uncomment and edit layer IDs, size condition, etc:

# this line would switch of color 2 to color 1:
# rc$pixel_assignments[which(rc$pixel_assignments == 2)] <- 1

# this line would absorb speckles of color 2 below the size_condition threshold:
# rc <- absorbLayer(rc, 2, size_condition, plotting = FALSE)

# plot result and (if  it looks good) slot it back into the list:
plot(rc, sizes = F)
impose_list[[i]] <- rc

# save result:
# saveRDS(impose_list, "images/birds/impose_list.rds")
