# bookkeeping ####
library(pavo)
library(recolorize)

# read in files
spectra <- readRDS("05_birds/rds_files/spectra.rds")
rc_list <- readRDS("05_birds/rds_files/impose_list.rds")

# note that we've got two variables for this analysis

# 1. two different sets of spectra (one for each specimen)
names(spectra)

# 2. the two different sources of chromatic data:
# a. spectral reflectance (using images only for spatial data)
# b. RGB values from the image (using images for spatial & chromatic data)

# Visual models (vismodel) & color distances (coldist) ####

# read in human photoreceptor sensitivity data from Stockman & Sharpe (2000)
human_receptors <- read.csv("05_birds/StockmanSharpe2000_human_receptors_400-700.csv", header = TRUE)

# for each set of spectra, make a vismodel object from human photoreceptor
# sensitivity data; we're using MWS + LWS for the achromatic channel for humans,
# following convention
vismodels <- lapply(spectra, \(x) vismodel(x[-(1:100), ],
                                           visual = human_receptors,
                                           achromatic = "ml"))

# ensure that the background "spectrum" is not NaN (we won't use this, but it's
# important to avoid some errors later)
for (i in 1:length(vismodels)) {
  colnames(vismodels[[i]]) <- c("s", "m", "l", "lum")
  vismodels[[i]][1, ] <- 0
}

# make lists for coldist and hsl objects:
coldists <- setNames(vector("list", length(vismodels)),
                     names(vismodels))
hsl_list <- coldists

# for each vismodel...
for (i in 1:length(vismodels)) {
  
  # color distances - we don't use these in the paper but still informative:
  coldists[[i]] <- coldist(vismodels[[i]], achromatic = TRUE,
                           n = c(0.629, 0.314, 0.057), 
                           weber = c(0.02, 0.0282842712, 0.0662973453)) # Hofer et al. 2005
  colnames(coldists[[i]])[1:2] <- c("c1", "c2")
  
  # HSL dataframe
  cp <- colspace(vismodels[[i]])
  hsl_list[[i]] <- data.frame(patch = 1:nrow(cp), 
                              hue = cp$h.theta, 
                              sat = cp$r.vec, 
                              lum = cp$lum)
}

# running adjacency analyses ####

# make an empty list
adjacency_dfs <- setNames(vector("list", length(vismodels)),
                          names(vismodels))

# we want to compare photos of the same individual across conditions,
# so extract the specimen IDs:
ids <- substr(names(vismodels), 7, 13)

# for each image...
for (i in 1:length(rc_list)) {
  
  # get appropriate coldist and hsl
  ref_idx <- which(sapply(ids, \(x) grepl(x, names(rc_list)[i])))
  spec <- spectra[[ref_idx]]
  cd <- coldists[[ref_idx]]
  hsl <- hsl_list[[ref_idx]]
  
  # run adjacency analysis
  zm <- rc_list[[i]]
  
  # order colors correctly
  if (nrow(zm$centers) > 1) {
    o <- match_colors(spec2rgb(spec)[-1], zm$centers)
    zm <- reorder_colors(zm, col_order = o)
  }
  
  # run adjacency
  a <- pavo::adjacent(classify_recolorize(zm), 
                      bkgID = 1, xscale = 1, 
                      coldists = cd, 
                      hsl = hsl)
  
  # append
  new_row_adj <- data.frame(imname = names(rc_list)[i],
                            m_lum = a$m_lum,
                            source = "spectra",
                            museumID = names(ref_idx))
  
  # calculate LAB values from image
  pixels <- zm$original_img[which(zm$pixel_assignments > 0)]
  rgb_pixels <- t(col2rgb(pixels) / 255)
  lab_pixels <- recolorize:::col2col(rgb_pixels, from = "sRGB", to = "Lab")
  
  # append
  new_row_rgb <- data.frame(imname = names(rc_list)[i],
                            m_lum = mean(lab_pixels[ , 1]),
                            source = "rgb",
                            museumID = names(ref_idx))
  
  # make (or start) hsl_comparison_df
  if (i == 1) {
    hsl_comparison_df <- rbind(new_row_adj,
                               new_row_rgb)
  } else {
    hsl_comparison_df <- rbind(hsl_comparison_df,
                               new_row_adj,
                               new_row_rgb)
  }
}


# compiling results ####

# the image names that start with "Diglossa" are from the original,
# color-calibrated and luminance-normalized dataset
ref_idx <- grep("Diglossa", hsl_comparison_df$imname)
ref_df <- hsl_comparison_df[ref_idx, ]

# make a new column for relative luminance
hsl_comparison_df$rel_lum <- NA

# for every image...
for(i in 1:nrow(hsl_comparison_df)) {
  
  # find the appropriate reference from the reference dataframe
  idx <- which(ref_df$museumID == hsl_comparison_df$museumID[i] &
                 ref_df$source == hsl_comparison_df$source[i])
  
  # calculate relative luminance as a fraction of the reference value
  hsl_comparison_df$rel_lum[i] <- hsl_comparison_df$m_lum[i] / ref_df$m_lum[idx]
  
}

# reorder so reference images are first
hsl_comparison_df <- hsl_comparison_df[c(ref_idx,
                                         setdiff(1:nrow(hsl_comparison_df), ref_idx)), ]
hsl_comparison_df$imageID <- factor(hsl_comparison_df$imname,
                                    levels = levels(factor(hsl_comparison_df$imname))[c(5, 6, 1:4, 7, 8)],
                                    labels = c("E", "A", "F", "B",
                                               "G", "C", "H", "D"))

# save output
write.csv(hsl_comparison_df, "05_birds/hsl_comparison_df.csv", row.names = FALSE)

# visualize/analyze results ####
hsl_comparison_df <- read.csv("05_birds/hsl_comparison_df.csv")

# reorder so reference images are first
hsl_comparison_df <- hsl_comparison_df[c(ref_idx,
                                         setdiff(1:nrow(hsl_comparison_df), ref_idx)), ]
hsl_comparison_df$imageID <- factor(hsl_comparison_df$imname,
                                    levels = levels(factor(hsl_comparison_df$imname))[c(5, 1, 3, 7,
                                                                                        6, 2, 4, 8)],
                                    labels = c(LETTERS[5:8], LETTERS[1:4]))

hsl_comparison_df$museumID <- factor(hsl_comparison_df$museumID, labels = c("LSUMZ 196409",
                                                                            "LSUMZ 129286"),
                                     levels = c("196409", "129286"))

# Luminance
library(ggplot2)
ggplot(hsl_comparison_df,
       aes(y = abs((1 - rel_lum)*100), 
           x = imageID, fill = source)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(name = "Source",
                    values = c("coral", "darkturquoise"),
                    labels = c("Image RGB",
                               "Spectra +\nzone map")) +
  geom_hline(yintercept = 0, lty = 2) +
  facet_wrap(~museumID*source, scales = "free_x") +
  ylab("% difference from calibrated image") +
  xlab("Image") +
  ggtitle("Saturation") +
  theme_bw()
