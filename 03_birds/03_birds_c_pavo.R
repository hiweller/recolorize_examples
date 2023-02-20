# bookkeeping ####
library(recolorize)
library(pavo)

# load color maps
bird_rc <- readRDS("03_birds/rds_files/impose_list.rds")

# adjust colors so they're a bit easier to distinguish for visualization:
new_pal <- adjust_color(bird_rc[[1]]$centers,
                        saturation = c(1, 0, 1.2, 1.3, 2),
                        brightness = c(1, 1, 1, 1, 2), plotting = T)
new_pal_hex <- rgb(new_pal[,1], new_pal[,2], new_pal[,3])
# again, because we're using these for visualization (not analysis),
# this is a fine alteration to make

# load spectra
# these have already been aggregated, smoothed, and set in the same
# order as the color palette:
spectra <- readRDS("03_birds/rds_files/spectra.rds")
# color "1" is the background spectrum (we have set it to all 0's)

plot(spectra, col = c("white", new_pal_hex), lwd = 4)
# ^ note that the navy blue and black colors are almost indistinguishable to us,
# but the navy blue has a much higher UV reflectance than the black

# loading spectra & generating coldist objects ####

# note that all the spectra ID's have been shifted up by 1, 
# because pavo requires a spectrum for the background color
# so spectrum 1 = background, spectrum 2 = color 1 in the color maps,
# etc; in this case:
# 2 = black (color 1)
# 3 = grey (color 2)
# 4 = yellow (color 3)
# 5 = brown (color 4)
# 6 = navy blue (color 5)
head(spectra)

# dog vismodel/coldist:
# generate vismodel:
dog_vm <- vismodel(spectra, visual = "canis",
                   achromatic = "cf.r")

# we need the background color to be non-NaN, even though we'll ignore it later:
dog_vm[1, ] <- 0 

# calculate colspace object:
dog_hsl <- colspace(dog_vm)

# 0 for the background color, as before:
dog_hsl[1, ] <- 0

# make hsl dataframe:
dog_hsl <- data.frame(patch = 1:nrow(dog_hsl), 
                      hue = dog_hsl$x, 
                      sat = dog_hsl$r.vec, 
                      lum = dog_hsl$lum)

# make coldist object:
dog_coldist <- coldist(dog_vm, achromatic = TRUE,
                       n = c(1, 1))

# set column names:
colnames(dog_coldist)[1:2] <- c("c1", "c2")

# image coldist
# this is not necessarily something you should do, even to represent human vision;
# here we're making the point that you miss important information by using RGB values rather than
# spectra (but outputs tend to be generally positively correlated)
classify_obj <- classify_recolorize(bird_rc[[1]])
default_coldist <- recolorize:::cielab_coldist(attr(classify_obj, "classRGB"))
default_hsl <- recolorize:::rgb2hsl(attr(classify_obj, "classRGB"))
# ^ the triple colon indicates a function that is not exported from a package
# (because these functions should only be used in exceptional cases)

# adjacency ####
# defaults (based on RGB colors; rough approximation of human vision)
default_adjacency <- lapply(bird_rc,
                            function(i) adjacent(classify_recolorize(i),
                                                 coldists = default_coldist,
                                                 bkgID = 1, 
                                                 xscale = 1, hsl = default_hsl))
saveRDS("03_birds/rds_files/default_adjacency.rds", object = default_adjacency)

# dichromatic dog
dog_adjacency <- lapply(bird_rc,
                        function(i) adjacent(classify_recolorize(i), 
                                             coldists = dog_coldist,
                                             bkgID = 1,
                                             xscale = 1, hsl = dog_hsl))
saveRDS("03_birds/rds_files/dog_adjacency.rds", object = dog_adjacency)

# plotting results ####
dog_df <- do.call(rbind, lapply(dog_adjacency,
                                function(i) i[ , grep("m_dS|m_dL|m_sat|m_lum", colnames(i))]), )
default_df <- do.call(rbind, lapply(default_adjacency,
                                    function(i) i[ , grep("m_dS|m_dL|m_sat|m_lum", colnames(i))]), )

# make images with our easier-to-distinguish color centers:
images <- lapply(bird_rc, function(i) constructImage(i$pixel_assignments, new_pal))

# make them ever-so-slightly transparent to cope with overlapping points:
for (i in 1:length(images)) {
  images[[i]][,,4][which(images[[i]][,,4]==1)] <- 0.90
}

# and rotate:
images <- lapply(images, as.raster)
images <- lapply(images, function(i) t(apply(i, 2, rev)))
images <- lapply(images, function(i) apply(i, 2, rev))

# plot luminance
plot(default_df$m_lum, 
     dog_df$m_lum, 
     type = "n",
     xlab = "Image RGB",
     ylab = "Dog visual model",
     main = "Mean pattern luminance")

# add line of best fit:
abline(lm(dog_df$m_lum ~ default_df$m_lum),
       lty = 2, col = "darkcyan")

# plot points as images:
for (i in 1:length(images)) {
  add_image(images[[i]], 
            x = default_df$m_lum[i], 
            y = dog_df$m_lum[i], 
            width = diff(par("usr")[1:2])*0.03)
}

# note that the reddish birds appear much less bright to the dog visual model
# than they do to a trichromatic human viewer looking at the image! (compare
# residuals, not absolute units--luminance is calculated differently for a vismodel
# then from imageRGB)
