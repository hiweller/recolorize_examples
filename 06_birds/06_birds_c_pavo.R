# bookkeeping ####
library(recolorize)
library(pavo)

# load color maps
bird_rc <- readRDS("06_birds/impose_list.rds")

# load spectra
# these have already been aggregated, smoothed, and set in the same
# order as the color palette 
spectra <- readRDS("06_birds/spectra.rds")
plot(spectra, col = rgb(bird_rc[[1]]$centers), lwd = 2)
# ^ note that the navy blue and black colors are almost indistinguishable to us,
# but the navy blue has a much higher UV reflectance than the black

# dropping some identical birds to make the example run faster
# (and because a 4x5 grid looks nice and this leaves us with 20 birds)
bird_rc <- bird_rc[-c(6, 10, 19, 20,
                      23, 24, 27, 28)] 

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

# bluetit vismodel/coldist:
# generate vismodel:
bluetit_vm <- vismodel(spectra, visual = "bluetit",
                       achromatic = "bt.dc")

# we need the background color to be non-NaN, even though we'll ignore it later:
bluetit_vm[1, ] <- 0 

# calculate colspace object:
bluetit_hsl <- colspace(bluetit_vm)

# 0 for background color, as before:
bluetit_hsl[1, ] <- 0

# make hsl dataframe:
bluetit_hsl <- data.frame(patch = 1:6, 
                          hue = bluetit_hsl$h.theta, 
                          sat = bluetit_hsl$r.vec, 
                          lum = bluetit_hsl$lum)

# and calculate coldist object, setting column names to appropriate values:
bluetit_coldist <- coldist(bluetit_vm, achromatic = TRUE)
colnames(bluetit_coldist)[1:2] <- c("c1", "c2")

# dog vismodel/coldist:
dog_vm <- vismodel(spectra, visual = "canis",
                   achromatic = "cf.r")
dog_hsl <- colspace(dog_vm)
dog_hsl[1, ] <- rep(1, ncol(dog_hsl))
dog_hsl <- data.frame(patch = 1:6, 
                      hue = dog_hsl$x, 
                      sat = dog_hsl$r.vec, 
                      lum = dog_hsl$lum)
dog_coldist <- coldist(dog_vm, achromatic = TRUE,
                       n = c(1, 1))
colnames(dog_coldist)[1:2] <- c("c1", "c2")

# and make a __VERY HACKY__ coldist & hsl from the RGB values:
classify_obj <- classify_recolorize(bird_rc[[1]])
default_coldist <- recolorize:::cielab_coldist(attr(classify_obj, "classRGB"))
default_hsl <- recolorize:::rgb2hsl(attr(classify_obj, "classRGB"))

# adjacency ####
# defaults (based on RGB colors)
default_adjacency <- lapply(bird_rc,
                            function(i) adjacent(classify_recolorize(i),
                                                 coldists = default_coldist,
                                                 bkgID = 1, 
                                                 xscale = 1, hsl = default_hsl))
saveRDS("06_birds/default_adjacency.rds", object = default_adjacency)

# tetrachromatic bluetit
bluetit_adjacency <- lapply(bird_rc,
                            function(i) adjacent(classify_recolorize(i), 
                                                 coldists = bluetit_coldist,
                                                 bkgID = 1,
                                                 xscale = 1, hsl = bluetit_hsl))
saveRDS("06_birds/bluetit_adjacency.rds", object = bluetit_adjacency)

# dichromatic dog
dog_adjacency <- lapply(bird_rc,
                        function(i) adjacent(classify_recolorize(i), 
                                             coldists = dog_coldist,
                                             bkgID = 1,
                                             xscale = 1, hsl = dog_hsl))
saveRDS("06_dog_adjacency.rds", object = dog_adjacency)

# plotting results ####
bluetit_df <- do.call(rbind, lapply(bluetit_adjacency,
                                    function(i) i[ , grep("m_dS|m_dL|m_sat|m_lum", colnames(i))]), )
dog_df <- do.call(rbind, lapply(dog_adjacency,
                                function(i) i[ , grep("m_dS|m_dL|m_sat|m_lum", colnames(i))]), )
default_df <- do.call(rbind, lapply(default_adjacency,
                                    function(i) i[ , grep("m_dS|m_dL|m_sat|m_lum", colnames(i))]), )


layout(matrix(1:2, nrow = 1))

# chromatic vs. achromatic boundary strength for our two real visual systems:
# chromatic:
plot(dog_df$m_dS, bluetit_df$m_dS,
     xlim = c(0.5, 3), ylim = c(1, 4.2),
     main = "Chromatic boundary strength",
     xlab = "Dichromat (dog)",
     ylab = "Tetrachromat (bluetit)")
abline(a = 0, b = 1, col = "tomato",
       lty = 2,
       lwd = 3) # not very close to a slope of 1!

# achromatic:
plot(dog_df$m_dL, bluetit_df$m_dL,
     xlim = c(5, 11), ylim = c(5, 11),
     main = "Achromatic boundary strength",
     xlab = "Dichromat (dog)",
     ylab = "Tetrachromat (bluetit)")
abline(a = 0, b = 1, col = "tomato",
       lty = 2,
       lwd = 3) # very close to a slope of 1!

# comparing default to tetrachromat visual system
# you could do this e.g. for a sample of images to test whether RGB images are
# giving you an accurate-enough sampling of your data

# comparing luminance:
plot(default_df$m_lum, bluetit_df$m_lum,
     main = "Mean luminance",
     xlab = "Image RGB",
     ylab = "Reflectance spectra")

# we're not plotting a slope of 1 (since these are different units), just seeing
# how correlated they are:
abline(lm(bluetit_df$m_lum ~ default_df$m_lum),
       lwd = 2, lty = 2, col = "dodgerblue")

# comparing saturation:
plot(default_df$m_sat, bluetit_df$m_sat,
     main = "Mean saturation",
     xlab = "Image RGB",
     ylab = "Reflectance spectra")
abline(lm(bluetit_df$m_sat ~ default_df$m_sat),
       lwd = 2, lty = 2, col = "dodgerblue")

# it's not perfect, but the values are reasonably tightly correlated
