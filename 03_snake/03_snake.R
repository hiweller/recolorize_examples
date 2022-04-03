# Running example B: a very shiny snake

# image:
snake <- "03_snake/snake_DavisRabosky.png"

# load library
library(recolorize)

# Blurring ####

# blurring helps mitigate the color variation due to texture (scales)
# we're using a median blur with a filter size of 8 pixels, 
# but you can use any of the imager blur functions (see function documentation)
s0 <- blurImage(readImage(snake), 
                "medianblur", n = 8)

# basic recolorize step ####
s1 <- recolorize2(s0, bins = 2, cutoff = 20, color_space = "sRGB")

# this looks alright, but the shininess is still a problem:
plot(s1)

# manual adjustments ####

# combine clusters 1 and 2 (1 is actually closer to 4 in color space, so we 
# couldn't do this automatically):
s2 <- mergeLayers(s1, list(c(1, 2))) 

# absorb the stray speckles of color 2 (white)
# in this case, we're only targeting speckles that are less than 0.3% the number
# of pixels in the image
s3 <- absorbLayer(s2, 2, function(s) s <= round(sum(s1$sizes) * 0.003))
