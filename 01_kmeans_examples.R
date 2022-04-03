# Examples of the kinds of images that don't work well with k-means clustering,
# as shown in Fig. 2

# images ####

# Pygoplites diacanthus 
# photo by Jack E. Randall
# from: http://pbs.bishopmuseum.org/images/JER/detail.asp?size=i&cols=10&ID=1432937402
fish <- "02_angelfish/fish_JRandall.png"

# Micrurus sp. 
# courtesy Alison Davis-Rabosky at University of Michigan EEB
snake <- "03_snake/snake_DavisRabosky.png"

# The same beetle (Chrysochroa mniszechii), under diffuse & direct light
# Photos by Nathan P. Lord & Able Chow at Louisiana State University
lighting <- dir("04_lighting_variation/", "png", full.names = TRUE)

# Social wasps (Polistes fuscatus)
# Photos by James Tumulty at Cornell University
wasps <- dir("05_wasps//wasp_faces/", "png", full.names = TRUE)

# Five colorful beetles (Chrysochroa spp.) 
# Photos by Nathan P. Lord & Able Chow at Louisiana State University
beetles <- dir("07_beetles/", "png", full.names = TRUE)

# k-means clustering ####

# load library
library(recolorize)

# Pygoplites angelfish (Fig. 2A)
fish_k <- recolorize(fish, method = "k", n = 4, plotting = TRUE)

# Note that if we run the same line multiple times, we get slightly different
# colors in a random order (sometimes you get very different colors):
fish_k <- recolorize(fish, method = "k", n = 4, plotting = TRUE)
fish_k <- recolorize(fish, method = "k", n = 4, plotting = TRUE)
fish_k <- recolorize(fish, method = "k", n = 4, plotting = TRUE)

# Micrurus snake (Fig. 2B)
snake_k <- recolorize(snake, method = "k", n = 3, plotting = TRUE)

# lighting variation (Fig. 2C)
diffuse_k <- recolorize(lighting[1], rotate = 90, method = "k", 
                        n = 2, color_space = "Lab")
direct_k <- recolorize(lighting[2], rotate = 90, 
                       method = "k", n = 2, color_space = "Lab")

# Polistes wasps (Fig. 2D)
wasp_k <- lapply(wasps, function(i) recolorize(i, method = "k",
                                               rotate = 180, n = 3))
# ^ note that these images have already been centered/scaled/masked using patternize
# (shown later in the wasp example) - I'm using them here so that we only do 
# color segmentation on the region of interesting (frons + clypeus)

# we can look at all the color palettes in aggregate:
layout(matrix(1:20, nrow = 5)); par(mar = c(1, 0, 0, 0))
for (i in 1:length(wasp_k)){
  plotColorPalette(wasp_k[[i]]$centers)
}

# Chrysochroa beetles
# we're resizing by 50% (resize = 0.5) so it runs a bit faster for the example
beetles_k <- lapply(beetles, function(i) recolorize(i, resize = 0.5, 
                                                    method = "k", n = 4,
                                                    plotting = FALSE))

# plot for comparison:
layout(matrix(1:15, nrow = 3, byrow = TRUE),
       heights = c(0.475, 0.475, 0.05)); par(mar = rep(0, 4))
lapply(beetles_k, function(i) plot(i$original_img))
lapply(beetles_k, function(i) plotImageArray(recoloredImage(i)))
lapply(beetles_k, function(i) plotColorPalette(i$centers, cex_text = 0.8))






