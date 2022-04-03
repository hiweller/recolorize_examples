# Running example C: getting the same color pattern data out of different
# lighting

# images:
lighting <- dir("04_lighting_variation/", "png", full.names = TRUE)

# load library
library(recolorize)

# diffuse light ####

# turn plotting to TRUE to see intermediate steps - they're off here to speed
# up the code
diffuse_init <- recolorize2(lighting[1], bins = 3, cutoff = 40, rotate = 90, 
                       plotting = FALSE)
diffuse_merge <- mergeLayers(diffuse_init, list(c(1, 4), c(2, 3)),
                        plotting = FALSE)
diffuse_final <- absorbLayer(diffuse_merge, 2, size_condition = function(s) s <= 500,
                        plotting = FALSE)

# view the final result:
plot(diffuse_final)

# direct light ####
# CIE Lab space worked better for this image than sRGB did
direct_init <- recolorize2(lighting[2], bins = 4, cutoff = 50, rotate = 90,
                           color_space = "Lab",
                           plotting = FALSE)
direct_merge <- mergeLayers(direct_init, list(c(1, 2), c(3, 4)),
                            plotting = FALSE)
direct_edit <- editLayer(direct_merge, 1, "fill", 3, 
                         plotting = FALSE)
direct_final <- absorbLayer(direct_edit, 2, size_condition = function(s) s <= 500,
                            plotting = FALSE)

# view the final result:
plot(direct_final)

# creating a shared color palette ####

# we can see that although the maps provide more or less the same spatial
# information, the colors are still different (this may or may not matter
# depending on your goal)
layout(matrix(1:2, nrow = 1)); par(mar = rep(0, 4))
plotImageArray(recoloredImage(diffuse_final)) 
plotImageArray(recoloredImage(direct_final))

# this is somewhat arbitrary, but if you want the color maps to be uniform (e.g.
# for visualization), you can provide an arbitrary set of color centers - in
# this case, we'll take the average of the two sets of color maps:
shared_color_palette <- (diffuse_final$centers + direct_final$centers) / 2

# we can swap out the $centers argument of the recolorize objects above with the
# shared_color_palette, or we can just use constructImage if we just want the
# output image:
diffuse_matching <- constructImage(diffuse_final$pixel_assignments, 
                                   shared_color_palette)
direct_matching <- constructImage(direct_final$pixel_assignments, 
                           shared_color_palette)

# now we can plot them and see they look the same:
layout(matrix(1:2, nrow = 1)); par(mar = rep(0, 4))
plotImageArray(diffuse_matching); plotImageArray(direct_matching)

# calculating image distances ####
dist0 <- imDist(raster_to_array(direct_final$original_img), 
                raster_to_array(diffuse_final$original_img),
                color_space = "sRGB",
                plotting = TRUE)
dist1 <- imDist(recoloredImage(diffuse_merge),
                recoloredImage(direct_merge), 
                color_space = "sRGB",
                plotting = TRUE)
dist2 <- imDist(recoloredImage(diffuse_final),
                recoloredImage(direct_final),
                color_space = "sRGB",
                plotting = TRUE)
dist_final <- imDist(diffuse_matching, direct_matching,
                     color_space = "sRGB", plotting = TRUE)

# and we can see how the color palette differences decrease over time:
layout(matrix(1:4, nrow = 1))
p <- viridisLite::viridis(100)

# plotting with imHeatmap lets us set the color range:
imHeatmap(dist0, range = c(0, sqrt(3)), legend = FALSE, palette = p)
imHeatmap(dist1, range = c(0, sqrt(3)), legend = FALSE, palette = p)
imHeatmap(dist2, range = c(0, sqrt(3)), legend = FALSE, palette = p)
imHeatmap(dist_final, range = c(0, sqrt(3)), legend = FALSE, palette = p)
