# Fig. 2: Pygoplites diacanthus angelfish

# image taken by John E. Randall, courtesy of the Bishop Museum:
fish <- "02_angelfish/fish_JRandall.png"

# load library
library(recolorize)

# initial clustering ####
# initial binning (in this case we're using CIE Lab space; other option is RGB)
f1 <- recolorize(fish, bins = 3, color_space = "Lab")

# object f1 is an S3 class recolorize object:
class(f1)

# and has its own plotting method:
plot(f1, sizes = FALSE)

# notice that we have 27 color centers:
f1$centers

# ...but most of them have 0 pixels assigned to them:
f1$sizes

# reclustering ####
# we can combine colors with a CIE Lab distance less than 50:
f2 <- recluster(f1, cutoff = 50)

# we could also just explicitly request 4 colors (yields the same thing here):
f3 <- recluster(f1, n_final = 4)

# we can also run both of these functions in sequence with recolorize2:
f4 <- recolorize2(fish, bins = 3,
                  color_space = "Lab",
                  cutoff = 50)

# as a general note, ordinarily you will need a biological basis for what color 
# classes you want to segment (rather than just aesthetics) - here we are using
# blue, yellow, black, and white, since these three colors have been shown
# (by Justin Marshall) to be important & distinguishable on the reef,
# but for a study of multiple images you would want a more formal biological
# basis for segmentation, such as cell types or established genetic heritability

# output ####
# finally, we can output our map as a series of binary mask layers:
layout(matrix(1:4, nrow = 2)); par(mar = rep(1, 4))
masks <- splitByColor(f2, plot_method = "binary") # try out different plotting methods!

# the masks are just matrices, 1 = color presence and 0 = color absence
# you could save the binary masks as pngs with the following code:
for (i in 1:length(masks)) {
  img_name <- paste0("02_angelfish/angelfish_color_patch_", i, ".png")
  png::writePNG(masks[[i]], img_name)
}
