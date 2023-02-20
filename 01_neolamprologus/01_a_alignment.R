# Example 1: Color pattern traits associated with species

# This example uses recolorize to color segment images of Neolamprologus brichardi and 
# N. pulcher, two species of social cichlid fishes in Lake Tanganyika
# which use their facial color patterns for social recognition. We also compare
# the segmentation using recolorize to standard k-means clustering and aggregated
# k-means clustering, then compare how well these three approaches successfully
# separate these two species on the basis of their facial color patterns.

# IMPORTANT: All images used in this example are courtesy Ad Konings
# (https://www.cichlidpress.com/), used with his permission; any reproduction of
# this example or these images must credit the original photographer!

# Step 1: Aligning images
# load library
library(patternize)

# set of specimen IDs
IDlist <- tools::file_path_sans_ext(dir("01_neolamprologus/images/", ".txt"))

# make list with images
imageList <- makeList(IDlist, type = "image",
                      prepath = "01_neolamprologus/images/",
                      extension = ".jpg")

# make list with landmarks
landmarkList <- makeList(IDlist,
                         type = "landmark",
                         prepath = "01_neolamprologus/images/",
                         extension = ".txt")

# We'll use the Cape Nambeyeye image as our reference:
target <- landmarkList[["Neo_brichardi_CapeNambeyeye"]]

# Set up mask (fish head minus the eye):
mask1 <- read.table("01_neolamprologus/images/masks/brichardi_nambeyeye_mask1.txt", header = FALSE)
mask2 <- read.table("01_neolamprologus/images/masks/brichardi_nambeyeye_mask2.txt", header = FALSE)

# get the width and height of the mask to maintain aspect ratio for our sampling (~3:2)
asp <- round(diff(apply(mask1, 2, range)))

# align images
# THIS IS COMMENTED OUT BY DEFAULT! This step takes 20-25 minutes to run on my
# Ubuntu laptop with 16Gb of RAM, since it's a relatively complicated process,
# so to run the tutorial I recommend skipping it and moving onto 01_b_recolorize
# using the aligned image list; to run, uncomment lines 34-55 below:

# imageList_aligned <- alignLan(imageList, landmarkList,
#                               res = c(300, round(300 * res[2] / res[1])),
#                               transformRef = target, 
#                               adjustCoords = TRUE,
#                               plotTransformed = T, 
#                               resampleFactor = 1, 
#                               cartoonID = "Neo_brichardi_CapeNambeyeye",
#                               maskOutline = list(mask1, mask2), 
#                               inverse = list(FALSE, TRUE)) # <- this tells 
## alignLan to invert the second mask (which is the eye) -- so it removes the
## area inside the polygon, not outside it

## notice that the images are flipped on the x- and y-axes:
# patternize::plotRasterstackAsImage(imageList_aligned[[1]])

## we can orient them correctly by flipping the rasterStacks:
# for (i in 1:length(imageList_aligned)) {
#   imageList_aligned[[i]] <- flip(imageList_aligned[[i]])
# }

## save RDS file
# saveRDS(imageList_aligned, "01_neolamprologus/rds_files/imageList_aligned.rds")

# alternatively, just load the aligned images:
imageList_aligned <- readRDS("01_neolamprologus/rds_files/imageList_aligned.rds")

# we can plot these to see if they look reasonable:
layout(matrix(1:20, nrow = 4)); par(mar = rep(1, 4))
lapply(imageList_aligned, plotRasterstackAsImage)
# often, if any of the aligned images look really warped, it's because
# the landmarks for that image are in the wrong order--I recommend to double
# check landmark order first when troubleshooting (but in this case they look
# fine)
