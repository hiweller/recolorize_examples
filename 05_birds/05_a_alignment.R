# Follows 04_a_alignment.R very closely 
library(patternize)

### Align set of 28 images ###

# set of specimen IDs
IDlist <- tools::file_path_sans_ext(dir("05_birds/images/"))

# make list with images
imageList <- makeList(IDlist, type = "image",
                      prepath = "05_birds/images/",
                      extension = ".JPG")

# make list with landmarks
landmarkList <- makeList(IDlist,
                         type = "landmark",
                         prepath = "05_birds/patternize_coordinates/",
                         extension = ".txt")

# Set target as LSUMZ 129286
target <- landmarkList[['Diglossa_Aug2021_LSUMZ129286_B_VIS']]

# Set up mask, which excludes everything but the breast/belly/neck (ventral)
mask1 <- read.table("05_birds/patternize_coordinates/mask.txt",
                    header = TRUE)

### Alignment ###
# this takes a few minutes on a 16Gb RAM laptop running Ubuntu, which is why
# it's commented out-- uncomment to run, but the output is already stored
# as a set of PNGs (to save on space) so you don't need this
imageList_aligned <- alignLan(imageList, landmarkList, transformRef = target,
                              adjustCoords = TRUE,
                              plotTransformed = F,
                              resampleFactor = 5, res = c(375, 1000),
                              cartoonID = 'Diglossa_Aug2021_LSUMZ129286_B_VIS',
                              maskOutline = mask1)
library(raster)
for (i in 1:length(imageList_aligned)) {
  extent(imageList_aligned[[i]]) <- c(0, 375, 0, 1000)
  
}

# save aligned bird breasts
saveRDS(imageList_aligned, file = '05_birds/rds_files/imageList_aligned.rds')

# given the size of the images, I also found it easier to save
# them as individual PNGs for color segmentation in the next step:
# for (i in 1:length(imageList_aligned)) {
#   img <- imageList_aligned[[i]]
#   img2 <- recolorize::brick_to_array(img)
#   plotImageArray(img2)
#   png::writePNG(img2, paste0("patternize_images/", names(imageList_aligned)[i], ".png"))
# }








