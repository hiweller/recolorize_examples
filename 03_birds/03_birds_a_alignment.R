# Follows 02_wasps_a_alignment.R very closely in process
library(patternize)

### Align set of 28 images ###

# set of specimen IDs
IDlist <- tools::file_path_sans_ext(dir("03_birds/images/", ".png"))

# make list with images
imageList <- makeList(IDlist, type = "image",
                      prepath = "03_birds/images/",
                      extension = ".png")

# make list with landmarks
landmarkList <- makeList(IDlist,
                         type = "landmark",
                         prepath = "03_birds/patternize_coordinates/",
                         extension = ".txt")

# Set target as LSUMZ 129286
target <- landmarkList[['Diglossa_Aug2021_LSUMZ129286_B_VIS']]

# Set up mask, which excludes everything but the breast/belly/neck (ventral)
mask1 <- read.table("03_birds/mask.txt",
                    header = TRUE)

### Alignment ###
# this takes a few minutes on a 16Gb RAM laptop running Ubuntu, which is why
# it's commented out-- uncomment to run, but the output is already stored
# as a set of PNGs (to save on space) so you don't need this
# imageList_aligned <- alignLan(imageList, landmarkList, transformRef = target,
#                               adjustCoords = TRUE,
#                               plotTransformed = T,
#                               resampleFactor = 2, res = c(1000, 375),
#                               cartoonID = 'Diglossa_Aug2021_LSUMZ129286_B_VIS',
#                               maskOutline = mask1)


# save aligned bird breasts
# saveRDS(imageList_aligned, file = '03_birds/rds_files/imageList_aligned.rds')









