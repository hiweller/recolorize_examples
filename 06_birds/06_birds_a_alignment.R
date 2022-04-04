# Follows 05_wasps_a_alignment.R very closely in process
library(patternize)

### Align set of 28 images ###

# set of specimen IDs
IDlist <- tools::file_path_sans_ext(dir("06_birds/cropped_resized/", ".png"))

# make list with images
imageList <- makeList(IDlist, type = "image",
                      prepath = "06_birds/cropped_resized/",
                      extension = ".png")

# make list with landmarks
landmarkList <- makeList(IDlist,
                         type = "landmark",
                         prepath = "06_birds/patternize_coordinates/",
                         extension = ".txt")

# Set target as LSUMZ 129286
target <- landmarkList[['Diglossa_Aug2021_LSUMZ129286_B_VIS']]

# Set up mask, which excludes everything but the breast/belly/neck (ventral)
mask1 <- read.table("06_birds/XY_Diglossa_Aug2021_LSUMZ129286_B_VIS.txt",
                    header = TRUE)

### Alignment ###
# this takes ~5 minutes on a 16Gb RAM laptop running Ubuntu, which is why
# it's commented out-- uncomment to run, but the output is already stored
# as an .RDS file in this folder so you don't have to

# imageList_aligned <- alignLan(imageList, landmarkList, transformRef = target, 
#                               adjustCoords = TRUE,
#                               plotTransformed = T, 
#                               resampleFactor = 2, res = c(1000, 375),
#                               cartoonID = 'Diglossa_Aug2021_LSUMZ129286_B_VIS', 
#                               maskOutline = mask1)


# save aligned faces
# saveRDS(imageList_aligned, file = '06_birds/imageList_aligned.rds')









