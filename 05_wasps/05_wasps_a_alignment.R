library(patternize)

### Align set of 20 images ###

# set of specimen IDs
IDlist <- tools::file_path_sans_ext(dir("05_wasps/original_images/", ".jpg"))

# make list with images
imageList <- makeList(IDlist, type = "image",
                      prepath = "05_wasps/original_images/",
                      extension = ".jpg")

# make list with landmarks
landmarkList <- makeList(IDlist,
                         type = "landmark",
                         prepath = "05_wasps/landmarks/",
                         extension = "_landmarks.txt")

# Set target as polistes 05
target <- landmarkList[['polistes_05']]

# Set up mask, which excludes eyes/mandibles and antenna holes
mask1 <- read.table("05_wasps/masks/polistes_05_mask.txt", header = FALSE)
mask2 <- read.table("05_wasps/masks/polistes_05_Lantenna.txt", header = FALSE)
mask3 <- read.table("05_wasps/masks/polistes_05_Rantenna.txt", header = FALSE)

### Alignment ###
# this takes ~1 minute on a 16Gb RAM laptop running Ubuntu
imageList_aligned <- alignLan(imageList, landmarkList, transformRef = target, 
                              adjustCoords = TRUE,
                              plotTransformed = T, 
                              resampleFactor = 5, 
                              cartoonID = 'polistes_05',
                              maskOutline = list(mask1, mask2, mask3), 
                              inverse = list(FALSE, TRUE, TRUE))


#save aligned faces
saveRDS(imageList_aligned, file = '05_wasps/rds_files/imageList_aligned.rds')

