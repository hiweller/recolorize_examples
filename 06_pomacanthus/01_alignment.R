# Part 1: alignment
library(patternize)
library(recolorize)
library(raster)

### Align set of 12 images ####

# set of specimen IDs
IDlist <- tools::file_path_sans_ext(dir("06_pomacanthus/images/", ".jpg"))

# make list with images
imageList <- makeList(IDlist, type = "image",
                      prepath = "06_pomacanthus/images/",
                      extension = ".jpg")

# make list with landmarks
landmarkList <- makeList(IDlist,
                         type = "landmark",
                         prepath = "06_pomacanthus/txt",
                         extension = ".txt")

# Set target as P. semicanthus 6.9cm SL
target <- landmarkList[['pom_sem_6-9SL']]

# Set up mask, which excludes eyes/mandibles and antenna holes
body_mask <- read.table("06_pomacanthus/masks/pom_sem_6-9SL_outline_1.txt", header = FALSE)
eye_mask <- read.table("06_pomacanthus/masks/pom_sem_6-9SL_outline_2.txt", header = FALSE)
dims <- round(apply(body_mask, 2, \(x) diff(range(x))))

### Alignment ###
# this takes ~1 minute on a 16Gb RAM laptop running Ubuntu
imageList_aligned <- alignLan(imageList, landmarkList, 
                              transformRef = target, 
                              adjustCoords = TRUE,
                              plotTransformed = T, 
                              resampleFactor = 1, 
                              res = dims, 
                              cartoonID = 'pom_sem_6-9SL',
                              maskOutline = list(body_mask, eye_mask), 
                              inverse = list(FALSE, TRUE))

# set extents again
for (i in 1:length(imageList_aligned)) {
  extent(imageList_aligned[[i]]) <- c(0, dims[1], 0, dims[2])
}

# flip Y:
for (i in 1:length(imageList_aligned)) {
  imageList_aligned[[i]] <- flip(imageList_aligned[[i]])
}

# save aligned images
saveRDS(imageList_aligned, file = '06_pomacanthus/rds_files/imageList_aligned.rds')

# convert from RasterBricks to image arrays:
imgs <- lapply(imageList_aligned, brick_to_array)
names(imgs) <- names(imageList_aligned)

# raster extents:
extent_list <- lapply(imageList_aligned, extent)

# save both:
saveRDS(imgs, file = "06_pomacanthus/rds_files/image_arrays.rds")
saveRDS(extent_list, file = "06_pomacanthus/rds_files/extent_list.rds")
