# load libraries
library(patternize)
library(raster)
library(StereoMorph)
library(geomorph)

# load in original landmarks
stereomorph_landmarks <- StereoMorph::readShapes("07_sliding_landmarks/landmarks/")

# we have landmarks.pixel, curves.control, and curves.pixel:
names(stereomorph_landmarks)

# fixed landmarks ####
# to make a version with only fixed landmarks, we just extract them from the
# landmarks.pixel object:
landmark_list_fixed <- lapply(seq(dim(stereomorph_landmarks$landmarks.pixel)[3]),
                              function(x) stereomorph_landmarks$landmarks.pixel[ , , x])

# name the list:
names(landmark_list_fixed) <- dimnames(stereomorph_landmarks$landmarks.pixel)[[3]]

# we can see each element of the list is a set of XY coordinates:
landmark_list_fixed$aristochromis_christyi

# and we can plot an example:
img <- as.raster(jpeg::readJPEG("07_sliding_landmarks/images/haplochromis_sauvagei.jpeg"))
example_pts <- landmark_list_fixed$haplochromis_sauvagei

# we have to reverse the y coordinates (in images, y = 0 is at the top):
example_pts[ , 2] <- dim(img)[1] - example_pts[ , 2]

# and plot:
plot(img); title("Fixed landmarks only", line = -0)
points(example_pts, pch = 21, col = "black", bg = "orchid")

# get rid of the duplicate eye landmark (otherwise this causes problems later):
# if you're also using a continuous curve, change "eye_end" to the name of one of 
# the landmarks anchoring your continuous curve
# IF YOU ARE NOT USING A CONTINUOUS CURVE YOU DON'T HAVE TO DO THIS PART AT ALL
eye_idx <- grep("eye_end", rownames(landmark_list_fixed[[1]]))
landmark_list_fixed <- lapply(landmark_list_fixed, \(x) x[-eye_idx, ])

# alignment ####
id_list <- names(landmark_list_fixed)
image_list <- makeList(IDlist = id_list, 
                       type = "image", 
                       prepath = "07_sliding_landmarks/images/", 
                       extension = ".jpeg")

# make mask
# mask <- read.table("07_sliding_landmarks/haplochromis_sauvagei_mask_including_head.txt")
mask <- read.table("07_sliding_landmarks/haplochromis_sauvagei_mask.txt")
mask_dims <- diff(apply(mask, 2, range))
mask_asp <- mask_dims[1] / mask_dims[2]

# run alignment:
# and run alignment
alignedImages <- alignLan(image_list, 
                          landmark_list_fixed,
                          id_list,
                          adjustCoords = FALSE,
                          res = c(260 * mask_asp, 260),
                          resampleFactor = 1,
                          transformRef = landmark_list_fixed[['haplochromis_sauvagei']], 
                          maskOutline = mask,
                          cartoonID = 'haplochromis_sauvagei',
                          plotTransformed = TRUE)
alignedImages <- lapply(alignedImages, raster::flip)

# look at the results:
layout(matrix(1:20, nrow = 5)); par(mar = rep(1.5, 4))
lapply(alignedImages, \(x) plotRGB(x, asp = 1))

# save:
saveRDS(alignedImages, file = paste0("07_sliding_landmarks/rds_files/aligned_fixed_landmarks_no_head.rds"))
