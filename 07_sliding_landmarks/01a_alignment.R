# load libraries
library(patternize)
library(raster)
library(StereoMorph)
library(geomorph)

# load in original landmarks
stereomorph_landmarks <- StereoMorph::readShapes("07_sliding_landmarks/landmarks/")

# we have landmarks.pixel, curves.control, and curves.pixel:
names(stereomorph_landmarks)

# sliding landmarks ####

# curve names:
names(stereomorph_landmarks$curves.pixel[[1]])

# define how many sliding landmarks we want per curve:
ncurvepts <- c(6, # head
               8, # eye
               6, # dorsal
               3, # caudal_dors
               3, # caudal_vent
               3, # operculum
               10) # midline

# then we can use geomorph just to automatically extract the sliding landmarks:
geomorph_landmarks <- geomorph::readland.shapes(stereomorph_landmarks, 
                                                # eye is a continuous curve:
                                                continuous.curve = 2, 
                                               nCurvePts = ncurvepts)
# ^ we get a warning about scaling - we can ignore this (we want the coordinates
# in pixels, not units, since we're going to use them to align the images, not
# make measurements)
landmark_list_sliding <- geomorph_landmarks$landmarks

# notice that compared to the fixed landmark case, we also have all these points
# labeled "curveLM":
landmark_list_sliding$aristochromis_christyi

# plot an example
img <- as.raster(jpeg::readJPEG("07_sliding_landmarks/images/haplochromis_sauvagei.jpeg"))
example_pts <- landmark_list_sliding$haplochromis_sauvagei

# we have to reverse the y coordinates (in images, y = 0 is at the top):
example_pts[ , 2] <- dim(img)[1] - example_pts[ , 2]

# and plot:
plot(img); title("Sliding landmarks", line = 0)
fixed_vs_sliding = factor(grepl("curve", rownames(example_pts)))
points(example_pts, pch = 21, col = "black", bg = c("orchid", "white")[fixed_vs_sliding], cex = c(1, 0.7)[fixed_vs_sliding])
legend(0, 300, fill = c("orchid", "white"), legend = c("Fixed", "Sliding"), cex = 0.8, bty = "n")

# get rid of the duplicate eye landmark (otherwise this causes problems later)
# 1. if you're also using a continuous curve, change "eye_end" to the name of one of 
#    the landmarks anchoring your continuous curve
# 2. IF YOU ARE NOT USING A CONTINUOUS CURVE YOU DON'T HAVE TO DO THIS PART AT ALL
eye_idx <- grep("eye_end", rownames(landmark_list_sliding[[1]]))
landmark_list_sliding <- lapply(landmark_list_sliding, \(x) x[-eye_idx, ])

# alignment ####
id_list <- names(landmark_list_sliding)
image_list <- makeList(IDlist = id_list, 
                       type = "image", 
                       prepath = "07_sliding_landmarks/images/", 
                       extension = ".jpeg")

# make mask
mask <- read.table("07_sliding_landmarks/haplochromis_sauvagei_mask.txt")

# and run alignment
alignedImages <- patternize::alignLan(image_list, 
                                      landmark_list_sliding,
                                      id_list,
                                      adjustCoords = FALSE,
                                      res = c(600, 260),
                                      resampleFactor = 1,
                                      transformRef = landmark_list_sliding[['haplochromis_sauvagei']], 
                                      maskOutline = mask,
                                      cartoonID = 'haplochromis_sauvagei',
                                      plotTransformed = TRUE)
alignedImages <- lapply(alignedImages, raster::flip)

layout(matrix(1:20, nrow = 5)); par(mar = rep(1, 4))
lapply(alignedImages, \(x) plotRGB(x, asp = 1))


saveRDS(alignedImages, 
        file = paste0("07_sliding_landmarks/rds_files/aligned_sliding_no_head.rds"))
