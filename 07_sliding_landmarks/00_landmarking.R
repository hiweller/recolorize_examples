# Step 0: Landmarking
# This step uses the StereoMorph digitizing app; see the documentation
# for the package for how to use it: 
# https://aaronolsen.github.io/software/stereomorph/StereoMorph%20User%20Guide%20v1.6.1.pdf

# Load library
install.packages("StereoMorph") # install if not yet installed
library(StereoMorph)
image_directory <- "07_sliding_landmarks/images/"
landmark_directory <- "07_sliding_landmarks/landmarks/"

if (!dir.exists(landmark_directory)) { dir.create(landmark_directory) }

# define landmarks and curves:

# individual fixed landmarks not associated with a curve:
landmarks_ref <- c("pectoral_dors", "pectoral_vent", "pelvic_ant", "anal_ant", "premax_post")

# curves are defined in a 3-column matrix: curve name, curve start, curve end
curves_ref <- matrix(c("head", "premax_ant", "dorsal_fin_ant", # make a curve called 'head' which connects the premax to the start of the dorsal fin
                       "eye", "eye_start", "eye_end",
                       "dorsal", "dorsal_fin_ant", "dorsal_fin_post",
                       "caudal_dors", "dorsal_fin_post", "caud_pedunc_dors",
                       "caudal_vent", "caud_pedunc_vent", "anal_post",
                       "operculum", "operculum_apex", "interopercle",
                       "midline", "operculum_apex", "caud_pedunc_mid"),
                     ncol = 3, byrow = TRUE)

# opens the digitizing app:
digitizeImages(image.file = image_directory,
               shapes.file = landmark_directory, 
               landmarks.ref = landmarks_ref,
               curves.ref = curves_ref, landmark.color.blur = "cyan", landmark.color.focus = "yellow", 
               control.point.color.blur = "orchid", control.point.color.focus = "hotpink", 
               landmark.radius = 5)


