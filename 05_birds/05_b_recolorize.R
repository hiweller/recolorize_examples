# bookkeeping ####
library(recolorize)
library(pavo)

# load one image at a time (helpful if you have a large dataset)
birds <- dir("05_birds/patternize_images/", full.names = TRUE)

# segmentation ####
# like example A, these images have lots of (specifically chromatic)
# variation, so batch processing based on the colors will not
# necessarily yield good results
rc_list <- vector("list", length(birds)) 
names(rc_list) <- tools::file_path_sans_ext(basename(birds))

# illustrating example for the 3rd image
# (LSUMZ 129286, Canon 80D DSLR, with flash/fluorescent lights)
i <- 3

# initial segmentation
rc <- recolorize2(birds[i], 
                  bins = 3,
                  cutoff = 25)

# merging layers & assigning a color palette for visualization
rc2 <- mergeLayers(rc, list(c(2, 3),
                            c(1),
                            c(4, 5)),
                   color_to = c("#804026","#322D31","#CCA269"))

# remove speckles
rc3 <- absorbLayer(rc2, 3, function(s) s <= 1000)

# insert into list
rc_list[[i]] <- rc3

# save result:
saveRDS(rc_list, "05_birds/rds_files/impose_list_practice.rds")

# we can load the set from the paper (check the call objects
# for the processing steps for each)
rc_list_final <- readRDS("05_birds/rds_files/impose_list.rds")
layout(matrix(1:8, nrow = 2)); par(mar = rep(0, 4))
for (i in 1:length(rc_list_final)) {
  plotImageArray(recoloredImage(rc_list_final[[i]]))
}
