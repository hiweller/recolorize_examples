# This image (by Nathan P. Lord) comes with the package (super down-sampled):
img <- system.file("extdata/corbetti.png", package = "recolorize")

# We can do this in one line because it's a pretty straightforward segmentation:
rc <- recolorize2(img, n_final = 5)

# We can also despeckle (this is almost entirely for visual appeal):
for (i in 1:length(rc$centers)) {
  rc <- absorbLayer(rc, i, 
                    size_condition <- \(x) x <= prod(dim(rc$original_img)) * 0.001)
}

# Or convert to a vector (again, more useful for creating a scale-free 
# visualization than having any real scientific value):
rc_vec <- recolorizeVector(rc, smoothness = 5, size_filter = 0)
plot(rc_vec)

# Or map to Werner's nomenclature:
rc_werner <- wernerColor(rc, which_img = "recolored")
