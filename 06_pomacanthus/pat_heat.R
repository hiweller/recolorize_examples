pat_heat <- function(rList, which_color = 1,
                         plotting = TRUE,
                     colpalette = rev(viridis::magma(100))) {
  
  IDlist <- names(rList)
  
  # restructure the raster list by layer instead of image
  rList <- lapply(rList, function(j) j[[which_color]])
  
 # sum rasters after masking
  summedRasters <- patternize::sumRaster(rList, IDlist, type = "RGB")
if (plotting) {
    cp <- c("white",
            colpalette)
    patternize::plotHeat(summedRasters, IDlist = IDlist,
             colpalette = cp, legend.side = 3)
  }
  
  # return the pca:
  return(summedRasters)
  
}
