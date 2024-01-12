#' Run a PCA on multiple colors
#' 
#' Runs a PCA on two or more colors from a patternize raster list.
#' 
#' @param rList List of raster objects, where each element is an individual, and
#'   the list elements of that element are individual color pattern layers for
#'   that individual.
#' @param which_colors Either "all" (the default) or a numeric vector indicating
#'   which colors to use for the combined PCA.
#' @param PCx,PCy Which principal components to plot on the X and Y axes, if
#'   plotting.
#' @param plotting Logical. Plot PCA results?
#' @param quietly Logical. Print progress messages?
#' 
#' @return A [stats::prcomp] object for the specified colors.
#' 
#' @export
patPCA_total <- function(rList, which_colors = "all",
                         PCx = 1, PCy = 2,
                         plotting = TRUE, 
                         quietly = TRUE) {
  
  # determine the number of colors
  n_layers <- length(rList[[1]])
  IDlist <- names(rList)
  
  # restructure the raster list by layer instead of image
  rList <- lapply(1:n_layers,
                  function(i) lapply(rList,
                                     function(j) j[[i]]))
  
  # redefine to only specified layers (if not "all")
  if (sum(which_colors != "all") > 0) {
    if (!is.numeric(which_colors)) {
      stop("'which_colors' must be 'all' or a numeric vector indicating which
           colors should be used for the summed PCA")
    }
    rList <- rList[which_colors]
    n_layers <- length(which_colors)
  }
  
  
  if (!quietly) { message("Summing raster lists...") }
  
  # sum rasters after masking
  summedRasters <- lapply(rList,
                          function(i) patternize::sumRaster(i, IDlist, type = "RGB"))
  
  if (!quietly) { message("Making dataframe from rasters...") }
  
  # make a list for storing the raster dataframes
  rasDFlist <- vector("list", length = n_layers)
  
  # make one dataframe per layer
  for (l in 1:n_layers) {
    for (r in 1:length(rList[[1]])) {
      
      # isolate layer from image
      layer <- rList[[l]][[r]]
      
      # swap out NA values
      layer[is.na(layer)] <- 0
      
      # convert to a dataframe
      ras <- raster::as.data.frame(layer)
      
      # either start or append the dataframe for this layer
      if (r == 1) { rasDF <- ras } else { rasDF <- cbind(rasDF, ras) }
    }
    
    # set column names and add to the list
    colnames(rasDF) <- names(rList[[l]])
    rasDFlist[[l]] <- rasDF
  }
  
  # make a stacked version for the full PCA
  rasDFstack <- do.call(rbind, rasDFlist)
  
  if (!quietly) { message(paste("Running PCA on", n_layers, 
                                "colors and", ncol(rasDFstack), "images...")) }
  
  # run a PCA
  comp <- prcomp(t(rasDFstack))
  pcdata <- comp$x
  rotation <- comp$rotation
  summ <- summary(comp)
  
  if (!quietly) { message("done") }
  
  if (plotting) {
    xrange <- range(pcdata[ , PCx])
    yrange <- range(pcdata[ , PCy])
    
    # be polite
    current_par <- par(no.readonly = TRUE)
    on.exit(par(current_par))
    
    # set parameters and plot
    par(mfrow=c(1,1), mar=c(4,4,2,2))
    plot(comp$x[,c(PCx,PCy)], col='black', pch=19,
         xlim = xrange, ylim = yrange,
         xlab=paste0('PC',PCx,' (', round(summ$importance[2,PCx]*100, 1), ' %)'),
         ylab=paste0('PC',PCy,' (', round(summ$importance[2,PCy]*100, 1), ' %)'))
    
  }
  
  # return the pca:
  return(comp)
  
}