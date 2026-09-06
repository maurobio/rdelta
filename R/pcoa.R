#' Principal Coordinates Analysis
#' 
#' Performs principal coordinates analysis (PCoA) on a distance matrix generated 
#' by the dist() function. This function reads the distance matrix and item names
#' from the DIST output files and creates a PCoA scatterplot.
#'
#' @param notu Number of operational taxonomic units (items) in the dataset
#'
#' @return Invisibly returns the PCoA results from cmdscale()
#' @export
#'
#' @examples
#' \dontrun{
#' # First generate a distance matrix
#' delta <- load_delta("chars", "items", "specs")
#' dist(delta)
#' 
#' # Then perform PCoA
#' pcoa(delta$get_items_nb())
#' }
pcoa <- function(notu) {
  # Validate input
  if (!is.numeric(notu) || notu <= 0 || notu != round(notu)) {
    stop("'notu' must be a positive integer")
  }
  
  # Check if distance matrix files exist
  if (!file.exists("dist.dis")) {
    stop("Distance matrix file 'dist.dis' not found. Run dist() first.")
  }
  if (!file.exists("dist.nam")) {
    stop("Item names file 'dist.nam' not found. Run dist() first.")
  }
  
  # Read distance matrix
  mat <- matrix(0, notu, notu)
  mat[row(mat) >= col(mat)] <- scan("dist.dis")
  names <- scan("dist.nam", what = "character")
  colnames(mat) <- names
  df.dist <- stats::as.dist(mat)
  
  # Perform principal coordinates analysis
  pco <- stats::cmdscale(df.dist, k = 2, eig = TRUE)
  pcovar <- pco$eig / sum(pco$eig[pco$eig > 0])
  
  # Plot PCoA results
  graphics::par(mar = c(4, 4, 4, 4))
  graphics::plot(
    pco$points,
    main = "PCoA Scatterplot",
    xlab = paste("Axis 1 (", round(pcovar[1], 3) * 100, "%)", sep = ""),
    ylab = paste("Axis 2 (", round(pcovar[2], 3) * 100, "%)", sep = ""),
    col = "blue",
    pch = 19
  )
  graphics::text(
    pco$points[, 1:2],
    labels = rownames(pco$points),
    pos = 3,
    cex = 0.7
  )
  
  message("PCoA completed successfully")
  message("  - Axis 1 variance: ", round(pcovar[1], 3) * 100, "%")
  message("  - Axis 2 variance: ", round(pcovar[2], 3) * 100, "%")
  message("  - Cumulative variance: ", round(sum(pcovar[1:2]), 3) * 100, "%")
  
  invisible(pco)
}