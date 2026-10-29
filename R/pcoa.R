#' Principal Coordinates Analysis
#'
#' Performs principal coordinates analysis (PCoA) on a distance matrix generated
#' by the dist() function. This function reads the distance matrix and item names
#' from the DIST output files and creates a PCoA scatterplot.
#'
#' @param notu Number of operational taxonomic units (items) in the dataset.
#'
#' @return Invisibly returns a list with the following components:
#'   \describe{
#'     \item{points}{A two-column matrix of the coordinates of the items on
#'       the first two principal coordinate axes.}
#'     \item{eig}{The eigenvalues of the PCoA solution, as returned by
#'       \code{\link[stats]{cmdscale}}.}
#'     \item{variance}{A numeric vector of length 2 giving the proportion of
#'       total variance explained by Axis 1 and Axis 2 (values between 0 and 1).}
#'     \item{cumulative}{A single numeric value giving the cumulative
#'       proportion of variance explained by the first two axes combined.}
#'     \item{cmdscale}{The full result object returned by
#'       \code{\link[stats]{cmdscale}}, for users who need access to all
#'       components.}
#'   }
#' @export
#'
#' @examples
#' # Requires the CONFOR and DIST executables on the PATH.
#' \donttest{
#' try({
#'   delta <- load_delta(
#'     system.file("extdata", "chars_viola", package = "RDelta"),
#'     system.file("extdata", "items_viola", package = "RDelta"),
#'     system.file("extdata", "specs_viola", package = "RDelta")
#'   )
#'   dist(delta)
#'   res <- pcoa(delta$get_items_nb())
#'   res$variance
#'   res$cumulative
#' })
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
  pco     <- stats::cmdscale(df.dist, k = 2, eig = TRUE)
  pcovar  <- pco$eig / sum(pco$eig[pco$eig > 0])

  # Plot PCoA results
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar), add = TRUE)
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

  # Return results invisibly so the user can access them programmatically
  invisible(list(
    points     = pco$points,
    eig        = pco$eig,
    variance   = pcovar[1:2],
    cumulative = sum(pcovar[1:2]),
    cmdscale   = pco
  ))
}