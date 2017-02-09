#' Rapid Clustering
#'
#' Rapid Clustering. It works for One Dimensional array data only currently.
#'
#' @param dat An one-dimension array data.
#' @param seeds The number of start seeds.
#' @param maxiter The maximum number of iteration specified.
#' @return A clustering object including clusters, actual iteration number,
#'   cluster centers and the size of each cluster, etc.
#'
#' @export
#'
rclus <- function(dat,seeds=5, maxiter = 1) {

    if (missing(dat) | is.null(dat)) stop("input data is missing!")

    #----------------------
    # Seeds initialization
    #----------------------
    init_seeds <- centers <- seeds_init(dat,num_centers=seeds)

    #---------------
    # Seeds updating
    #---------------
    clusters    <- c()
    old_centers <- centers
    for (iter in 1:maxiter) {
        # assign the observation to cluster
        for (idx in seq_along(dat)) {
            clusters[idx] <- which.min(abs(dat[idx]-centers))
        }

        # update the centroids
        centers <- sort(tapply(dat,clusters,mean))

        #
        if (identical(centers,old_centers)) {
            break
        } else {
            old_centers <- centers
        }
    }

    #---------------------------------------------------------------------------
    # Final assignment of the data to the nearest/closest updated centroid
    #---------------------------------------------------------------------------
    for (idx in seq_along(dat)) {
        clusters[idx] <- which.min(abs(dat[idx]-centers))
    }

    #------------------
    # Test if converged
    #------------------
    new_centers <- sort(tapply(dat,clusters,mean))
    convergence <- ifelse(identical(new_centers, centers),0,1)

    #--------
    # output
    #--------
    structure(list(input_data  = dat,
                   num_seeds   = seeds,
                   init_seeds  = init_seeds,
                   maxiter     = maxiter,
                   clusters    = clusters,
                   convergence = convergence,
                   iter        = iter,
                   size        = t(as.matrix(table(clusters))),
                   centers     = new_centers),class = "rclus")
}


