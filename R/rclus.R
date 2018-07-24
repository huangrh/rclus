# Copyright (C) 2016-2017 Ren-Huai Huang <huangrenhuai@gmail.com>
#
# This file is part of rclus.
#
# rclus is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# rclus is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with rclus.  If not, see <http://www.gnu.org/licenses/>.


#' Rapid Clustering
#'
#' Rapid Clustering. It works for One Dimensional array data only currently.
#'
#' @param dat An one-dimension array data.
#' @param seeds The number of start seeds or a vector of centroids (or cluster centers).
#' @param maxiter The maximum number of iteration specified.
#' @param strict Strict value. It prevents an observation from being assigned to
#'   a cluster if its distance to the nearest cluster seed exceeds the value of
#'   the STRICT. Set strict = NULL to disable the strict mode.
#'
#' @return A clustering object including clusters, actual iteration number,
#'   cluster centers and the size of each cluster, etc.
#'
#' @export
#'
rclus <- function(dat,seeds=5, maxiter = 1000, strict =NULL) {

    if (missing(dat) | is.null(dat)) stop("input data is missing!")

    #----------------------
    # Seeds initialization
    #----------------------
    if (length(seeds) > 1) {
        cat("\n","seeds:",seeds,"\n")
        init_seeds <- centers <- seeds
    } else if (length(seeds) == 1 & seeds == round(seeds)) {
        init_seeds <- centers <- seeds_init(dat, num_centers=seeds)
    } else {
        stop("check the seeds")
    }


    #---------------
    # Seeds updating
    #---------------
    clusters     <- c()
    old_centers  <- centers
    iter_history <- as.data.frame(matrix(ncol=5))
    for (iter in 1:maxiter) {
        # assign the observation to cluster
        for (idx in seq_along(dat)) {
            clusters[idx] <- which.min(abs(dat[idx]-centers))
        }

        # update the centroids
        if (is.null(strict) || strict < 0) {
            centers <- sort(tapply(dat,clusters,mean))
        } else {
            # replicate SAS fastclus strict mode
            strict_index <- (abs(dat-centers[clusters]) < strict)
            centers <- sort(tapply(dat[strict_index],clusters[strict_index],mean))
        }


        #
        # iter_history[iter,] <- round(abs((centers - old_centers)/old_centers),6)
        if (identical(centers,old_centers)) {
            break
        } else {
            cat("-")
            old_centers <- centers
        }
    }

    cat("\n","Center:",centers)
    #---------------------------------------------------------------------------
    # Final assignment of the data to the nearest/closest updated centroid
    #---------------------------------------------------------------------------
    for (idx in seq_along(dat)) {
        clusters[idx] <- which.min(abs(dat[idx]-centers))
    }

    #------------------
    # Test if converged
    #------------------
    # calculate the centers
    if (is.null(strict) || strict < 0) {
        new_centers <- sort(tapply(dat,clusters,mean))
    } else {
        # replicate SAS fastclus strict mode
        strict_index <- (abs(dat-centers[clusters]) < strict)
        new_centers  <- sort(tapply(dat[strict_index],clusters[strict_index],mean))
    }
    # test
    convergence <- ifelse(identical(new_centers, centers),0,1)


    #--------
    # output
    #--------
    structure(list(input_data  = dat,
                   num_seeds   = seeds,
                   init_seeds  = init_seeds,
                   maxiter     = maxiter,
                   cluster     = clusters,
                   star        = clusters,
                   convergence = convergence,
                   iter        = iter - 1,
                   size        = t(as.matrix(table(clusters))),
                   centers     = new_centers,
                   # iter_history= iter_history,
                   tot.wcssr  = sum((dat-new_centers[clusters])^2)),
              class = "rclus")
}


