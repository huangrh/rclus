# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Seeds initialization
seeds_init <- function(dat, num_centers) {

    # initialize the centroids of clusters
    centers   <- sort(dat[1:5])
    lookup    <- rclus:::lookup_generator(centers)

    # --------------------------------------------------------------------------
    # Seeds replacing:
    # Two tests are made to each observation to decide if it can qulify as a new seed.
    #---------------------------------------------------------------------------

    for (idx in seq_along(dat)) {

        # The index of the nearest seed from the observation
        idx_min_cen2obs <- which.min(abs(dat[idx]-centers))

        #------------------------------------
        # Test 1: an old seed is replaced if
        # the distance between the observation and its closest seed
        # > the minimum distance between seeds.
        #-----------------------------------
        dist_in_centers <- dist(centers)
        if (min(abs(dat[idx]- centers))> min(dist_in_centers)) {

            # the seeds that is replaced is selected from the two seeds that are closest to each other
            idxs_mcen2cen <- lookup(which.min(dist_in_centers))
            mindist_of_2seeds <- c()
            # the seed to be placed is the one of these two with the shortest distance to
            # the closest of the remaining seeds when the other seed is repalced by the current observation
            #
            # idxs_mcen2cen[1]
            mindist_of_2seeds[1] <- min(abs(c(centers[-idxs_mcen2cen],dat[idx])
                                            - centers[idxs_mcen2cen[1]]))

            # idxs_mcen2cen[2]
            mindist_of_2seeds[2] <- min(abs(c(centers[-idxs_mcen2cen],dat[idx])
                                            - centers[idxs_mcen2cen[2]]))

            # update the seeds
            centers[idxs_mcen2cen[which.min(mindist_of_2seeds)]]=dat[idx]

        } else if(min(abs(dat[idx]- centers[-idx_min_cen2obs])) >
                  min(abs(centers[idx_min_cen2obs] - centers[-idx_min_cen2obs]))){
            # -----------------------------------------------------------------------
            # test 2: the observation repalces its nearest seed if
            # the smallest distance from the observation to all seeds other than the nearest one
            # >
            # shortest distance from the nearest seed to all other seeds
            #------------------------------------------------------------------------
            centers[idx_min_cen2obs] <- dat[idx]
        }
    }
    sort(centers)
}
