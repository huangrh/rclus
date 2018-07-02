# Copyright (C) 2016-2018 Ren-Huai Huang <huangrenhuai@gmail.com>
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
