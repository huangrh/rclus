## ---- include=FALSE------------------------------------------------------
# Load the hospital summary score
require(readxl)
file <- system.file("cms/sas_oct2016/sas/SASoutput_Oct2016_2016-11-14.xlsx",package="cmsdata")
dat  <- read_excel(file,sheet="star_2016oct")
grp_score<- dat$"Summary_score_win"
fit  <- kmeans(grp_score,centers=5,iter.max=100,nstart=50)
fit  <- rstarating::cen2star(fit)
table(fit$star)


## ----seed initilization--------------------------------------------------
# initialize the centroids of clusters
centers   <- sort(grp_score[1:5])
lookup    <- rclus:::lookup_generator(centers)
clusters  <- c()
 
# ---------------------------------------------------------------------------
# Seeds replacing: 
# Two tests are made to each observation to decide if it can qulify as a new seed.
# 
for (idx in seq_along(grp_score)) {

    # The index of the nearest seed from the observation
    idx_min_cen2obs <- which.min(abs(grp_score[idx]-centers))
    
    # Test 1: an old seed is replaced if 
    # the distance between the observation and its closest seed 
    # > the minimum distance between seeds. 
    # 
    dist_in_centers <- dist(centers)
    if (min(abs(grp_score[idx]- centers))> min(dist_in_centers)) {
        
        # the seeds that is replaced is selected from the two seeds that are closest to each other
        idxs_mcen2cen <- lookup(which.min(dist_in_centers))
        mindist_of_2seeds <- c()
        # the seed to be placed is the one of these two with the shortest distance to 
        # the closest of the remaining seeds when the other seed is repalced by the current observation 
        # 
        # idxs_mcen2cen[1]
        # mindist_of_2seeds[1] <- min(abs(c(centers[-idxs_mcen2cen]) - centers[idxs_mcen2cen[1]]))
        mindist_of_2seeds[1] <- min(abs(c(centers[-idxs_mcen2cen],grp_score[idx]) 
                                        - centers[idxs_mcen2cen[1]]))
        
        # idxs_mcen2cen[2]
        # mindist_of_2seeds[2] <- min(abs(c(centers[-idxs_mcen2cen]) - centers[idxs_mcen2cen[2]]))
        mindist_of_2seeds[2] <- min(abs(c(centers[-idxs_mcen2cen],grp_score[idx]) 
                                        - centers[idxs_mcen2cen[2]]))
        
        # update the seeds
        centers[idxs_mcen2cen[which.min(mindist_of_2seeds)]]=grp_score[idx]
        
    } else if(min(abs(grp_score[idx]- centers[-idx_min_cen2obs])) > 
              min(abs(centers[idx_min_cen2obs] - centers[-idx_min_cen2obs]))){
        # ----------------------------------------------------------------------------------
        # test 2: the observation repalces its nearest seed if
        # the smallest distance from the observation to all seeds other than the nearest one 
        # >
        # shortest distance from the nearest seed to all other seeds
        #
        centers[idx_min_cen2obs] <- grp_score[idx]
    }
}
sort(centers)

## ------------------------------------------------------------------------
# -----------------------------------------------------------------
clusters <- c()
n=1
for (iter in 1:n) {
    for (idx in seq_along(grp_score)) {
        clusters[idx] <- which.min(abs(grp_score[idx]-centers))
    }
    
    # update the centroids
    centers <- sort(tapply(grp_score,clusters,mean)) 
}

# -----------------------------------------------------------------
# reassign the observartion to the nearest/closest updated centroid
for (idx in seq_along(grp_score)) {
    clusters[idx] <- which.min(abs(grp_score[idx]-centers))
}

#
table(clusters)
centers

## ------------------------------------------------------------------------
# -----------------------------------------------------------------
n=20
old_centers <- centers
for (iter in 1:n) {
    # Assign each observation to its nearest/closest centroid
    for (idx in seq_along(grp_score)) {
        clusters[idx] <- which.min(abs(grp_score[idx]-centers))
    }
    
    # update the centroids
    centers <- sort(tapply(grp_score,clusters,mean)) 
    
}

# -----------------------------------------------------------------
# Reassign each observartion to its nearest/closest updated centroid
for (idx in seq_along(grp_score)) {
    clusters[idx] <- which.min(abs(grp_score[idx]-centers))
}

#
table(clusters)
centers

## ----use rclus-----------------------------------------------------------
require(rclus)

# One iteration
fit1 <- rclus(grp_score,seeds=5,maxiter=1)
fit1
fit1$init_seeds

# maxiter = 20
fit20 <- rclus(grp_score,seeds=5,maxiter = 20)
fit20
fit20$init_seeds


