#' Transform Cluster To Star
#'
#' The clusters are transformed to star from 1 to 5 according to their
#' mean clustering scores
#'
#' @param The output object from kmeans clustering
#' @return The input object plus the star rating.
#'
#' @examples
#' fit <- restar(fit)
#'
#' @export
cen2star <-function(fit = fit) {

  # centers is the mean of the cluster in the group
  len <- length(fit$centers)
  star_helper <- data.frame(star_mean=as.numeric(fit$centers),
                            cluster=rownames(fit$centers))
  star_helper <- star_helper[order(star_helper$star_mean),]
  star_helper$star <- 1:len

  # ref: http://adv-r.had.co.nz/Subsetting.html
  fit$star <- star_helper[match(as.numeric(fit$cluster),
                                as.numeric(star_helper$cluster)),][["star"]]
  fit$star_helper <- star_helper
  return(fit)
}
