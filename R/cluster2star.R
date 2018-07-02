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
