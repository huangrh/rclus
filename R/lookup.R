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


#' Index Look-Up
#'
#' @param centroid The controid is the center of the corresponding clustrer.
#' @return A lookup function.
#'
look_up <- function(centroid) lookup_generator(centroid)


#' Lookup Generator
#'
lookup_generator <- function(centroids=NULL){

    n=length(centroids) - 1

    df <- as.data.frame(matrix(NA,n,n))
    for (ci in 1:n) {
        for (ri in (ci):n) df[ri,ci] = paste(ci,ri+1)
    }
    look_up <- na.omit(unlist(df))
    function(idx){
        as.integer(unlist(strsplit(c(look_up)[idx]," ")))
    }
}


