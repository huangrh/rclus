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


#' S3 Generic Function For Hospital Star Rating
#'
#' Hospital star rating from wisorized summary scores.
#'
#' @param x Winsorized summary score returned from relvm.
#' @param method Kmean cluster algorithm.\itemize{ \item kmeans: kmeans
#'   clustering. \item rclus: rapid clustering in rclus package.}
#' @param score_col The column name in a data frame as input for clustering.
#' @param iter.max The maximum number of iterations.
#' @return Hospital rating stars 1, 2, 3, 4, and 5. star 5 indicates the best
#'   hospital in USA.
#' @seealso \code{\link{rating.data.frame}}
#' @export
rating <- function(x,method,score_col,iter.max) UseMethod("rating")


#' @rdname rating
#'
#' @export
rating.default <- function(x, method = c("rclus2","rclus","kmeans","na"),iter.max=1000) {
    if (is.null(x)) return(NULL);

    star <- switch(method[1],
                   kmeans={
                       fit = kmeans(x, centers=5, iter.max=iter.max, nstart = 500)
                       fit = cen2star(fit)
                       fit$star
                   },
                   rclus={
                       fit = rclus(x,seeds=5,maxiter = iter.max,strict = NULL)
                       fit$cluster
                   },
                   # for 2017 Dec SAS Pack
                   rclus2={
                       groups <- cut(x,breaks=c(quantile(x,probs = seq(0,1,by=0.2))),
                                     labels=paste0(seq(0,80,20),"-",seq(20,100,20)),include.lowest = TRUE)
                       # step 1: generate seeds
                       cat("\n","Step 1: ")
                       cl1   <- rclus(x, seeds=tapply(x,groups,median), maxiter=iter.max, strict=NULL)

                       # step 2: use the seeds from step 1 and use strict mode
                       cat("\n","Step 2:")
                       fit   <- rclus(x, seeds=cl1$centers, maxiter=iter.max, strict=1)

                   },
                   na={
                       fit=NULL
                   })
    fit
}

#' S3 Method of Rating
#'
#' Hospital star rating from wisorized summary scores in a data frame.
#'
#' @param x A data frame containing winsorized summary score returned from
#'   relvm.
#' @param method Kmean cluster algorithm.\itemize{ \item kmeans: kmeans
#'   clustering. \item rclus: rapid clustering in rclus package.}
#' @param score_col The column name in a data frame serving as input for
#'   clustering. If null, it will be set according to the method: "rclus2" uses
#'   "sum_score" and all other methods use "sum_score_win".
#' @param iter.max The maximum number of iterations.
#' @param report_indicator The indicator 0 and 1. "1" indicates the clustering uses
#'   the rows only with a report_indicator of 1 from the input data frame.
#' @return A data.frame containing the hospital rating stars 1, 2, 3, 4, and 5.
#'   star 5 indicates the best hospital in USA.
#' @seealso \code{\link{rating}}
#' @export
rating.data.frame <- function(x,
                              method = c("rclus2","rclus","kmeans","na"),
                              score_col=NULL,
                              iter.max=1000) {
    # subset by report_indicator
    if (identical(method[1],"rclus2")) {
        row_idx = x$report_indicator %in% 1
        if (is.null( score_col)) score_col="sum_score"
    } else {
        row_idx = rep(TRUE, nrow(x))
        if (is.null(score_col)) score_col = "sum_score_win"
    }


    # call Rating.default
    if (exists(score_col,x))
    {
        fit = rating(x[row_idx,score_col],method=method,iter.max=iter.max)
    } else {
        stop("rating: the input score column doesn't match!")
    }

    # Output
    star              <- fit$star
    fit$summary_score <- cbind.data.frame(x[row_idx,],star)
    fit$star          <- cbind.data.frame(x[row_idx,'ccnid',drop=F],star)
    fit
}
