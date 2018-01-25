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
#' @param x A data frame containing winsorized summary score returned from relvm.
#' @param method Kmean cluster algorithm.\itemize{ \item kmeans: kmeans
#'   clustering. \item rclus: rapid clustering in rclus package.}
#' @param score_col The column name in a data frame serving as input for
#'   clustering.
#' @param iter.max The maximum number of iterations.
#' @return A data.frame containing the hospital rating stars 1, 2, 3, 4, and 5.
#'   star 5 indicates the best hospital in USA.
#' @seealso \code{\link{rating}}
#' @export
rating.data.frame <- function(x,method = c("rclus2","rclus","kmeans","na"),
                              score_col="sum_score_win",iter.max=1000) {
    if (exists(score_col,x))
    {
        fit = rating(x[,score_col],method=method,iter.max=iter.max)
    } else {
        stop("rating: the input score column doesn't match!")
    }
    star <- fit$star
    fit$summary_score <- cbind.data.frame(x,star)
    fit
}
