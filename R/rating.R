#' S3 Generic Function For Hospital Star Rating
#'
#' Hospital star rating from wisorized summary scores.
#'
#' @param x Winsorized summary score returned from relvm2 which contains the
#'   wisorized summary score.
#' @param method Kmean cluster algorithm.\itemize{ \item kmeans: kmeans
#'   clustering. \item rclus: rapid clustering in rclus package.}
#' @param score_col The column name in a data frame as input for clustering.
#' @return Hospital rating stars 1, 2, 3, 4, and 5. star 5 indicates the best
#'   hospital in USA.
#' @seealso \code{\link{rating.data.frame}}
#' @export
rating <- function(x,method,score_col) UseMethod("rating")


#' @rdname rating
#'
#' @export
rating.default <- function(x, method = c("rclus","kmeans","na")) {
    if (is.null(x)) return(NULL);

    star <- switch(method[1],
                   kmeans={
                       fit = kmeans(x, centers=5, iter.max=100, nstart = 50)
                       fit = cen2star(fit)
                       fit$star
                   },
                   rclus={
                       fit = rclus(x,seeds=5,maxiter = 200)
                       fit$cluster
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
#' @param x A data frame contains Winsorized summary score returned from relvm2
#'   which contains the wisorized summary score.
#' @param method Kmean cluster algorithm.\itemize{ \item kmeans: kmeans
#'   clustering. \item rclus: rapid clustering in rclus package.}
#' @param score_col The column name in a data frame serving as input for
#'   clustering.
#' @return A data.frame containing the hospital rating stars 1, 2, 3, 4, and 5.
#'   star 5 indicates the best hospital in USA.
#' @seealso \code{\link{rating}}
#' @export
rating.data.frame <- function(x,method = c("rclus","kmeans","na"),score_col="sum_score_win") {
    if (exists(score_col,x))
    {
        fit = rating(x[,score_col],method=method)
    } else {
        stop("rating: the input score column doesn't match!")
    }
    fit$summary_score <- cbind.data.frame(x,fit$star)
    fit
}
