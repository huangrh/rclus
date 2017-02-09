#' Index Look-Up
#'
#' @param centroids T
#' @return A lookup function to loo
#' @export
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


