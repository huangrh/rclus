#' S3 method of print for rclus class
#'
#'
#'
#' @export
#'
print.rclus <- function(object,...) {
    cat("\n$class:\n");         print.default(class(object))
    cat("\n$iter:\n");          print.default(object$iter)
    cat("\n$size:\n");          print.default(object$size)
    cat("\n$centers:\n");       print.default(object$centers)
    cat("\n$convergence:\n");   print.default(object$convergence)
    cat("\n$avalable fields:\n",names(object),"\n")
}
