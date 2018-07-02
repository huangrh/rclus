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


#' S3 method of print for rclus class
#'
#' @export
#'
print.rclus <- function(object,...) {
    cat("\n$class:\n");         print.default(class(object))
    cat("\n$iter:\n");          print.default(object$iter)
    cat("\n$size:\n");          print.default(object$size)
    cat("\n$centers:\n");       print.default(object$centers)
    cat("\n$total within-cluster sum of square residues:\n");print.default(object$tot.wcssr)
    cat("\n$convergence:\n");   print.default(object$convergence)
    cat("\n$avalable fields:\n",names(object),"\n")
}


