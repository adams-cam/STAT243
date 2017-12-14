################################################################
#
# generate founders function
# Xu, Weijie; Chen, Yuwen; Adams, Cameron; Zhou, Yilin
#
# Final Projct
# STAT 243
# Fall 2017
#
################################################################

#' Generate founders function
#'
#' Generates founders given a covariate data matrix or dataframe and returns a matrix of founder chromosomes. Number of founders is determined by \code{\link{choose}(C, 2)} where C is number of predictors. Maximum number of founding chromosomes is 200. User can specify number of starting chromosomes and this number can be larger than 200. Founding chromosmes are generated randomly and unique values are kept.
#'
#' @param X a matrix or dataframe of predictor variables
#' @param start_chrom a numeric value indicating user specified number of founding chromosomes. Default is null.
#'
#' @export


generate_founders <- function(X, start_chrom) {

    # number of predictors ---------------
    C <- dim(X)[2]

    # number of founders ----------------
    if (is.null(start_chrom)) {
        P <- 2 * C
        if (P > 200) {         #check for max/min chrom
            P <- 200
        } else if (P < 20) {
            P <- 20
        }
    } else {
        if (start_chrom > 200) cat("Warning: P > 200, algorithm may require lots of running time")
        P <- start_chrom #user number of chroms
    }

    # randomly generate founders ----------------
    geneSample <- sample(c(0, 1),
                         replace = TRUE,
                         size = ceiling(1.2 * C * P))

    # create a first generation ----------------
    x <- seq_along(geneSample)
    firstGen <- split(geneSample, ceiling(x / C))
    generation_t0 <- matrix(unlist(unique(firstGen)[1:P]),
                            ncol = C, byrow = TRUE)
    generation_t0 <- generation_t0[apply(generation_t0, 1,
                                         function(x) !all(x == 0)), ]

    return(generation_t0)
}
