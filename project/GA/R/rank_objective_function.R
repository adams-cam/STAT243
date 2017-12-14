################################################################
#
# Rank parent fitness
# Xu, Weijie; Chen, Yuwen; Adams, Cameron; Zhou, Yilin
#
# Final Projct
# STAT 243
# Fall 2017
#
################################################################

#' Rank parent chromosome fitness
#'
#' Parents are ranked according to their fitness. Rank order is inverse with east fit parent has a rank == 1.
#'
#' @param obj_fun_output a numeric vector containg the objective function output for each chromosome for ranking.
#' @param minimize a logical value indicating whether to rank according to minimization or maximaziation optimization. Default is minimize.
#'
#' @export

rank_objective_function <- function(obj_fun_output, minimize) {

    P <- length(obj_fun_output)

    if (isTRUE(minimize)) {
        r <- base::rank(-obj_fun_output, na.last = TRUE, ties.method = "average")
    } else {
        r <- base::rank(obj_fun_output, na.last = TRUE, ties.method = "average")
    }

    return(cbind(chr = 1:P, parent_rank = r, obj_fun_output))
}
