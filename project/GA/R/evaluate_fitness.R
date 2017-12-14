################################################################
#
# evaluate fitness function
# Xu, Weijie; Chen, Yuwen; Adams, Cameron; Zhou, Yilin
#
# Final Projct
# STAT 243
# Fall 2017
#
################################################################



#' Function to evaluate fitness of chromosomes.
#'
#' This function uses default or user supplied ojective functions, rank functions, to evaluate fitness of a generation chromosomes. This process can be parallized if \code{nCores > 1}. Parallel operations uses \code{\link{mclapply}} to parallelize operations. Max number of parallel operations is determined by \code{\link{detectCores}}. Prescheduling = TRUE is only option for compuations. It is advised to only use parallelization if inputed a large dataframe with great than 1000 oberations and/or predictors.
#'
#' @param generation_t0 a matrix of parent chromosomes to be evaluated. Columns correspond to predictors/genes and rows correspond to parents/chromosomes.
#' @param Y vector of response variable
#' @param X a matrix or dataframe of predictor variables
#' @param family a character string describing the error distribution and link function to be used in the model. Default is gaussian.
#' @param nCores an integer indicating number of parallel processes to run when evaluating fitness. See \code{\link{evaluate_fitness}} and details below.
#' @param minimize a logical value indicating whether optimize should be minimized (TRUE) or maximized (FALSE).
#' @param objective_function function for computing objective. Default is \code{\link{AIC}}. User can specify custom function.
#' @param rank_objective_function a function that ranks parents by their fitness as determined by optimize criteria.
#'
#' This function uses \code{\link{mclapply}} to parallel processess. User can specify number of cores to use, with a maximum number of cores equal to output of \code{\link{detectCores}}. Parallel processing is set to preschedule by default; dynamic parallelization is not available at this time.
#' @export

evaluate_fitness <- function(generation_t0, Y, X,
                             family,
                             nCores, minimize,
                             objective_function,
                             rank_objective_function) {

    #number parent chromosomes
    P <- dim(generation_t0)[1]

    ######
    #evaluate and rank each chromosome with selected objective function
    ######

    # serial ----------------
    if (nCores == 1) {

        # lm ----------------
        if (family == "gaussian") {
            obj_fun_output <- sapply(1:P, function(i) {
                mod <- stats::lm(Y ~ X[, generation_t0[i, ] == 1])
                return(objective_function(mod))
            })
            # glm ----------------
        } else if(family != "gaussian") {
            obj_fun_output <- sapply(1:P, function(i) {
                mod <- stats::glm(Y ~ X[, generation_t0[i, ] == 1], family = family)
                return(objective_function(mod))
            })
        }

        # parallel ----------------
    } else if (nCores > 1) {

        # lm ----------------
        if (family == "gaussian") {
            obj_fun_output <- unlist(parallel::mclapply(1:P, function(i) {
                mod <- stats::lm(Y ~ X[, generation_t0[i, ] == 1])
                return(objective_function(mod))
            }, mc.preschedule = TRUE, mc.cores = nCores))
            # glm ----------------
        } else if(family != "gaussian") {
            obj_fun_output <- unlist(parallel::mclapply(1:P, function(i) {
                mod <- stats::glm(Y ~ X[, generation_t0[i, ] == 1], family = family)
                return(objective_function(mod))
            }, mc.preschedule = TRUE, mc.cores = nCores))
        }
    }

    # rank ----------------
    parent_rank <- rank_objective_function(obj_fun_output, minimize)

    # return rankings ----------------
    return(parent_rank)
}
