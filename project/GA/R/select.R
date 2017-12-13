################################################################
#
# Main GA function
# Xu, Weijie; Chen, Yuwen; Adams, Cameron; Zhou, Yilin
#
# Final Projct
# STAT 243
# Fall 2017
#
################################################################

#' Variable selection using genetic algorithms
#'
#' select implements genetic algorithms for variable selection for GLMs by optimizing package or user specified objective functions such as AIC, BIC, and logloglikelihood.
#' Uses functions: \code{\link{generate_founders}}, \code{\link{evaluate_fitness}}, and \code{\link{create_next_generation}}.
#' Functions find optimal variables by using evolutationry biology concepts of natural selection, fitness, genetic crossover, and mutation. Founding generation of chromosomes is randomly generated and evaluated using an critieria such as AIC, BIC, or loglihood. Parents are selected by their fitness, and generate children chromosomes. As each generation breeds and produces new genreations, the algorithm moves towards the optimum.
#'
#' 1. Geof H. Givens, Jennifer A. Hoeting (2013) Combinatorial Optimization (italicize). Chapter 3 of Computational Statistics (italicize).
#'
#' @param Y vector of response variable
#' @param X a matrix or dataframe of predictor variables
#' @param family a character string describing the error distribution and link function to be used in the model. Default is gaussian.
#' @param objective_function function for computing objective. Default is \code{\link{stats::AIC}}. User can specify custom function.
#' @param crossover_parents a function for crossover between mate pairs. User can specify custom function.
#' @param crossover_method a character string describing crossover method. Default is multi-point crossover.
#' @param pCrossover a numeric value for he probability of crossover for each mate pair.
#' @param start_chrom a numeric value for the  size of the popuation of chromosomes. Default is \code{choose(C, 2)} \eqn{\le 200}, where C is number of predictors.
#' @param mutation_rate a numeric value for rate of mutation. Default is \eqn{1 / (P \sqrt C)}, where P is number of chromosomes, and C is number of predictors.
#' @param converge a logical value indicating whether algorithm should attempt to converge or run for specified number of iterations. If \code{TRUE}, convergence will occur when highest ranked chromosomes is equal to mean of top 50\% in current and previous generation.
#' @param tol a numeric value indicating convergence tolerance. Default is 1e-4.
#' @param iter an integer specifying maximum number of generations algorithm will produce. Default is 100
#' @param minimize a logical value indicating whether optimize should be minimized (TRUE) or maximized (FALSE).
#' @param parallel a logical value indicating whether chromosmes should be evaluated using parallel processes. See \code{\link{evaluate_fitness}}.
#'
#'If user wants to use custom objective_function, they must use a function that is compatible with \code{\link{lm}} or \code{\link{glm}} fitted objects which output a numberic value of length 1.
#'
#' @examples
#' # test
#' rm(list = ls())
#'
#' set.seed(1111)
#'
#' # simulate data for gaussian GLM
#' library(simrel)
#' library(GA)
#'
#' n <- 100 # number obs
#' p <- 10 # number predictors
#' m <- 2 # number relevant latent components
#' q <- 5 # number relevant predictors
#' gamma <- 0.2 # speed of decline in eigenvalues
#' R2 <- 0.5 # theoretical R-squared according to the true linear model
#' relpos <- sample(1:p, m, replace = F) # positions of m
#' dat <- simrel(n, p, m, q, relpos, gamma, R2) # generate data
#' x <- dat$X
#' y <- dat$Y
#'
#' \dontrun{test_GA <- GA::select(y, x)}
#'
#' # 1. Generate founders:  200 chromosomes
#' # 2. Evaluate founders
#' # 3. Begin breeding
#' # Generations: 1-2-3-4-5-6-7-8-9-10-11-12-13-14-15-16-17-18-19-20-21-22-23-24-25-26- #### Converged! ####
#'
#' # True data
#' # dat$relpred
#' [1]  1  4  5  7 10
#'
#' # true beta coefficients
#' dat$beta
#'         [,1]
#' # [1,] -0.21787024
#' # [2,]  0.00000000
#' # [3,]  0.00000000
#' # [4,] -0.68961539
#' # [5,]  0.67167213
#' # [6,]  0.00000000
#' # [7,]  0.03082501
#' # [8,]  0.00000000
#' # [9,]  0.00000000
#' # [10,]  0.20498849
#'
#' # GA output
#' test_GA$BestModel
#' # [1] "6"  "10" "11" "12" "19" "21" "25"
#'
#' test_GA$optimize
#' #$optimize$obj_func
#' # [1] "AIC"
#' # $optimize$value
#' # [1] 231.2034
#' # $optimize$minimize
#' # [1] TRUE
#' # $optimize$method
#' # [1] "method1"
#'
#' @export

select <- function(Y, X, family = "gaussian",
                  objective_function = stats::AIC,
                  crossover_parents_function = crossover_parents,
                  crossover_method = c("method1", "method2", "method3"),
                  pCrossover = 0.8,
                  start_chrom = NULL,
                  mutation_rate = NULL,
                  converge = TRUE,
                  tol = 1e-4,
                  iter = 100,
                  minimize = TRUE,
                  parallel = FALSE) {


    require(parallel)

    ########
    #error checking
    ########

    # X
    if (!is.matrix(X) & !is.data.frame(X)) stop("X must be matrix or dataframe")

    # Y
    if (!is.vector(Y) & !is.matrix(Y) & ncol(Y) > 1) stop("Y must be vector or 1 dimensional matrix")

    # family
    if (family == "gaussian" & all(Y %% 1 == 0)) {cat("Warning: outcome distribution is are 1, 0 integer, family == 'gaussian' may not be suitable")}
    if (family == "gamma" & sum(Y > 0) > 0) {cat("Warning: outcome values < 0, family == 'gamma' may produce errors")}

    # objective_function
    if (!is.function(objective_function)) stop("Error: objective_function must be a function")

    # crossover_parents
    if (!is.function(crossover_parents_function)) stop("Error: crossover_parents must be a function")

    # crossover_method
    if (!is.character(crossover_method)) stop("Error: crossover_method should be a character string")
    if (length(crossover_method) > 1) crossover_method <- crossover_method[1]

    # pCrossover
    if (!is.numeric(pCrossover) | pCrossover < .Machine$double.eps | pCrossover > 1) stop("Error: pCrossover must be number between 0 and 1")
    if (pCrossover < 0.5) cat("Warning: pCrossover < 0.5 may not reach optimum")

    # mutation_rate
    if (!is.null(mutation_rate)) {
        if(!is.numeric(mutation_rate)) stop("Error: mutation rate must be numeric")
        if(mutation_rate < 0 | mutation_rate > 1) stop("Error: mutation rate must be bewteen 0 and 1")
    }

    # converge
    if (!is.logical(converge)) stop("Error: converge must be logical (TRUE/FALSE). Default is TRUE")

    # tol
    if (!is.numeric(tol)) stop("Error: tol must be numeric. Default is 1e-4")

    # iter
    if (!is.numeric(iter)) stop("Error: iter must be numeric")
    if (length(iter) > 1) stop("Error: iter be of length one")

    # minimize
    if (!is.logical(minimize)) stop("Error: minimize must be logical (TRUE/FALSE). Default is TRUE")

    # parallel
    if (!is.logical(parallel)) stop("Error: parallel must be logical (TRUE/FALSE). Default is TRUE")


    ##########
    # Perform genetic algorithm
    #########
    t1 <- Sys.time() # timing

    # Step 1: Generate founders ----------------
    generation_t0 <- generate_founders(X, start_chrom)
    P <- nrow(generation_t0) #num chromosomes
    cat("1. Generate founders: ", P, "chromosomes")


    # Step 2. Evaluate founder fitness Fitness of inital pop ----------------
    cat("\n2. Evaluate founders")
    obj_fun_output_t0 <- evaluate_fitness(generation_t0, Y, X,
                                        family,
                                        parallel, minimize,
                                        objective_function,
                                        rank_objective_function)

    #create array to store fitness data for each iteration
    convergeData <- array(dim = c(P, 2, 1)) #P x 2 x iter

    #save founder fitness evaluation data
    convergeData[, , 1] <- obj_fun_output_t0[
                            order(obj_fun_output_t0[, 2], decreasing = T),
                            c(1, 3)]

    # Step 3. loop through successive generations  ----------------
    cat("\n3. Begin breeding \n Generations: ")
    t1 <- c(t1, Sys.time())
    for (i in 1:iter) {

        # 1. create next generation ----------------
        if (i == 1) {
            generation_t1 <- generation_t0
            obj_fun_output_t1 <- obj_fun_output_t0
        }

        generation_t1 <- create_next_generation(generation_t1,
                                            obj_fun_output_t1,
                                            select_parents,
                                            crossover_method,
                                            crossover_parents_function,
                                            pCrossover,
                                            mutate_child,
                                            mutation_rate)

        # 2. evaluate children fitness ----------------
        obj_fun_output_t1 <- evaluate_fitness(generation_t1, Y, X,
                                            family,
                                            parallel, minimize,
                                            objective_function,
                                            rank_objective_function)

        # store fitness data
        convergeData <- abind::abind(convergeData,
                                obj_fun_output_t1[order(obj_fun_output_t1[, 2],
                                decreasing = T), c(1, 3)])

        # cat generation and save timing
        cat(i, "-", sep = "")

        # 3. check convergence ----------------
        if (i > 10 & isTRUE(converge)) {
            if(isTRUE(all.equal(mean(convergeData[1:(P * 0.25), 2, i]),
                                convergeData[1, 2, i],
                                check.names = F,
                                tolerance = tol)) &
               isTRUE(all.equal(mean(convergeData[1:(P * 0.25), 2, (i - 1)]),
                                convergeData[1, 2, i],
                                check.names = F,
                                tolerance = tol))) {
                cat("\n#### Converged! ####")
                break
            }
        }
    }

    # Step 4. process output ----------------
    t1 <- c(t1, Sys.time())
    best_scores <- convergeData[, , i]
    best_scores <- best_scores[best_scores[, 2] == best_scores[1, 2], ]
    bestModel <- generation_t1[convergeData[, 1, i], ]
    value <- convergeData[1, 2, dim(convergeData)[3]]
    if(dim(convergeData)[3] < iter) {converged <- "Yes"
    } else {converged <- "No"}

    output <- list("Best_model" =
                       colnames(x)[round(colMeans(bestModel[1:dim(best_scores)[1], ]), 0) == 1],
                   optimize = list("obj_func" = paste(substitute(objective_function))[3],
                                 value = as.numeric(round(value, 4)),
                                 minimize = minimize,
                                 method = crossover_method),
                   iter = dim(convergeData)[3],
                   converged = converged,
                   convergeData = convergeData,
                   timing = t1)
    class(output) <- "GA"
    return(output)
}


