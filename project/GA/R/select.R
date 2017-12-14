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
#' @param objective_function function for computing objective. Default is \code{\link{AIC}}. User can specify custom function.
#' @param crossover_parents_function a function for crossover between mate pairs. User can specify custom function. Default is \code{\link{crossover_parents}}.
#' @param crossover_method a character string describing crossover method. Default is multi-point crossover. See \code{\link{crossover_parents}}.
#' @param pCrossover a numeric value for he probability of crossover for each mate pair.
#' @param start_chrom a numeric value for the  size of the popuation of chromosomes. Default is \code{choose(C, 2)} \eqn{\le 200}, where C is number of predictors.
#' @param mutation_rate a numeric value for rate of mutation. Default is \eqn{1 / (P \sqrt C)}, where P is number of chromosomes, and C is number of predictors.
#' @param converge a logical value indicating whether algorithm should attempt to converge or run for specified number of iterations. If \code{TRUE}, convergence will occur when highest ranked chromosomes is equal to mean of top 50\% in current and previous generation.
#' @param tol a numeric value indicating convergence tolerance. Default is 1e-4.
#' @param iter an integer specifying maximum number of generations algorithm will produce. Default is 100
#' @param minimize a logical value indicating whether optimize should be minimized (TRUE) or maximized (FALSE).
#' @param nCores an integer indicating number of parallel processes to run when evaluating fitness. Default is 1, or no paralleization. See \code{\link{evaluate_fitness}}.
#'
#'If user wants to use custom objective_function, they must use a function that is compatible with \code{\link{lm}} or \code{\link{glm}} fitted objects which output a numberic value of length 1.
#'
#' @examples
#' # Simulated data
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
#' relpos <- base::sample(1:p, m, replace = FALSE) # positions of m
#' dat <- simrel::simrel(n, p, m, q, relpos, gamma, R2) # generate data
#' x <- dat$X
#' y <- dat$Y
#'
#' \dontrun{sim_GA <- GA:select(y, x, family = "gaussian", objective_function = stats::AIC,
#' crossover_method = "method1", pCrossover = 0.8, converge = TRUE, minimize = TRUE, nCores = 1)}
#'
#' # mtcars
#' data(mtcars)
#'
#' y <- mtcars$mpg
#' x <- mtcars[, 2:11]
#'
#' \dontrun{GA_mtcars <- GA:select(y, x, family = "gaussian", objective_function = stats::AIC,
#' crossover_method = "method1", pCrossover = 0.8, converge = TRUE, minimize = TRUE, nCores = 1)}
#'
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
                  nCores = 1L) {



    ########
    #error checking
    ########

    # X
    if (missing(X)) stop("Error: must input a matrix of predictor data")
    if (!is.matrix(X) & !is.data.frame(X)) stop("X must be matrix or dataframe")

    # Y
    if (missing(Y)) stop("Error: must input a vector of reponse data")
    if (!is.vector(Y) & !is.matrix(Y)) stop("Y must be vector or 1 column matrix")
    if (is.matrix(Y) | is.data.frame(Y)) {
        if(ncol(Y) > 1) stop("Y must be vector or 1 column matrix")
    }
    if (length(Y) != dim(X)[1]) stop("Error: X and Y dimensions don't match")

    # family
    if (family == "gaussian" & all(Y %% 1 == 0)) {cat("Warning: outcome distribution is are 1, 0 integer, family == 'gaussian' may not be suitable")}
    if (family == "gamma" & sum(Y > 0) > 0) {cat("Warning: outcome values < 0, family == 'gamma' may produce errors")}
    if (!family %in% c("gaussian", "binomial", "gamma", "poisson")) stop("Error: family argument misspecified")

    # objective_function
    if (!is.function(objective_function)) stop("Error: objective_function must be a function")

    # crossover_parents
    if (!is.function(crossover_parents_function)) stop("Error: crossover_parents must be a function")

    # crossover_method
    if (!is.character(crossover_method)) stop("Error: crossover_method should be a character string")
    if (length(crossover_method) > 1) crossover_method <- crossover_method[1]
    if (!crossover_method %in% c("method1", "method2", "method3")) stop("Error: incorrect crossover method misspecified")

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

    # nCores
    if (!is.integer(nCores)) stop("Error: nCores must be integer of length 1")
    if (nCores > parallel::detectCores()) stop("Error: nCores cannot be larger than detectCores()")
    if (nCores < 1) stop("Error: nCores must be >= 1")


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
                                        nCores, minimize,
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
                                            nCores, minimize,
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

            #if((abs(convergeData[1, 2, i] - convergeData[1, 2, (i - 1)]) /
            #          abs( convergeData[1, 2, (i - 1)])) < tol) {
                cat("\n#### Converged! ####")
                break
            }
        }
    }

    # Step 4. process output ----------------
    t1 <- c(t1, Sys.time())

    # get models with the best fitness
    best_scores <- convergeData[, , i]
    if (sum(best_scores[, 2] == best_scores[1, 2]) > 1) {
        num_best_scores <- sum(best_scores[, 2] == best_scores[1, 2])
    } else {num_best_scores <- 1}
    bestModel <- generation_t1[convergeData[, 1, i], ]

    # other output information
    value <- convergeData[1, 2, dim(convergeData)[3]]
    if(dim(convergeData)[3] < iter) {converged <- "Yes"
    } else {converged <- "No"}

    # create output list
    output <- list("Best_model" =
                       colnames(X)[round(colMeans(bestModel[1:dim(best_scores)[1], ]), 0) == 1],
                    optimize = list("obj_func" = paste(substitute(objective_function))[3],
                                value = as.numeric(round(value, 4)),
                                minimize = minimize,
                                method = crossover_method),
                    iter = dim(convergeData)[3] - 1,
                    converged = converged,
                   convergeData = convergeData,
                   timing = t1)

    # set class
    class(output) <- c("GA", class(output))

    return(output)
}


