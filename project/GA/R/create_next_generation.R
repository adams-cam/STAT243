################################################################
#
# create next generation function
# Xu, Weijie; Chen, Yuwen; Adams, Cameron; Zhou, Yilin
#
# Final Projct
# STAT 243
# Fall 2017
#
################################################################

#' Create next generation from parent chromosomes.
#'
#' Using fitness ranks of parent chromosomes, this function will select parents based upon their fitness rank, generate two children from each mate pair and mutate those child chromosomes. Function uses \code{\link{select_parents}}, \code{\link{crossover_parents}}, and \code{\link{mutate_child}} to generate children.
#'
#' @param generation_t0 a matrix of parent chromosomes to be evaluated. Columns correspond to predictors/genes and rows correspond to parents/chromosomes.
#' @param obj_fun_output a numeric vector containg the objective function output for each parent chromosome.
#' @param select_parents function for selection of parents for \code{\link{crossover_parents}}. See \code{\link{select_parents}}.
#' @param crossover_method a character string indicating which crossover method to use: c("method1", "method2", "method3"). Default is "method1". See \code{\link{crossover_parents}}
#' @param crossover_parents a function for crossover between two parents. See \code{\link{crossover_parents}}.
#' @param pCrossover a number between 0 and 1 indicating the probability of crossover between two parents.
#' @param mutate_child a function for mutagenesis of child genes. See \code{\link{mutate_child}}.
#' @param mutation_rate a number between 0 and 1 indicating the probability of mutation. See \code{\link{mutate_child}}.
#'
#'
#' @export

create_next_generation <- function(generation_t0, obj_fun_output,
                                   select_parents,
                                   crossover_method,
                                   crossover_parents,
                                   pCrossover,
                                   mutate_child,
                                   mutation_rate) {

    # set variables
    P <- dim(generation_t0)[1]
    C <- dim(generation_t0)[2]
    parent_rank <- obj_fun_output[, 2]

    #Create matrix for next generation
    generation_t1 <- matrix(NA, dim(generation_t0)[1], dim(generation_t0)[2])

    #########
    #Selection, Crossover, and Mutation
    #########

    i <- 1 #initialize while loop
    while(i <= dim(generation_t1)[1]) {

        # Selection ----------------
        parentInd <- select_parents(parent_rank)

        # Crossover ----------------
        children <- crossover_parents(generation_t0, parentInd,
                                      crossover_method, pCrossover, parent_rank)

        # Mutation ----------------
        child1 <- mutate_child(mutation_rate, children[1, ], P, C)
        child2 <- mutate_child(mutation_rate, children[2, ], P, C)

        # Check if all zeros -----------------
        if (#!any(apply(generation_t1, 1, function(x) all(x == child1))) &
            #!any(apply(generation_t1, 1, function(x) all(x == child2))) &
            !all(child1 == 0)  & !all(child2 == 0)) {
            generation_t1[c(i, i + 1), ] <- rbind(child1, child2)
            # update counter
            i <- i + 2
        }
    }

    # return new new generation
    return(generation_t1)
}
