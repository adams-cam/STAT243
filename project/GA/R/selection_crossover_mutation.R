################################################################
#
# parent selection, crossover, and child mutation functions
# Xu, Weijie; Chen, Yuwen; Adams, Cameron; Zhou, Yilin
#
# Final Projct
# STAT 243
# Fall 2017
#
################################################################

#' Selection of Parents for Crossover
#'
#' Selection of first parent is done according to fitness ranks generated with \code{\link{rank_objective_function}}. Probably of parent rank, \eqn{\frac{2\ r_{i}}{P(P+1)}}, from \eqn{i = 1,...,P}, where rank_i are fitness ranks of each parent chromosomes, and P is number of of parent chromosomes. The second parent is selected randomly and will not be the same as the previous parent. Fucntion returns ranks of select parents to be used for \code{\link{crossover_parents}}.
#'
#' @param parent_rank a vector indicating the rank of parents chromosomes. Ranking order is inverse: parent chromosomes with lowest fitness rank will have rank == 1.
#'
#' References:
#' Geof H. Givens, Jennifer A. Hoeting (2013) Combinatorial Optimization (italicize). Chapter 3 of Computational Statistics (italicize).
#' @export


select_parents <- function(parent_rank) {

    # get number of chromosomes
    P <- length(parent_rank)

    # probability of selection
    phi <- (2 * parent_rank) / (P * (P + 1))

    # select first parent by parent_rank, second random
    parent1 <- base::sample(1:P, 1, prob = phi, replace = T)
    parent2 <- base::sample((1:P)[-parent1], 1, replace = T)

    return(c(parent1, parent2))
}

#' Parent Chromosome Crossover
#'
#' Parent chromosomes selected by \code{\link{select_parents}} will be used to generate new children using concept of crossover to combine genetic data to produce offspring. Parents are selected for crossover according to probability of crossover. If crossover does not occour, parent chromosomes are passed exactly to offpsring. This function produces two children from each parent chromosomes mate pair using one of three methods.
#'
#' \describe{
#' \item{Method 1}{Multipoint crossover. Parent data is exchanged at three randomly selected crossover points along the chromosome to produce two offspring.}
#' \item{Method 2}{Probabilities for each gene are created from according to relative rankings of each parent and then child genotypes are sampled from \code{\link{rbinom}} using those probabilities.}
#' \item{Method 3}{Condordant mate pair genes are passed directly offspring, and non-corcordant genes are sampled from \code{\link{rbinom}} using probabilities proportional to relative ranking of each parent.}
#' }
#'
#' @param generation_t0 a matrix of parent chromosomes to be evaluated. Columns correspond to predictors/genes and rows correspond to parents/chromosomes.
#' @param parentInd a vector containing the rank indexes of two parents selected for crossover.
#' @param crossover_method a character string indicating which method of crossover to be used, default is method 1, multipoint crossover.
#' @param pCrossover a number between 0 and 1 indicating the probability of crossover for each mate pair. Default is 0.8.
#' @param parent_rank a integer vector of fitness ranks for parent chromosomes.
#'
#' References:
#' Geof H. Givens, Jennifer A. Hoeting (2013) Combinatorial Optimization (italicize). Chapter 3 of Computational Statistics (italicize).
#' @export

crossover_parents <- function(generation_t0, parentInd,
                              crossover_method, pCrossover, parent_rank)  {

    # get parent info
    parent1 <- generation_t0[parentInd[1], ]
    parent2 <- generation_t0[parentInd[2], ]
    C <- length(parent1)
    parent1r <- parent_rank[parentInd[1]]
    parent2r <- parent_rank[parentInd[2]]

    if (stats::rbinom(1, 1, pCrossover) == 1 ) {
        if (crossover_method == "method1") {

            #METHOD 1 ----------------
            #multipoint crossover: three crossover points
            cross <- sort(sample(seq(2,(C - 2), 2), 3, replace = F))

            child1 <- as.integer(c(parent1[1:cross[1]],
                            parent2[(cross[1] + 1):cross[2]],
                            parent1[(cross[2] + 1):cross[3]],
                            parent2[(cross[3] + 1):C]))
            child2 <- as.integer(c(parent2[1:cross[1]],
                            parent1[(cross[1] + 1):cross[2]],
                            parent2[(cross[2] + 1):cross[3]],
                            parent2[(cross[3] + 1):C]))

        } else if (crossover_method == "method2") {

            #METHOD 2 ----------------
            #method upweights parent with higher parent_rank high
            childProb <- parent1 * parent1r[1] /
                (parent1r + parent2r) +
                parent2 * parent2r /
                (parent1r + parent2r)
            child1 <- stats::rbinom(C, 1, prob = childProb)
            child2 <- stats::rbinom(C, 1, prob = childProb)

        } else if (crossover_method == "method3") {

            #METHOD 3 ----------------
            #randomly samples non-concordant variables between parents
            # slightly upweights parent selected by prob. proportional to parent_rank
            child1 <- parent1
            child2 <- parent2
            child1[parent1 != parent2] <-
                stats::rbinom(sum(parent1 - parent2 != 0), 1,
                       prob = parent1r / (parent1r + parent2r))
            child2[parent1 != parent2] <-
                stats::rbinom(sum(parent1 - parent2 != 0), 1,
                       prob = parent2r / (parent1r + parent2r))
        }
        return(rbind(as.integer(child1), as.integer(child2)))
    } else {
        child1 <- parent1
        child2 <- parent2
        return(rbind(child1, child2))
    }
}


#' Child Chromosome Mutagenesis.
#'
#' Child chromosomes produced using \code{\link{crossover_parents}} passed to this function will be modified using concept of genetic mutation, whereby a polymorphisms are produced according to a probably of mutatgenesis. Here, the default mutation rate is proportional to the size of the popluation, P, and number of chromosomes, C, \eqn{\frac{1}{P * C^{0.5}}}. User's can specify the mutation rate in \code{\link{select}}.
#'
#' @param mutation_rate an optional numeric value between 0 and 1 indicating mutation rate.
#' @param child a vector containing the chromosome of child produced by crossover.
#' @param P an integer indicating the number of parent chromosomes.
#' @param C an integer indicating the number of predictor variables or genes.
#'
#' @export

mutate_child <- function(mutation_rate, child, P, C) {

    if (is.null(mutation_rate)) {
        return(as.integer(abs(round(child, 0) -
                        stats::rbinom(C, 1,
                                    prob = 1 / (P * sqrt(C))))))
    } else {

        if (mutation_rate > 1 | mutation_rate < 0) {
            stop("Error: mutation_rate must be between 0 and 1")
        }

        return(as.integer(abs(round(child, 0) -
                        stats::rbinom(C, 1,
                                    prob = mutation_rate))))
    }
}


