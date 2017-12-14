context("create_next_generation")

# test data
Y <- as.matrix(mtcars$mpg)
X <- as.matrix(mtcars[2:ncol(mtcars)])
dim(X)

# get input data
C <- dim(X)[2] # number genes
P <- 2* C # number of chromosomes

# generate chromosomes to test
geneSample <- sample(c(0, 1),
                     replace = TRUE,
                     size = ceiling(1.2 * C * P))

x <- seq_along(geneSample)
firstGen <- split(geneSample, ceiling(x / C))
generation_t0 <- matrix(unlist(unique(firstGen)[1:P]),
                        ncol = C, byrow = TRUE)
generation_t0 <- generation_t0[apply(generation_t0, 1,
                                     function(x) !all(x == 0)), ]

obj_fun_output <- evaluate_fitness(generation_t0, Y, X,
                                   family = "gaussian",
                                   nCores = 1, minimize = TRUE,
                                   objective_function = stats::AIC,
                                   rank_objective_function)



# evaluation
test_that('test that create_next_generation works with pCrossover = 1',
          {test <- create_next_generation(generation_t0, obj_fun_output,
                                                      select_parents,
                                                      crossover_method = "method1",
                                                      crossover_parents,
                                                      pCrossover = 1,
                                                      mutate_child,
                                                      mutation_rate = 0.01)
          expect_equal(dim(test), c(20,10))
          expect_is(test, "matrix")
          expect_type(test, "integer")
})

# another one with different probability
test_that('test that create_next_generation works with pCrossover != 1',
          {expect_error(create_next_generation(generation_t0, obj_fun_output,
                                         select_parents,
                                         crossover_method,
                                         crossover_parents,
                                         pCrossover = 0.8,
                                         mutate_child,
                                         mutation_rate = 0.01))
})


