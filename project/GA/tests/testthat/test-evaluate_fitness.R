context("evaluate_fitness")

# test data ----------------
Y <- as.matrix(mtcars$mpg)
X <- as.matrix(mtcars[2:ncol(mtcars)])

# get input data
C <- dim(X)[2] # number genes
P <- 2 * C # number of chromosomes

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

# serial evaluation
test_that('serial fitness evaluation works',
          {test <- evaluate_fitness(generation_t0, Y, X,
                                     family = "gaussian",
                                     nCores = 1, minimize = TRUE,
                                     objective_function = stats::AIC,
                                     rank_objective_function)
          expect_is(test, "matrix")
          expect_type(test, "double")
})

# parallel evaluation
test_that('parallel fitness evaluation works',
          {test <- evaluate_fitness(generation_t0, Y, X,
                                    family = "gaussian",
                                    nCores = 2, minimize = TRUE,
                                    objective_function = stats::AIC,
                                    rank_objective_function)
          expect_is(test, "matrix")
          #expect_type(test, "double")
})

# test maximize evaluation and other objective functions
test_that('Other objective_functions work',
          {test <- evaluate_fitness(generation_t0, Y, X,
                                    family = "gaussian",
                                    nCores = 1, minimize = FALSE,
                                    objective_function = stats::logLik,
                                    rank_objective_function)
          expect_is(test, "matrix")
          expect_type(test, "double")
})


