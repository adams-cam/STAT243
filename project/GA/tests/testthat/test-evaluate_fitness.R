context("evaluate_fitness")

# test data
Y <- as.matrix(mtcars$mpg)
X <- as.matrix(mtcars[2:ncol(mtcars)])

# get input data
C <- dim(x)[1] # number genes
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



test_that('serial fitness evaluation works',
          expect_is(evaluate_fitness(generation_t0, Y, X,
                                     family = "gaussian",
                                     parallel = FALSE, minimize = TRUE,
                                     objective_function = stats::AIC,
                                     rank_objective_function), ))

test_that('parallel fitness works',
          expect_is(fitness_parallel(pop = pop,y = y,x = x,family = "gaussian",fitness_function = stats::AIC,ncores = 2), "numeric")
)
