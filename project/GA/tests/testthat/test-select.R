context('Variable selection using genetic algorithms')

y <- mtcars$mpg
x <- as.matrix(mtcars[,c(-1)])

library(MASS)
fit <- lm(mpg~., data = mtcars)
step <- stepAIC(fit, direction="both")
step

# test for method1
test_that('Variable selection using genetic algorithms',{
  test1 <- select(y, x, family = "gaussian", objective_function = stats::AIC,
                  crossover_parents_function = crossover_parents,
                  crossover_method = 'method1', pCrossover = 0.8,
                  start_chrom = NULL, mutation_rate = NULL, converge = TRUE,
                  tol = 1e-04, iter = 100, minimize = TRUE, parallel = FALSE)
  
  expect_equal(test1$optimize$value, AIC(lm(mpg~wt+qsec+am, data = mtcars)),
               tolerance = 1e0)
})

# test for method2
test_that('Variable selection using genetic algorithms',{
  test2 <- select(y, x, family = "gaussian", objective_function = stats::AIC,
                  crossover_parents_function = crossover_parents,
                  crossover_method = 'method2', pCrossover = 0.8,
                  start_chrom = NULL, mutation_rate = NULL, converge = TRUE,
                  tol = 1e-04, iter = 100, minimize = TRUE, parallel = FALSE)
  
  expect_equal(test2$optimize$value, AIC(lm(mpg~ wt+qsec+am, data = mtcars)), 
               tolerance = 1e0)
})

# test for method3
test_that('Variable selection using genetic algorithms',{
  test3 <- select(y, x, family = "gaussian", objective_function = stats::AIC,
                  crossover_parents_function = crossover_parents,
                  crossover_method = 'method3', pCrossover = 0.8,
                  start_chrom = NULL, mutation_rate = NULL, converge = TRUE,
                  tol = 1e-04, iter = 100, minimize = TRUE, parallel = FALSE)
  
  expect_equal(test3$optimize$value, AIC(lm(mpg~wt+qsec+am, data = mtcars)),
               tolerance = 1e0)
})
