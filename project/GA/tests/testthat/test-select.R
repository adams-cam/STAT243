context('Variable selection using genetic algorithms')

y <- mtcars$mpg
x <- as.matrix(mtcars[,c(-1)])


test_that('Test the invalid inputs',{
  expect_error(select(y, x, family = "gaussian", nCores = 1.5))
  # invalid input for 'nCores', which should be integer
  
  expect_error(select(y, x, family = "gaussian", pCrossover = 1.2))
  # invalid input for 'pCrossover', which should be integer
  
  expect_error(select(y, x, family = "gaussian", pCrossover = 0.2))
  # invalid input for 'pCrossover', which should be integer
  
  expect_error(select(y, x, family = "gaussian", start_chrom = 10.5))
  # invalid input for 'start_chrom', which should be integer
  
  expect_error(select(y, x, family = "gaussian", mutation_rate = 2))
  # invalid input for 'mutation_rate', which should be integer
  
  expect_error(select(y, x, family = "gaussian", mutation_rate = 0.5))
  # invalid input for 'mutation_rate', which should be integer
  
  expect_error(select(y, x, family = "gaussian", iter = 25.5))
  # invalid input for 'iter', which should be integer
  
  expect_error(select(x, family = "gaussian"))
  # missing input for 'y', which should be a vector
})

test_that('Test different family',{
  y1 <- sample(c(0,1),32,replace = TRUE)
  # create a new response variable for binomial and poisson family
  
  expect_output(select(y1, x, family = "binomial"))
  # test the whether the binomial family works for the function
  
  expect_output(select(y1, x, family = "poisson"))
  # test the whether the poisson family works for the function
})
