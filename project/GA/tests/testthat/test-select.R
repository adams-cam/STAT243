<<<<<<< HEAD
context('selet')

y <- mtcars$mpg
x <- as.matrix(mtcars[,c(-1)])


# test for method1
test_that('GA algorithm works ',
            {test <- GA::select(y, x, family = "gaussian",
                                objective_function = stats::AIC)
              expect_type(test, "list") #list
              expect_s3_class(test, "GA") # of class GA
              expect_length(test, 6)
              expect_type(GA::select(y, x,
                    family = "gaussian",objective_function = stats::AIC)$Best_model,
                        "character")
})

test_that('GA algorithm does not converge',
            {expect_equal(GA::select(y, x, family = "gaussian", iter = 100,
                                mutation_rate = 0.8)$converged, "No")
            expect_equal(GA::select(y, x, family = "gaussian", iter = 100,
                                        mutation_rate = 0.8)$iter, 100)
})

test_that('test for input errors',
            {expect_error(GA::select(y, "foo", family = "gaussian"))
            expect_error(GA::select(x, family = "gaussian"))
            expect_error(GA::select(y[-1], x))
            expect_error(GA::select(y, x, family = "binomial"))
            expect_error(GA::select(y, x, nCores = 1000L))
            expect_error(GA::select(cbind(y, y, y), x))
            expect_error(GA::select(y, x, objective_function = "AIC"))
            expect_error(GA::select(y, x, minimize = "True"))
            expect_error(GA::select(y, x, family = "gessian"))
            expect_error(GA::select(y, x, converge = "Yes please do"))
})

=======
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
>>>>>>> ba1109017fcbc50d196469189416699dc6f8322e
