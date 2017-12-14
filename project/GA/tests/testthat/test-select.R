context('select')

# test data
y <- mtcars$mpg
x <- as.matrix(mtcars[, c(-1)])

# test GA::select
test_that('GA algorithm works ',
            {test <- GA::select(y, x, family = "gaussian",
                                objective_function = stats::AIC)
              expect_type(test, "list") #list
              expect_s3_class(test, "GA") # of class GA
              expect_type(GA::select(y, x,
                    family = "gaussian", objective_function = stats::AIC)$Best_model,
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

