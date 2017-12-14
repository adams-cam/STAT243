context('Rank parents accourding to their fitness')

objfunout <- runif(100,0,1)
rnk1 <- rank_objective_function(objfunout,T)

# Test the dimension, class, and type of the output of rank_objective_function
# with minimize set to TRUE.
test_that('test rank_objective_function works',{
  expect_equal(dim(rnk1), c(100,3))
  expect_true(is.numeric(rnk1))
  expect_true(is.matrix(rnk1))
})

rnk2 <- rank_objective_function(objfunout,T)

# Test the dimension, class, and type of the output of rank_objective_function
# with minimize set to FALSE.
test_that('test rank_objective_function works',{
  expect_equal(dim(rnk2), c(100,3))
  expect_true(is.numeric(rnk2))
  expect_true(is.matrix(rnk2))
})

