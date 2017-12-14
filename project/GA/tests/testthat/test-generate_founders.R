context("generate_founders")

y <- mtcars$mpg
x <- as.matrix(mtcars[,c(-1)])

t0 <- generate_founders(x,12)

# test the dimension, class, and type of the output
test_that('generate_founders works',
          {expect_equal(dim(t0)[1], 12)
            expect_true(is.matrix(t0))
            expect_true(is.numeric(t0))
            expect_false(0 %in% apply(t0, 1, sum))})
