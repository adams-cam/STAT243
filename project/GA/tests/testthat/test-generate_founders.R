context("generate_founders")

y <- mtcars$mpg
x <- as.matrix(mtcars[,c(-1)])

C <- dim(x)[2]
P <- 2 * C

# test the dimension, class, and type of the output
test_that('generate_founders works',
          {test <- generate_founders(x, NULL)
            expect_equal(dim(test)[2], C)
            expect_equal(dim(test)[1], P)
            expect_true(is.matrix(test))
            expect_true(is.numeric(test))
            expect_false(any(rowSums(test == 0) == C))})

test_that('generate_founders works with user defined chrom_start',
          {test <- generate_founders(x, 25)
          expect_equal(dim(test)[2], C)
          expect_equal(dim(test)[1], 25)
          expect_true(is.matrix(test))
          expect_true(is.numeric(test))
          expect_false(any(rowSums(test == 0) == C))})
