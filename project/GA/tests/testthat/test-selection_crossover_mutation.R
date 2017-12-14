context("selection_crossover_mutation")

#test data
Y <- as.matrix(mtcars$mpg)
X <- as.matrix(mtcars[2:ncol(mtcars)])
dim(X)

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

parentInd <- sample(1:P, 2, replace = F)
parent_rank <- 1:P

child <-
# select_parents ----------------
test_that("select_parents works ",
          {test <- select_parents(parent_rank)
              expect_is(test, "integer")
              expect_type(test, "integer")
              expect_true(all(!is.na(test))) # not all 0's
              expect_equal(length(test), 2) # 2 children
              expect_true(all(test > 0 & test <= P)) # C chromosomes
})


# crossover_parents ----------------
test_that("crossover_parents works and crossover method 1 works",
          {test <- crossover_parents(generation_t0, parentInd,
                        crossover_method = "method1", pCrossover = 1, parent_rank)
          expect_is(test, "matrix")
          expect_type(test, "integer")
          expect_true(any(test == 1 | test == 0)) # O's and 1's
          expect_true(!all(test == 0)) # not all 0's
          expect_equal(dim(test)[1], 2) # 2 children
          expect_equal(dim(test)[2], C) # C chromosomes
          })

test_that("crossover_parents works and crossover method 2 works",
          {test <- crossover_parents(generation_t0, parentInd,
                        crossover_method = "method2", pCrossover = 1, parent_rank)
            expect_is(test, "matrix")
            expect_type(test, "integer")
            expect_true(any(test == 1 | test == 0)) # O's and 1's
            expect_true(!all(test == 0)) # not all 0's
            expect_equal(dim(test)[1], 2) # 2 children
            expect_equal(dim(test)[2], C) # C chromosomes
            })

test_that("crossover_parents works and crossover method 3 works",
          {test <- crossover_parents(generation_t0, parentInd,
                                     crossover_method = "method3", pCrossover = 1, parent_rank)
            expect_is(test, "matrix")
            expect_type(test, "integer")
            expect_true(any(test == 1 | test == 0)) # O's and 1's
            expect_true(!all(test == 0)) # not all 0's
            expect_equal(dim(test)[1], 2) # 2 children
            expect_equal(dim(test)[2], C) # C chromosomes
            })

# mutate_child ----------------
mutation_rate <- NULL
child <- stats::rbinom(C, 1, runif(1, min = 0.35, max = 0.65))

test_that("mutate_child works when mutation_rate is null",
          {test <- mutate_child(mutation_rate, child, P, C)
            expect_type(test, "integer")
            expect_true(any(test == 1 | test == 0)) # O's and 1's
            expect_true(!all(test == 0)) # not all 0's
            expect_equal(length(test), C) # C chromosomes
          })

test_that("mutate_child works when user specifies a mutation rate",
          {test <- mutate_child(mutation_rate = 0.01, child, P, C)
          expect_type(test, "integer")
          expect_true(any(test == 1 | test == 0)) # O's and 1's
          expect_true(!all(test == 0)) # not all 0's
          expect_equal(length(test), C) # C chromosomes
          })

test_that("mutate_child works when user specifies a mutation rate",
          {test <- mutate_child(mutation_rate = 0.01, child, P, C)
          expect_type(test, "integer")
          expect_true(any(test == 1 | test == 0)) # O's and 1's
          expect_true(!all(test == 0)) # not all 0's
          expect_equal(length(test), C) # C chromosomes
          })

test_that("mutate_child breaks when user specifies an incorrect mutation rate",
            {expect_error(mutate_child(mutation_rate = 1.2, child, P, C))
            expect_error(mutate_child(mutation_rate = -0.2, child, P, C))
          })


