setwd("/Users/two/stat243project/GA/R/STAT243/project/GA/tests/testthat")
context("create_next_generation")

# test data
Y <- as.matrix(mtcars$mpg)
X <- as.matrix(mtcars[2:ncol(mtcars)])
dim(X)

# get input data
C <- dim(X)[2] # number genes
P <- 2* C # number of chromosomes

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


dim(generation_t0)


obj_fun_output <- evaluate_fitness(generation_t0, Y, X,
                                   family = "gaussian",
                                   nCores = 1, minimize = TRUE,
                                   objective_function = stats::AIC,
                                   rank_objective_function)


crossover_method<-"method1"
# basic evaluation
test_that('create next generation works',
          {
          #Assign a probability in crossovber
          pCrossover<-1
            
          #Assign mutation rate
          mutation_rate<-0.1
          test <- create_next_generation(generation_t0, obj_fun_output,
                                                      select_parents,
                                                      crossover_method,
                                                      crossover_parents,
                                                      pCrossover,
                                                      mutate_child,
                                                      mutation_rate) 
          expect_equal(dim(test), c(20,10))
          expect_is(test, "matrix")
          expect_type(test, "integer")
          })

# another one with different probability
test_that('other probability works',
          {
          #Assign a probability in crossovber
          pCrossover<-0
          
          #Assign mutation rate
          mutation_rate<-1
          test <- create_next_generation(generation_t0, obj_fun_output,
                                         select_parents,
                                         crossover_method,
                                         crossover_parents,
                                         pCrossover,
                                         mutate_child,
                                         mutation_rate) 
          expect_equal(dim(test), c(20,10))
          expect_is(test, "matrix")
          expect_type(test, "integer")
          })

# test whether invalid input will throw error
test_that('serial fitness evaluation works',
          
          {
          #Assign a probability in crossovber
          pCrossover<-1.1
            
          #Assign mutation rate
          mutation_rate<-1  
          expect_error(test <- create_next_generation(generation_t0, obj_fun_output,
                                                      select_parents,
                                                      crossover_method,
                                                      crossover_parents,
                                                      pCrossover,
                                                      mutate_child,
                                                      mutation_rate)) 
          })
test_that('serial fitness evaluation works',
          
          {
            #Assign a probability in crossovber
            pCrossover<--0.1
            
            #Assign mutation rate
            mutation_rate<-1  
            expect_error(test <- create_next_generation(generation_t0, obj_fun_output,
                                                        select_parents,
                                                        crossover_method,
                                                        crossover_parents,
                                                        pCrossover,
                                                        mutate_child,
                                                        mutation_rate)) 
          })
test_that('serial fitness evaluation works',
          
          {
            #Assign a probability in crossovber
            pCrossover<-1
            
            #Assign mutation rate
            mutation_rate<--1  
            expect_error(test <- create_next_generation(generation_t0, obj_fun_output,
                                                        select_parents,
                                                        crossover_method,
                                                        crossover_parents,
                                                        pCrossover,
                                                        mutate_child,
                                                        mutation_rate)) 
          })
test_that('serial fitness evaluation works',
          
          {
            #Assign a probability in crossovber
            pCrossover<-1
            
            #Assign mutation rate
            mutation_rate<-1.3  
            expect_error(test <- create_next_generation(generation_t0, obj_fun_output,
                                                        select_parents,
                                                        crossover_method,
                                                        crossover_parents,
                                                        pCrossover,
                                                        mutate_child,
                                                        mutation_rate)) 
          })

#Make sure other method works
test_that('methods 2 works',
          
          {
            #Assign a probability in crossovber
            pCrossover<-1
            #Assign Different methods
            crossover_method<-"method2"
            #Assign mutation rate
            mutation_rate<-1  
            test <- create_next_generation(generation_t0, obj_fun_output,
                                           select_parents,
                                           crossover_method,
                                           crossover_parents,
                                           pCrossover,
                                           mutate_child,
                                           mutation_rate) 
            expect_equal(dim(test), c(20,10))
            expect_is(test, "matrix")
            expect_type(test, "integer")
          })

test_that('methods 3 works',
          
          {
            #Assign a probability in crossovber
            pCrossover<-1
            
            #Assign mutation rate
            mutation_rate<-1  
            
            #Assign Method
            crossover_method<-"method3"
            test <- create_next_generation(generation_t0, obj_fun_output,
                                           select_parents,
                                           crossover_method,
                                           crossover_parents,
                                           pCrossover,
                                           mutate_child,
                                           mutation_rate) 
            expect_equal(dim(test), c(20,10))
            expect_is(test, "matrix")
            expect_type(test, "integer")
          })
