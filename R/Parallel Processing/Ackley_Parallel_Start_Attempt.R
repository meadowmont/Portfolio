#Parallel start optim attempt 
#Below is the Rosenbrock attempt
#Rosenbrock function

#load packages
library(parallel)
library(foreach)
library(doParallel)

ackley <- function(xx)
{
  ##########################################################################
  #
  # ACKLEY FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUTS:
  #
  # xx = c(x1, x2, ..., xd)
  # a = constant (optional), with default value 20
  # b = constant (optional), with default value 0.2
  # c = constant (optional), with default value 2*pi
  #
  ##########################################################################
  
  
  a=20; b=0.2; c=2*pi
  
  d <- length(xx)
  
  sum1 <- sum(xx^2)
  sum2 <- sum(cos(c*xx))
  
  term1 <- -a * exp(-b*sqrt(sum1/d))
  term2 <- -exp(sum2/d)
  
  y <- term1 + term2 + a + exp(1)
  return(y)
}


rastr <- function(xx)
{
  ##########################################################################
  #
  # RASTRIGIN FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, ..., xd)
  #
  ##########################################################################
  
  d <- length(xx)
  
  sum <- sum(xx^2 - 10*cos(2*pi*xx))
  
  y <- 10*d + sum
  return(y)
}

# gather results from all processes and find the overall best solution
# Rosenbrock Banana function
Rosenbrock_f <- function(x) {
  return ( 100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2 )
}

# # bounds (lb, ub) (restrictions on the variables)
# lb <- c(-3, -5)      
# ub <- c(4, 2)
# # -3 <= x1 <= 4   and  -5 <= x2 <= 2
# n <- length(lb)
# numstart <- 4

# initialize start time
start_time <- Sys.time()

# bounds (lb, ub) (restrictions on the variables)
dim <- 2
lb <- rep(-32.768, dim)
ub <- rep(32.768, dim)

# number of cores to use 
num_process <- 8

# number of batches
num_batches <- 12

# -32.768 <= xi <= 32.768
n <- length(lb)

set.seed(42) #make sure it replicates

# optimize from every starting point
optimize_start <- function(start_point) {
  optim(par = start_point, fn= ackley, lower = lb, upper = ub)
}

# initialize all_results and results
all_results <- list()

#for loop
for (i in 1:num_batches) {
  # initialize 
  initsols <- replicate(num_process, lb +  runif(n)*(ub - lb))  # generates initsols within bounds
  # run Rosenbrock simulation on the datasets in parallel
  cl <- makeCluster(num_process)
  registerDoParallel(cl)
  
  # optimize every interation in parallel
  results <- foreach(start_point = 1:num_process) %dopar% {
    optimize_start(initsols[ ,start_point])}
  
  # append results
  all_results <- c(all_results, results)
  
  # stop the parallel backend
  stopCluster(cl)
  
}


# gather results from all processes and find the overall best solution
best_result <- all_results[[1]]
for (i in 2:(num_process * num_batches)) {
  if (all_results[[i]]$value < best_result$value) {
    best_result <- all_results[[i]]
  }
}

# record end time 
end_time <- Sys.time()

# Print the execution times
cat("Parallel (type 1) execution time:",
    end_time - start_time, "\n")

