#Problem Set 
#Miguel Pereira

#SparseGrid function
sg.int<-function(g,...,lower,upper)
{ require("SparseGrid")
  
  lower<-floor(lower)
  
  upper<-ceiling(upper)
  
  if (any(lower>upper)) stop("lower must be smaller than upper")
  
  #This gives us all possible combinations of the supplied vectors
  gridss<-as.matrix(expand.grid(seq(lower[1],upper[1]-1,by=1),
                                seq(lower[2],upper[2]-1,by=1)))
  #Note: dimension is what we may want to change
  sp.grid <- createIntegrationGrid( 'KPU', dimension=2, k=5 )
  
  nodes<-gridss[1,]+sp.grid$nodes
  
  weights<-sp.grid$weights
  
  #nrow(gridss) is the number of dimensions
  for (i in 2:nrow(gridss))
    
  {
    nodes<-rbind(nodes,gridss[i,]+sp.grid$nodes)  
    
    weights<-c(weights,sp.grid$weights)
    
  }
  
  #Here is where I should parallelize
  gx.sp <- apply(nodes, 1, g,...)
  val.sp <- gx.sp %*%weights
  val.sp
}




###########
#Exercise 1
###########
library(SparseGrid); library(plyr); library(doParallel)



sg.int.multidim<-function(f, ..., lower,upper, dim, parallel=FALSE){ 
  require("SparseGrid")
  require("plyr")
  
  #The necessary instructions for the parallel option to work
  if (parallel==TRUE){
    require("doParallel")
    registerDoParallel(cores=4)
  }
  
  lower<-floor(lower)
  upper<-ceiling(upper)
  
  #Testing whether the inputs are valid
  if (any(lower>upper)) stop("Lower must be smaller than upper")
  if (length(lower)!=length(upper)) stop("Lower and Upper vectors have to be of the same length")
  
  #This function creates a sequence for each dimensions specified
  gridFun <- function(i){
    seq(lower[i], upper[i]-1, by=1)
  }
  
  #This gives us all possible combinations of the supplied vectors
  gridss <- as.matrix(expand.grid(llply(1:dim, gridFun, .parallel=parallel)))
  
  #Here we create the grids and the nodes required for integration
  sp.grid <- createIntegrationGrid( 'KPU', dimension=dim, k=5 )
  nodes<-gridss[1,]+sp.grid$nodes
  weights<-sp.grid$weights
  
  
  for (i in 2:nrow(gridss)){
    nodes<-rbind(nodes,gridss[i,]+sp.grid$nodes)  
    weights<-c(weights,sp.grid$weights)
  }
  
  #And finally integrating
  gx.sp <- aaply(nodes, 2, f, ..., .parallel=parallel)
  
  val.sp <- gx.sp %*%weights
  return(val.sp)
}


###TESTING THE FUNCTION

#Two dimension
l<-c(1,2,2,3,4)
u<-c(4,6,7,7,7)
f<-function(x) x^2
#Without parallel (2 dimensions)
  #The original function
  sg.int(f,lower=l,upper=u)
  #My function allowing different dimensions and parallel
  sg.int.multidim(f,lower=l,upper=u,dim=2)

  #With parallel
  sg.int.multidim(f,lower=l,upper=u,dim=2, parallel=TRUE)
  
#Without parallel (3 dimensions)
  sg.int.multidim(f,lower=l,upper=u,dim=3)
  


###########
#Exercise 2
###########

#Already above


###########
#Exercise 3
###########
library(testthat)


#Testing correct inputs
test_that("Correct Inputs", {
  # one-dimensional function
    example1 <- function(x){ 
    return(x^3)
  }
  expect_error(sg.int.multidim("random", lower=c(1), upper=c(20),dim=1),
               "f should be a function!")
  expect_error(sg.int.multidim(example1, lower=c(1), upper=c(20), dim="a"),
               "dim should be numeric!")
  expect_error(sg.int.multidim(example1, lower=c(20), upper=c(1),dim=1),
               "lower must be smaller than upper!")
  expect_error(sg.int.multidim(example1, lower=c(1,0), upper=c(20,10),dim=1),
               "dimension mismatches!")
})


#Testing correct outputs
test_that("Correct Outcome", {
  f<-function(x) x^3
  int <- as.vector(sg.int.multidim(f, lower=c(1), upper=c(20), dim=1))
  expect_equal(int, 39999.75, tolerance=.01)
})

#Not good
test_that("Correct Outcome", {
  f<-function(x) x^3
  int <- as.vector(sg.int.multidim(f, lower=c(1), upper=c(20), dim=1))
  expect_equal(int, 49999.75, tolerance=.01)
})


###########
#Exercise 4
###########


# Measuring gains in speed when running in parallel
library(microbenchmark)

f <- function(x) x^2

#Maybe it's a problem with my computer, but it looks like it doesn't like to do it in parallel
  #Case with two dimensions
  microbenchmark("no parallel"=sg.int.multidim(f,lower=l,upper=u,dim=2), 
                 "parallel"=sg.int.multidim(f,lower=l,upper=u, dim=2, parallel=T),
               times=5)
  #Case with three dimensions
  microbenchmark(sg.int.multidim(f,lower=l,upper=u, dim=3), 
               sg.int.multidim(f,lower=l,upper=u, dim=3, parallel=T),
               times=5)
#I shouldn't blame the computer. It's quite likely my fault.



###########
#Exercise 5
###########

library(cubature); library(mvtnorm)

dimensions<-2
example1 <- function(x){
    dmvnorm(x, mean=rep(0, dimensions), sigma=diag(rep(1, dimensions)))
  }

#The correct answer for the integral
ans <- as.numeric(pmvnorm(upper=rep(1, 2), mean=rep(0, 2), sigma=diag(rep(1, 2))))

##### COMPARING ACCURACY

#With adaptIntegrate the result is very close to the truth
adaptIntegrate(example1, lowerLimit=rep(-100, 2), upperLimit=rep(1, 2))$integral - ans
#With my function it is not so close, but not bad either
sg.int.multidim(example1, lower=rep(-1, 2), upper=rep(1, 2), dim=2) - ans


### COMPARTING SPEED
microbenchmark("adapt"=adaptIntegrate(example1, lowerLimit=rep(-100, 2), upperLimit=rep(1, 2))$integral,
               "sparse"=sg.int.multidim(example1, lower=rep(-1, 2), upper=rep(1, 2), dim=2),
               times=10)
microbenchmark("adapt"=adaptIntegrate(example1, lowerLimit=rep(-100, 2), upperLimit=rep(1, 2))$integral,
               "sparse"=sg.int.multidim(example1, lower=rep(-1, 2), upper=rep(1, 2)),
               times=10)


###########
#Exercise 6
###########

integrateMC <- function(fun, lower, upper, n, dim, parallel=F){
  # randomly selecting coordinates to integrate over
  random.points <- matrix(runif(n=(n*dim), min=lower, max=upper), ncol=dim)
  # Applying the function to the different points
  x.values <- aaply(random.points, 1, fun, .parallel=parallel)
  # "Integrating" by taking count of number of points under curve (mean), multiplying by length of integral
  # and taking it to the power of how many dimensions we have
  return(mean(x.values)*(upper-lower)^dim)
}

# Example
integrateMC(example1, lower=c(-2,5), upper=c(2,6), n=1000, dim=2)
