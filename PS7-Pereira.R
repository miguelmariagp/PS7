#Problem Set 
#Miguel Pereira

#SparseGrid function


#It's code
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

sg.int.multidim<-function(f,lower,upper, parallel=FALSE){ 
  require("SparseGrid")
  require("plyr")
  
  #The necessary instructions for the parallel option to work
  if (parallel==TRUE){
    require("doParallel")
    registerDoParallel(cores=4)
  }
  
  lower<-floor(lower)
  upper<-ceiling(upper)
  
  if (any(lower>upper)) stop("Lower must be smaller than upper")
  if (length(lower)!=length(upper)) stop("Lower and Upper vectors have to be of the same length")
  
  #Number of dimensions derived from the length of the vectors introduced
  dim<-length(lower)
  
  #This gives us all possible combinations of the supplied vectors
  gridss<-as.matrix(expand.grid(apply(cbind(lower,upper),
                                      1,function(x) seq(x[1],x[2]-1,by=1))))
  
  #Note: dimension = number of dimensions wanted
  sp.grid <- createIntegrationGrid( 'KPU', dimension=dim, k=5 )
  
  nodes<-gridss[1,]+sp.grid$nodes
  
  weights<-sp.grid$weights
  
  for (i in 2:nrow(gridss)){
    nodes<-rbind(nodes,gridss[i,]+sp.grid$nodes)  
    weights<-c(weights,sp.grid$weights)
  }
  
  gx.sp <- apply(nodes, 1, f)
  
  val.sp <- gx.sp %*%weights
  return(val.sp)
  
}


###TESTING THE FUNCTION

#Two dimension
l<-c(1,2)
u<-c(4,6)
f<-function(x) x^2
#Without parallel
  #The original function
  sg.int(f,lower=l,upper=u)
  #My function allowing different dimensions and parallel
  sg.int.multidim(f,lower=l,upper=u)

  #With parallel
  sg.int.multidim(f,lower=l,upper=u, parallel=TRUE)
  


#Three dimensions
ll<-c(1,2,3)
uu<-c(4,6,8)
#Without parallel
sg.int.multidim(f,lower=ll,upper=uu)

#With parallel
sg.int.multidim(f,lower=ll,upper=uu,parallel=TRUE)

#Four dimensions
lll<-sort(rnorm(4,1,4))
uuu<-sort(rnorm(4,6,2))
sg.int.multidim(f,lower=lll,upper=uuu)




###########
#Exercise 2
###########

#Already above


###########
#Exercise 3
###########
library(testthat)
library(mvtnorm)

ll<-c(1,2,3)
uu<-c(4,6,8)
f<-function(x) x^2

test_that("Basic output test", {
  int <- sg.int.multidim(f,lower=ll,upper=uu)
  expect_that( int, is_a("matrix") )
  expect_that(length(int), equals(length(ll)))
})

(x1<-as.numeric(pmvnorm(upper=rep(.5, 2), mean=rep(0, 2), sigma=diag(rep(1, 2)))))
(x2<-as.numeric(sg.int.multidim(example1, lower=rep(-1, 2), upper=rep(1, 2))))

#Fine here
test_that('Function within 0.05 of true answer',{
  expect_equal(x1,x2,tolerance=.05)
}
)

#Not fine anymore
test_that('Function within 0.01 of true answer',{
  expect_equal(x1,x2,tolerance=.01)
}
)


###########
#Exercise 4
###########


# Measuring gains in speed when running in parallel
library(microbenchmark)

#Maybe it's a problem with my computer, but it looks like it doesn't like to do it in parallel
  #Case with two dimensions
  microbenchmark(sg.int.multidim(f,lower=l,upper=u), 
               sg.int.multidim(f,lower=l,upper=u, parallel=T),
               times=5)
  #Case with three dimensions
  microbenchmark(sg.int.multidim(f,lower=ll,upper=uu), 
               sg.int.multidim(f,lower=ll,upper=uu, parallel=T),
               times=5)
#I shouldn't blame the computer. It's quite likely my fault.



###########
#Exercise 5
###########

library(cubature)


example1 <- function(x){
  dmvnorm(x, mean=rep(0, 2), sigma=diag(rep(1, 2)))
}

#The correct answer for the integral
ans <- as.numeric(pmvnorm(upper=rep(1, 2), mean=rep(0, 2), sigma=diag(rep(1, 2))))

##### COMPARING ACCURACY

#With adaptIntegrate the result is very close to the truth
adaptIntegrate(example1, lowerLimit=rep(-100, 2), upperLimit=rep(1, 2))$integral - ans
#With my function it is not so close, but not bad either
sg.int.multidim(example1, lower=rep(-1, 2), upper=rep(1, 2)) - ans


### COMPARTING SPEED
microbenchmark("adapt"=adaptIntegrate(example1, lowerLimit=rep(-100, 2), upperLimit=rep(1, 2))$integral,
               "sparse"=sg.int.multidim(example1, lower=rep(-1, 2), upper=rep(1, 2)),
               times=10)
microbenchmark("adapt"=adaptIntegrate(example1, lowerLimit=rep(-100, 2), upperLimit=rep(1, 2))$integral,
               "sparse"=sg.int.multidim(example1, lower=rep(-1, 2), upper=rep(1, 2)),
               times=10)



###########
#Exercise 6
###########

integrateMC <- function(func, lower, upper, n){
  dim<-length(lower)
  # randomly selecting coordinates to integrate over
  random.points <- matrix(runif(n=(n*dim), min=lower, max=upper), ncol=dim)
  # Applying the function to the different points
  x.values <- apply(random.points, 1, func)
  # "Integrating" by taking count of number of points under curve (mean), multiplying by length of integral
  # and taking it to the power of how many dimensions we have
  return(mean(x.values)*(upper-lower)^dim)
}

# Example
integrateMC(example1, lower=c(-2,5), upper=c(2,6), n=1000)
