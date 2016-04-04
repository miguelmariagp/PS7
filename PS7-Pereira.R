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
    #registerDoSNOW(makeCluster(2, type = "SOCK"))
  }

  lower<-floor(lower)
  upper<-ceiling(upper)
  
  if (any(lower>upper)) stop("lower must be smaller than upper")
  
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
  
  gx.sp <- aaply(.data=nodes,
                 .margins = 1,
                 .parallel = parallel,
                 .fun = f)
  val.sp <- t(gx.sp) %*%weights
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
sg.int.multidim(f,lower=l,upper=u,parallel=TRUE)


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

#####TO DO

library(testthat)
test_that("Correct inputs",{
  fun<-function(x) x^2
  expect_that(typeof())
  
  
})
typeof(fun)


test_that("Basic output test", {
  ll<-c(1,2,3)
  uu<-c(4,6,8)
  f<-function(x) x^2
  int <- sg.int.multidim(f,lower=ll,upper=uu)
  
  expect_that( int, is_a("matrix") )
  expect_that(length(int), equals(length(ll)))
})



test_that("Output test", {
  ll<-c(1,2,3)
  uu<-c(4,6,8)
  f<-function(x) x^2
  int <- sg.int.multidim(f,lower=ll,upper=uu)
  
  expect_that( int, is_a("matrix") )
  expect_that(length(int), equals(length(ll)))
})



###########
#Exercise 4
###########


# Measuring gains in speed when running in parallel
library(microbenchmark)

#Something is wrong with my parallel
microbenchmark(sg.int.multidim(f,lower=l,upper=u), 
               sg.int.multidim(f,lower=l,upper=u, parallel=T),
               times=5)
microbenchmark(sg.int.multidim(f,lower=ll,upper=uu), 
               sg.int.multidim(f,lower=ll,upper=uu, parallel=T),
               times=5)




###########
#Exercise 5
###########

library(cubature)
library(mvtnorm)


example1 <- function(x){
  dmvnorm(x, mean=rep(0, 2), sigma=diag(rep(1, 2)))
}

#The correct answer for the integral
ans <- as.numeric(pmvnorm(upper=rep(1, 2), mean=rep(0, 2), sigma=diag(rep(1, 2))))

#With adaptIntegrate the result is very close to the truth
adaptIntegrate(example1, lowerLimit=rep(-100, 2), upperLimit=rep(1, 2))$integral - ans
#With my function it is not so close:
sg.int.multidim(example1, lower=rep(-1, 2), upper=rep(1, 2)) - ans



###########
#Exercise 6
###########

mc.int<-function(f,lower,upper, parallel=FALSE){ 
  require("SparseGrid")
  require("plyr")
  require("mvtnorm")
  #The necessary instructions for the parallel option to work
  if (parallel==TRUE){
    require("doParallel")
    registerDoParallel(cores=4)
  }
  
  lower<-floor(lower)
  upper<-ceiling(upper)
  
  if (any(lower>upper)) stop("lower must be smaller than upper")
  
  #Number of dimensions derived from the length of the vectors introduced
  dim<-length(lower)
  
  #This gives us all possible combinations of the supplied vectors
  gridss<-as.matrix(expand.grid(apply(cbind(lower,upper),
                                      1,function(x) seq(x[1],x[2]-1,by=1))))
  
  #Note: dimension = number of dimensions wanted
  sp.grid <- createMonteCarloGrid(rnorm, dimension=2, num.sim=100 )
  nodes<-gridss[1,]+sp.grid$nodes
  weights<-sp.grid$weights
  
  for (i in 2:nrow(gridss)){
    nodes<-rbind(nodes,gridss[i,]+sp.grid$nodes)  
    weights<-c(weights,sp.grid$weights)
  }
  
  gx.sp <- aaply(.data=nodes,
                 .margins = 1,
                 .parallel = parallel,
                 .fun = f)
  val.sp <- t(gx.sp) %*%weights
  return(val.sp)
  
}


sg.int.multidim(example1, lower=rep(-1, 2), upper=rep(1, 2))
mc.int(example1, lower=rep(-1, 2), upper=rep(1, 2))
