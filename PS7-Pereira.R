#Problem Set 
#Miguel Pereira

#SparseGrid function
#Figuring what it does
l<-c(1,2)
u<-c(4,6)
as.matrix(expand.grid(seq(l[1],u[1]-1,by=1),
                      seq(l[2],u[2]-1,by=1)))
t<-createIntegrationGrid( 'KPU', dimension=2, k=5 )


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


ll
uu

cbind(ll,uu)

gg<-as.matrix(expand.grid(apply(cbind(ll,uu),
                            1,function(x) seq(x[1],x[2]-1,by=1))))

gg[1,]
as.matrix(expand.grid(seq(ll[1],uu[1]-1,by=1),
                      seq(ll[2],uu[2]-1,by=1)))


###########
#Exercise 1
###########


sg.int.multidim<-function(f,lower,upper, parallel=FALSE){ 
  require("SparseGrid")
  require("plyr")
  if (parallel=TRUE){
    require("doSNOW")
    registerDoSNOW(makeCluster(2, type = "SOCK"))
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
#Three dimensions
ll<-c(1,2,3)
uu<-c(4,6,8)
f<-function(x) x^2
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

Already above


###########
#Exercise 3
###########


library(testthat)
test_that("Correct inputs",{
  fun<-function(x) x^2
  expect_that(typeof())
  
  
})
typeof(fun)


library(cubature)

example1 <- function(x){
  return(cos(x[1]+x[2]+x[3]))
}

adaptIntegrate(example1,c(1,2,3),c(5,6,7))

sg.int.multidim(example1,c(1,2,3),c(5,6,7))


###########
#Exercise 4
###########


# Measuring gains in speed when running in parallel
library(microbenchmark)

microbenchmark(sg.int.multidim(f,lower=ll,upper=uu), 
               sg.int.multidim(f,lower=ll,upper=uu, parallel=T),
               times=10)
