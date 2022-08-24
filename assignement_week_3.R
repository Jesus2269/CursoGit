
#-----Cache Matrix-----

makecachematrix <- function(x = matrix())   {
  invers <- NULL
  set <- function(y){
    x <<-y
    invers <<- NULL
  }
  get <- function() {x}
  setInvers <- function(calculatedinvers) {
    invers <<- calculatedinvers
    getInvers <- function() {invers}
    list(set=set, get=get,
         setInvers=setInvers,
         getInvers=getInvers)
  }
}


#-----Cache solve

cachesolve <- function(x,...) {
  invers <- x$getInvers()
  if(!is.null(invers)) {
    message("getting cached data")
    return(invers)
  }
  data <- x$get()
  invers <- solve(data, ...)
  x$setInvers(invers)
  invers
}

