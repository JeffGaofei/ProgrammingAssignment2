##Setting up a list of 3 functions: get(data) setInv(erse Matrix) GetInv(erse Matrix)
##Since X is a pre-defined x just as the numeric vector in the demo, the SET function is omitted

makeCacheMatrix <- function(x=matrix()){
  i <- NULL ##declare i to store the inversed matrix
  get <- function() x ## get matrix x to be inversed
  setInv <- function(y) i <<- y ##assign a value (the inversed matrix) to i
  getInv <- function() i ##return the inversed matrix i
  list(get=get,setInv=setInv,getInv=getInv)
}

cacheSolve <- function(x, ...) {
  i <- x$getInv() ##read the cached data
  if(!is.null(i)){ ##check whether there's data cached or not
    message("getting cached data")
    return(i) ##return cached data
  }
  ##If there's no data cached
  data <- x$get() ## get matrix x to be inversed
  i <- solve(data) ##solve the inverse and assign it to internal variable i
  x$setInv(i) ##assign internal i to global i and store the data
  i ##return global i
}
