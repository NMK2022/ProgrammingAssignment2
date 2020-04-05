##This generic function, makeCacheMatrix, will allow for the caching of the 
##inverse of a matrix we set by assigning the set + setinverse and get + getinverse 
##Matrix. Thus, allowing for our second function, cacheSolve, to take the
##input (from first function makeCacheMatrix) and give us an output.

##Using the matrix & NULL statements we assigned in the first function, we can provide 
##an if statement in our second function to allow our output to be either a value or NULL 
##when we carry out our subsequent inverse matrix cache operations.


makeCacheMatrix <- function(x = matrix()){
  inverseM <- NULL
  setM <- function(y){
    x <<- y
    inverseM <<- NULL
  }
  getM <- function() x
  seti <- function(inverse) inverseM <<- inverse
  geti <- function() inverseM
  list(setM = setM, getM = getM, seti = seti, geti = geti)
}


cacheSolve <- function(x, ...){
  inverseM <- x$geti()
  if(!is.null(inverseM)){
    message("getting cached data")
    return(inverseM) 
  }
  data <- x$getM()
  inverseM <- solve(data, ...)
  x$seti(inverseM)
  inverseM
}