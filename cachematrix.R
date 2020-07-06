## Here we define a 'CacheMatrix' object

## The data for the matrix, and its inverse,
## will live in the environment of the function
## and not in the main environment where bad
## things might happen to it

makeCacheMatrix <- function(x = matrix()) {
  
  # By default, x is the matrix provided upon creation
  # and inv is the inverse, which has not been computed
  
  inv <- NULL
  
  # Provides a method for resetting the internal state,
  # which allows the data in the matrix to be updated
  
  set <- function(newx) {
    x <<- newx
    inv <<- NULL
  }
  
  # Provides a method for accessing the matrix data
  
  get <- function() x
  
  # Provides a method for storing the computed inverse
  
  setinv <- function(newinv) inv <<- newinv
  
  # Provides a method for accessing the cached inverse
  
  getinv <- function() inv
  
  # The actual data structure for the object
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## This calculates the matrix inverse in the ordinary way, then
## stores the result within the environment of the CacheMatrix

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
