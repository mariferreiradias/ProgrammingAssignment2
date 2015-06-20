## This pair of functions calculates the inverse of a matrix (assuming that x is
# invertible) caching the inverse.

# The function MakeCacheMatrix creates an object in a list with four functions
# that can cache the inverse of x.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x #return the value of x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The function cacheSolve use the objects from makeCacheMatrix, first verifing
# if the value inv exists and isn't null, and if it is false calculates the
# inverse with the vector stored in makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
