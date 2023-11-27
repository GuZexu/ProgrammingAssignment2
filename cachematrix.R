## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of
## a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will 
##not discuss here).My assignment is to write a pair of functions that cache the inverse of a matrix.

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  iom <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) iom <<- inverse
  getinverse <- function() iom
  list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


##This function computes the inverse of the special "matrix" returned Matrix above. If the inverse has already
##been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the
##cache.

cacheSolve <- function(x, ...) {
  iom <- x$getinverse()
  if(!is.null(iom)) {
    message("getting cached data")
    return(iom)
  }
  data <- x$get()
  iom <- solve(data, ...)
  x$setinverse(iom)
  iom
}
