#The following functions calculate the inverse of a matrix and cache it.
#You can then access the cached matrix to save computation time later

# This function:
# Caches the matrix given in the parameter x
# Caches the inverse of x for fast lookup

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv
  list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}

# This function:
# Computes the inverse of x and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}
