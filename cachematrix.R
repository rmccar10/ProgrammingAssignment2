## This function will invert a matrix if one is present, and if not it will create a matrix and then invert it

## The first function creates a special vector if one doesn't already exist

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function () x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function inverts the vector above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}