## functions overview:

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


## create a matrix to cache inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL

  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  getinv <- function() inv
  setinv <- function(inverse) inv <<- inverse

  list(get=get, set=set, getinv=getinv, setinv=setinv)
}


## returns inverse of a given matrix

cacheSolve <- function(x, ...) {
  inv <- x$getinv()

  if (!is.null(inv)) {
    message("inverse is cached")
    return(inv)
  }

  m <- x$get()
  inv <- solve(m, ...)

  x$setinv(inv)

  return(inv)
}