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