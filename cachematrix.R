##The function "makeCacheMatrix" creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) a <<- inverse
  getinverse <- function() a
  list (set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## The cacheSolve function computes for the inverse of the matrix returned by the function makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  a <- x$getinverse()
  if (!is.null(a)){
    message("getting cached data")
    return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$setinverse(a)
  a
}
