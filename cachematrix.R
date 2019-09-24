## Caching the Inverse of a Matrix
## rather than computing it repeatedly

## The function creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    inv <<- NULL
    
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}

## This function creates the inverse of the matrix created by makeCacheMatrix()
## If the inverse is already present in the cache then it will simply pull data from cache without computing

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
      message("getting cached data")
      return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}