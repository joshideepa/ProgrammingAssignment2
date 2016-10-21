## Cache the inverse of a matrix and if the inverse has already been calculated 
## then the cachesolve should retrieve the inverse from the cache

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x	
  setInverse <- function(solve) inv <<- solve	
  getInverse <- function() inv	
  list(set=set, 
       get=get, 
       setInverse = setInverse, 
       getInverse = getInverse)

}


## This function gives the cached inverse of a matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
