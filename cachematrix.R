## Contains a function for solving the inverse of a matrix and a function for caching it
## If the cache of the inverse exists, it will get the value in the cache

## creates a matrix object and stores the inverse of the matrix in cache

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Return a matrix that is the inverse of 'x'
## If the cache of the inverse exists, it will get the value in the cache

cacheSolve <- function(x, ...) {

  i <- x$getinverse()
  
  if(!is.null(i)){
    message("getting the cached inverse")
    return(i)
  }
  
  m <- x$get()
  i <- solve(m)
  x$setinverse(i)
  i

}
