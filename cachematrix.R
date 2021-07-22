## Put comments here that give an overall description of what your
## functions do


## make matrix assign to variable x, and initialize inv to NULL

makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL 
    set <- function(y) {
      x <<- y 
      inv <<- NULL 
    } 
    get <- function() x 
    setInverse <- function(inverse) 
    inv <<- inverse 
    getInverse <- function() inv
    list(set = set, 
         get = get, 
         setinverse = setInverse,
         getinverse = getInverse)
}
                                                            

## Write a short comment describing this function


cacheSolve <- function(x, ...) { ## if user had calculated the same matrix before
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)                   ## return old result(m) directly 
  }
  matrix <- x$get()           ## otherwise, get the uncalculated matrix
  i <- solve(matrix, ...)     ## calculate the inverse matrix
  x$setinverse(i)             ## reassign inverse matrix
  i                           ## print the inverse matrix
}
