## The following two functions makeCacheMatrix() and cacheSolve()
## are facilitating caching the inverse value of a matrix
## providing getinverse and setinverse properties.

## finction - makeCacheMatrix() 
## Input arg 'x' is of type matrix
## provides a container for caching the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL
         set <- function(y) {
           x <<- y
           inv <<- NULL
         }
         get <- function() x
         setiverse <- function(solve) inv <<- solve
         getinverse <- function() inv
         list(set = set, get = get,
              setiverse = setiverse,
              getinverse = getinverse)
  
}


## function name - cacheSolve() 
## Input arg 'x' is of type matrix
## retrieves an existing inverse of a matrix or
##calculates and stores it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setiverse(inv)
        inv
}
