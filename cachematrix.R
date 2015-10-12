## The functions below allow one to create a matrix that
## can cache its inverse and thereby compute it only once
## while returning it as many times as needed.

## This function creates the caching mechanism for the 
## matrix x, which can either be passed in or set via 
## the set() function.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list( set = set, 
            get = get,
            setinv = setinv, 
            getinv = getinv)
}


## This function returns the inverse of the matrix x,
## first checking if it has been computed and cached,
## an only computing it if it has not yet been done.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
