## These functions cache a matrix and then allow the cache to be returned,
## saving calculation time.  If no cache present, the inverse is generated
## for future caching

## makeCacheMatrix the cache of the matrix and stores it into the inv variable
## where it is accessible by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve looks for the inverse of a given matrix, and if found
## retrieves it.  If not, it creates it and stores it for future caching

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("The inverse has been found and retrieved")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
