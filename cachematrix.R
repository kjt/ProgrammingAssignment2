## The following functions wrap the R matrix class, allowing the inverse of the
## matrix to be cached and repeatedly referenced without having to recalculate it.
##
## The first function, makeCacheMatrix, provides the data cache for a matrix and
## its inverse, as well as some basic accessor functions. The second function,
## cacheSolve, has two behaviors. The first time it's called, it gets the matrix
## from the cache, calculates the inverse and stores the inverse back in the cache.
## The second and Nth time cacheSolve is called with the same cache object, it retrieves
## the previously-computed inverse from the cache object. This prevents the inverse
## from having to be recalculated on subsequent calls.

## Note that this is done in classic R style rather than the encapsulation style found in
## other object-oriented languages. It would have been just as easy for makeCacheMatrix
## to calculate the inverse itself. FWIW, I think that would have been a cleaner
## abstraction, considering that the cache object already has its own member functions.

## Example:
## m <- matrix(rnorm(1000000), nrow=1000)
## m_cached<-makeCacheMatrix(m)
## cacheSolve(m_cached)
## cacheSolve(m_cached)

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## sanity checks, even though we can assume an invertible matrix
  if (class(x) != "matrix") {
    stop ("Argument must be a square matrix")
  }
  d<-dim(x)
  if (d[1] != d[2]) {
    stop ("Matrix must be square")
  }
    
  inv <- NULL
  
  ## create and return the accessor functions
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
