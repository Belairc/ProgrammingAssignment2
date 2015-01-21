
## makeCacheMatrix is supposed to create a special matrix object 
## user inputs the parameters of a matrix
## use solve() function to compute the inverse of a square matrix

makeCacheMatrix <- function(x = matrix()) {
  ## matrix creates a matrix from the given set of values
  ## <<- used in function searches environment for existing definition
  ## m keeps track of whether calculation has been performed NULL = no
  ## every time makeCacheMatrix is started m is reset to NULL

    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    get <- function() x
    setsolve <- function(solve) m <<- solve ##solve computes inverse
    getsolve <- function() m  
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
    
  }


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
## however if the inverse has already been calculated, cachSolve should 
## retrieve the inverse from the cache.
## m keeps track of whether calculation has been performed for that matrix NULL = no

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## use solve() function to compute the inverse of a square matrix
  
    m <- x$getsolve()
    ##message("m is ", m) ## test m value
    if(!is.null(m)) {   ## checks if m has value other than NULL
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    message("calculating m... ")
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
