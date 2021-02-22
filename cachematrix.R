## Here we present two functions to improve the algorithm of matrix inversion. 
##It can store the inverse matrices that were already computed in the past, 
##making the process faster than just inverting matrices all over again. 

## This function (makeCacheMatrix) creates a list containing a function that can
## set the value of our matrix, get the value of this matrix, set the inverse of
## the matrix and lastly, get the value of the inverse matrix.


makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  

}


## This function (cacheSolve) calculates the inverse of the matrix created
## previously. Before it calculates, this function checks for past inverted 
## matrices and If they already exist, it gets the matrix from the cache and 
## skips the computation. 

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
