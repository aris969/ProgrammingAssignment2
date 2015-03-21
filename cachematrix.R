## These two functions will take a supplied inversable matrix and 
## calculate the inverse then store it in memory. 
## This will allow functions that require the inverse of the matrix
## to refer to the cached version rather than calculate the inverse at run time. 
## This is especially useful of the function requiring the inverse is within a loop.


## makeCacheMatrix takes in a matrix and builds a
## set of functions that will cache and 
## retrieve the matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  cacheinv <- NULL
  set <- function(y) { 
    ## '<<-' will assign the value to an object in different 
    ## environment than the present
    x <<- y ## Stores the matrix
    cacheinv <<- NULL
  }
get <- function() x ## Retrieves the matrix 
setinv <- function(inverse) cacheinv <<- inverse ## Stores the matrix inverse
getinv <- function() cacheinv ## Retrieves the matrix inverse
list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)
}


## cacheSolve will take the output from makeCacheMatrix, 'x', and 
## return the inverse of the matrix supplied to makeCacheMatrix.  

cacheSolve <- function(x, ...) {
  m <- x$getinv() ## get the cached inverse
  ## If there is a cached inverse then return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## If there is no cached inverse returned from getinv() then 
  ## calculate the inverse from the original matrix
  data <- x$get() ## get the original matrix 
  m <- solve(data) ## calculate the inverse
  x$setinv(m) ## cache the inverse
  m ##return the inverse
}
