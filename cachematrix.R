## Put comments here that give an overall description of what your
## functions do

## Creates functions used as the input for the cacheSolve() function
## 1.  Initialize the matrix
## 2.  Retrieve the matrix
## 3.  Set the inverse
## 4.  Get the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setmatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)

}


## Uses the output from the makeCacheMatrix as its input for x
## returns the inverse of the matrix input of the makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrixinverse <- x$getinverse()
  ##Checks to see if the inverse has already been calculated and then will get it from the cache
  if(!is.null(matrixinverse)) {
    message("getting cached data")
    return(matrixinverse)
  }
  ##Otherwise it does the calculation of the matrix
  data <- x$get()
  matrixinverse <- solve(data, ...)
  x$setinverse(matrixinverse)
 return( matrixinverse)
}


