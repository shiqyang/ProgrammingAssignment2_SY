## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL 
  setMatrix <- function(y) { #set the value of the matrix
    x <<- y
    invMatrix <<- NULL
  }
  getMatrix  <- function() x #Get the value of the matrix
  setInverse <- function(inverse) invMatrix <<- inverse #set the value of the invertible matrix
  getInverse <- function() invMatrix #get the value of the invertible matrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) { #if inverse matrix is not NULL
    message("getting cached invertible matrix") #return message "getting cached invertible matrix"
    return(invMatrix) #return the invertible matrix
  }#if value of the invertible matrix is NULL then
  MatrixData <- x$getMatrix()  #get the original Matrix
  invMatrix <- solve(MatrixData, ...)#use solve function to inverse the matrix
  x$setInverse(invMatrix) #set the invertible matrix
  return(invMatrix)#return the invertible matrix
}