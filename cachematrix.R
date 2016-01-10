## Functions to provide a cached version of solve for inverse matrix
##
## These two functions allow the user to generate a custom matrix object that 
## provides a caching mechanism for the inverted result.
##
## Example:
##
## Create a cacheable object
## matrix <- matrix(1:4,2,2)
##
## The first call will calculate the inverse, cache the result and return
## inverse1 = cacheSolve(matrix)
##
## The second call should be significantly faster, as the function should
## immediately return the cached value
## inverse2 = cacheSolve(matrix)

## Creates a object to hold the matrix and it's cached inverse
makeCacheMatrix <- function(matrix = matrix()) {
      inverseMatrix <- NULL
      set <- function(newMatrix) {
            matrix <<- newMatrix
            inverseMatrix <<- NULL
      }
      get <- function() matrix
      setinverse <- function(inverse) inverseMatrix <<- inverse
      getinverse <- function() inverseMatrix
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)

      }

## Returns the inverse of the passed matrix variable. If a cached version
## of the inverse already exists, this is returned immediately without 
## recalculating

cacheSolve <- function(matrix, ...) {
      inverseMatrix <- matrix$getinverse()
      if(!is.null(inverseMatrix)) {
            return(inverseMatrix)
      }
      data <- matrix$get()
      inverseMatrix <- solve(data)
      matrix$setinverse(inverseMatrix)
      inverseMatrix
}
