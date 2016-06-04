##
## Youngjae Seo
## 
## With the function'makeCacheMatrix' and 'cacheSolve',
## users can create a special "Matrix" which is able to cache its inverse of the matrix.
## Only at the first execution of 'cacheSolve' it calculates the inverse of the matrix.
## At the next execution, it will simply return the value from the cache.
##
## * Samples
## > cm <- makeCacheMatrix(matrix(c(1.1, 2.2, 3.3, 4.4), nrow = 2, ncol = 2))
## > cacheSolve(cm)
## [,1]       [,2]
## [1,] -1.8181818  1.3636364
## [2,]  0.9090909 -0.4545455
## > cacheSolve(cm)
## getting cached data
## [,1]       [,2]
## [1,] -1.8181818  1.3636364
## [2,]  0.9090909 -0.4545455


###############################################
# Function Name: makeCacheMatrix
# Description: Create a special "Matrix" caching the inverse of the matrix
# Arguments: 
#   1. Matrix (Default: 1x1 NA matrix)
# Return: A special "Matrix", which is really a list containing a function to
#   1. set the value of the matrix
#   2. get the value of the matrix
#   3. set the value of the inverse of the matrix
#   4. get the value of the inverse of the matrix
###############################################
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

###############################################
# Function Name: cacheSolve
# Description: Calculates the inverse of the special "matrix" created with the function 'makeCacheMatrix'
#              If there is the inverse of the matrix in cache, this function simply returns it.
#              IF NOT, calculte the inverse of the matrix and store it in cache, and then returns it.
# Arguments: 
#   1. a special "matrix" created with the function 'makeCacheMatrix'
#   ... further arguments passed to the function 'solve'.
# Return: The inverse of the matrix
###############################################
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
