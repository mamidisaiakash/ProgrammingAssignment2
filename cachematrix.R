## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## This function creates a Matrix object which is used to cache its inverse
## It can set or get matrix by calling the setinverse/getinverse methods

makeCacheMatrix <- function(x = matrix()) 
{
  inverse <- NULL
  # set is used to replace x by other matrix y
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  # Here the inverse of the matrix generated
  setinverse <- function(inv) inverse <<- inv
  # Function to get the inverse of the matrix
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  # inverse of matrix x is assigned to inv
  inv <- x$getinverse() 
  # check for existance of inverse, if available return it.
  if (!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  
  # It means cached matrix is not available, Inverse of the matrix is computed and cached to matrix inv
  # it can we used for the new requests.
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
