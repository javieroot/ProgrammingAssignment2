# The cacheSolve function checks if the matrix that receives as a parameter have 
# computed his inverse, if not, makes his calculation. To accomplish this uses a
# special "matrix", which is really a list of functions for storage executed on  
# the parameter required, where the spacial "matrix" is created with 
# makeCacheMatrix function.


# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # Empty variable
  inv <- NULL
  # Function that change y argument to global variable and inv
  set <- function(y){
    x   <<- y
    inv <<- NULL
  }
  # A identity function: matrix x is returned from matrix x
  get    <- function() x
  # Creating a function for computing the inverse matrix from solve function
  setinv <- function(solve) inv <<- solve
  # Computing inverse matrix x
  getinv <- function() inv
  
  # Return a list with previous calculations
  list(set    = set,
       get    = get,
       setinv = setinv,
       getinv = getinv)

}


# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then the cachesolve should retrieve the inverse from
# the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # if exist the inverse matrix its value is assigned to inv variable
  inv <- x$getinv()
  # If exist the inverse matrix, is returned and print in screen a message that
  # exist, if not then continuing
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Assigning the matrix x to data variable
  data <- x$get()
  # Computing inverse matrix
  inv  <- solve(data, ...)
  # Updating global variable x with the inverse matrix
  x$setinv(inv)
  # Return inverse matrix
  inv
  
}
