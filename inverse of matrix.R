# makeCacheMatrix: 
# This function creates a special "matrix" 
# object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  #the value of inv is NULL
  inv <- NULL 
  #when set(y), then x=y, inv=NULL
  set <- function(y) {
    x <<- y 
    inv <<- NULL
  }
  #Assign x to get
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  #copy inv to getInverse(NULL)
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#cacheSolve: This function computes the inverse of the special matrix 
# returned by makeCacheMatrix above. 
# If the inverse has already been calculated 
# (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x',
  #First assign getInverse to inv. 
  # If inv is not NULL, the following prompt will pop up
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #Assign the initial numerical vector to mat
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv) #Cache m
  inv
}

# test my code
x <-makeCacheMatrix(matrix(c(1,0,0,0,1,0,0,0,2),ncol=3,nrow=3))
cacheSolve(x)
x$get()
x$getInverse()
