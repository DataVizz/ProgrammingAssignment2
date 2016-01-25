## Put comments here that give an overall description of what your
## functions do

## The makecachematrix function does the following steps:
# 1. It creates a user defined matrix x
# 2. Initializes the inv and set variables and caches the inv variable to Null
# 3. Initializes the get, setinverse and getinverse variables 
# 4. Creates a list to store the set, get, setinverse and getinverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cache solve function helps in reading the cache memory and retreiving the variables stored when
# running the makecachematrix function
# 1. Initializes the inv variable by getting the inverse of the matrix x
#2. Checks if inverse is not null before getting is cached data and returns the inverse
#3. Fills the mat variable by getting matrix x from the cache
#4. Fills in the inverse variable by running the solve function
#5. Lastly, sets the inverse of the cached matrix x and prints it out to the console.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  
}
