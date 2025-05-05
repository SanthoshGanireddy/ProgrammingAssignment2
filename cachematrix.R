## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}
cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

# Function to create a special matrix object that caches its inverse 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset cached inverse when matrix changes
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse
  getInverse <- function() inv
  
  # Return list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute the inverse of the special matrix
cacheSolve <- function(x, ...) {
  # Try to get cached inverse
  inv <- x$getInverse()
  
  # If inverse is already calculated, return it with a message
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse
  x$setInverse(inv)
  
  # Return the inverse
  inv
}



