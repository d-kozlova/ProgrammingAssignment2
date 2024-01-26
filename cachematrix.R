## The makeCacheMatrix function stores the cached value of the input and inverse matrix
## The cacheSolve function gets the cached value of the input matrix and calculates its inverse


## The makeCacheMatrix function stores a list of 4 functions: set, get, setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    ## Set a new value (y) for the input matrix, its inverse(i) is then set to NULL
    set <- function(y){
      x <<- y
      i <<- NULL
    }
    
    ## return the value of the input matrix
    get <- function() x
    ## setinverse does not calculate the inverse matrix, but instead just stores its value that we can change manually
    setinverse <- function(inverse) i <<- inverse
    ## the getinverse function returns the value of the inverse matrix
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

## cacheSolve returns the cached inverse matrix if it exists
## If there's no cached value for the inverse matrix, the inverse is calculated and stored using the setinverse function from makeCacheMatrix

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message ("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
