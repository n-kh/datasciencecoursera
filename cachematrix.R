## Considering that a usual statistics job requires working with big data 
##idea is to store certain values in some other environment than the function 
##environment so that it can be fetched whenever needed and avoid big 
##calculations multiple number of times. 

##Here in this function we are 
##creating a special matrix that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## in this function we are checking if the inverse already exist in the cache or not
##If it exists we retrieve otherwise calculate the inverse and store in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
