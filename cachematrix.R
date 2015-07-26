## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is able to get and set a matrix into a cache (the list), as well as to get and set 
## the respective inverses. It does however NOT calculate the inverse, but simply caches them.

makeCacheMatrix <- function (x=matrix()) {
  
  
  
  inv <- NULL
  set <-function(y)  { 
    
                       x<<- y
                       inv <<- NULL
                     }   
  
  
  get <-function() x
  
  setinverse <- function (inverse) inv <<-inverse
  getinverse <- function () inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
  
}


## CacheSolve checks if the inverse of a value exists in the cache. If it does, it returns said inverse. 
## Otherwise, it calculates a new inverse, which makeCacheMatrix then can add to the cache.

cacheSolve <- function(x, ...) {
  
  
  inv<- x$getinverse()
  
  if(!is.null(inv)) {
                      message("getting cached data")
                      return(inv)
  }
  
  data <- x$get()
  
  inv <- solve(data, ...)
  
  x$setinverse(inv)
  
  
  inv
  
  
  
}
