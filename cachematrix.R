## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is able to get and set a matrix into a cache (the list), as well as to get and set 
## the respective inverses. It does however NOT calculate the inverse, but simply caches them.

makeCacheMatrix <- function (x=matrix()) {
  
  
  
  inv <- NULL
#sets a matrix in the cache  
  set <-function(y)  { 
    
                       x<<- y       
                       inv <<- NULL
                     }   
  
#gets a matrix from the cache  
  get <-function() x
  
#gets & sets the inverse for use in cacheSolve 
  setinverse <- function (inverse) inv <<-inverse
  getinverse <- function () inv
 

#the list is used to cache the matrixes   
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
  
}


## CacheSolve checks if the inverse of a value exists in the cache. If it does, it returns said inverse. 
## Otherwise, it calculates a new inverse, which makeCacheMatrix then can add to the cache.

cacheSolve <- function(x, ...) {
  
#attempts to get the inverse from the cache
  inv<- x$getinverse()
  
#checks if the inverse has already been calculated  
  if(!is.null(inv)) {
                      message("getting cached data")
                      return(inv)
                    }
  
  
#gets matrix for the inverse calculation
  data <- x$get()
  
#calculates the inverse if it has not been cached and returns it for use in makeCacheMatrix    
  inv <- solve(data, ...)

#returns the data to the cache  
  x$setinverse(inv)
  
#displays the inverted matrix  
  inv
  
  
  
}
