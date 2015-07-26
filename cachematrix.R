## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##this function will create some variables and then return the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {

  local_m <- NULL                                          
  set <- function(y) {                                   
    cache_x <<- y                                      
    cache_m <<- NULL                                           
  }
  get <- function() cache_x                              
  set_cache_m <- function(local_m) cache_m <<- local_m    ## Create function to set the value of cache_m in cache to the value of local_m passed in the call to '$set_cache_m.        
  get_cache_m <- function() cache_m                       ## Create function to retrieve value of cache_m from cache and return cache_m to the caller so we can check it for NULL
  list(set = set, get = get,
       set_cache_m = set_cache_m,
       get_cache_m = get_cache_m)
  
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  local_m<- x$get_cache_m()               ## Get the value for m in the cache environment and put it in a local m.
  if(!is.null(local_m)) {                 ## Check to see if m is NULL.  
    message("getting cached data")         ## If m is not NULL, return the value of m with a message.
    return(local_m)
  }                                      
  startingmatrix <- x$get()               
  endingmatrix <- solve(startingmatrix)  
  x$set_cache_m(endingmatrix)           
  endingmatrix   
}
