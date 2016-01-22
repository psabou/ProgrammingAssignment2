## R Programming, Coursera, programming assignment 2

## The two functions in this file create a special matrix object for which the inverse
## can be stored in the cache. The function makeCacheMatrix creates the matrix object
## and stores the value of the inverse when computed. The function cacheSolve retrieves 
## the inverse in the cache if it has not been computed already; otherwise 
## it computes the inverse and stores it in the cache.

## The function makeCacheMatrix creates a special matrix object and returns a list 
## of four functions
## 1 - set() sets the value of the matrix 
## 2 - get() returns the value of the matrix
## 3 - setinv() stores the value of the inverse in the cache
## 4 - getinv() returns the value of the inverse. If inverse has not been computed, value is NULL.
## Matrix value can be set on function call as an argument. Default is an empty matrix.

makeCacheMatrix <- function(x = matrix()) {
      
     
            inv <- NULL
            
            set <- function(y) {
                  x <<- y
                  inv <<- NULL
            }
            
            get <- function() x
            
            setinv <- function(minverse) inv <<- minverse
            getinv <- function() inv
            
            list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)
      

}


## This function computes the inverse of a special matrix x. Before it does so,
## it looks to see if the inverse of the matrix has already been computed. If so, it retrieves
## the value of the inverse from the cache and returns it. If not, it computes the inverse, 
## stores it in the cache and returns the value of the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     
       
            inv <- x$getinv()
            
            if(!is.null(inv)) {
                  message("getting cached data")
                  return(inv)
            }
            
            data <- x$get()
            
            inv <- solve(data, ...)
            
            x$setinv(inv)
            
            inv
      
      
      
}
