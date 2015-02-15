## Below are two functions that are used to create a
## special object that stores a matrix and caches its inverse.

## The function `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
            ## Set the value of the matrix
            i <- NULL
            set <- function(y) {
                    x <<- y
                    i <<- NULL
            }
            
            ## Get the value of the matrix
            get <- function() x
            
            ## Set the value of the inverse
            setinverse <- function(solve) i <<- solve
            
            ## Get the value of the inverse
            getinverse <- function() i
            list(set = set, get = get,
                   setinverse = setinverse,
                   getinverse = getinverse)        
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the inverse in the cache via the `setinverse`
## function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        
        ## Check if the inverse is already stored
        if(!is.null(i)) {
                    message("Getting chached data")
                    return(i)
        }
        
        ## Calculate the inverse if it is not yet available
        data <- x$get()
        i <- solve(data, ...)
        ## and set the inverse in the cache
        x$setinverse(i)
        i
}