## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list of 4 functions;set, get, setinverse and getinverse which
## allows the user to set and get a matrix and allows the user to set and get the inverse of a matrix. 
## Using the <<- operator, the function stores the matrix as well as it's inverse so that it can be used in a 
## different environment.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) i <<- inverse
        
        getinverse <- function() i
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve return the inverse of the matrix set by calling makeCacheMatrix fn.
## If the inverse matrix is cached, then it returns the cached value else it 
## computes the inverse, stores it and returns the same.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        data <- x$get()
        
        i <- solve(data)
        
        x$setinverse(i)
        
        i        
}
