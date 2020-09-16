## makeCacheMatrix creates a special matrix object which caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {            ##set the matrix x
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set=set, 
             get=get, 
             setInverse=setInverse, 
             getInverse=getInverse)
        
        ##returns a list of methods
}


## cacheSolve calculates the inverse or gives it back if already calculated

cacheSolve <- function(x, ...) { 
        m <- x$getInverse()             ##matrix that is the inverse of "x"
        
        ##Give inverse if already set
        if(!is.null(m)) {
                message("getting chached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}


##Computing the inverse of a square matrix can be done with the 
## solve function in R. For example, if X is a square invertible matrix, 
##then solve(X) returns its inverse.
