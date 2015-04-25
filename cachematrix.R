##Stephane MASSON - 20150425 
##Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly . This pair of functions caches the inverse of a matrix.
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and ##the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.

## The function makeCacheMatrix creates a special "matrix" object, which can be inverted. The operation that are done:
##1 - set the value of the matrix
##2 - get the value of the matrix
##3 - set the value of the inverse of the matrix
##4 - get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
##Initialization
        inverse <- NULL
##1 - set the value of the matrix
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
##2 - get the value of the matrix
        get <- function() x
##3 - set the value of the inverse of the matrix
        setinverse <- function(inv) inverse <<- inv
##4 - get the value of the inverse of the matrix
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special "matrix" created with the function makeCacheMatrix. 
##1 -It first checks to see if the mean has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
##2 - Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function
##Important note - This function always assume that the matrix is convertible

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
		
		##1 -It first checks to see if the mean has already been calculated.
        if(!is.null(inverse)) { message("getting cached data")
                                return(inverse)
        }
		##2 - Otherwise, it calculates the inverse of the matrix and sets the value of the inverse 
        matrix.data <- x$get()
        inverse <- solve(matrix.data,...)
        x$setinverse(inverse)
		
		##return the result
        inverse
}
