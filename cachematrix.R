## Put comments here that give an overall description of what your
## functions do

## These 2 functions are used to provide functions to cache a matrix and
## to compute and cache its inverse.

## Write a short comment describing this function

## This function returns a list of functions that sets the values of a matrix,
## gets the values of the matrix, sets the values of the matrix inverse, and
## gets the values of the matrix inverse.

makeCacheMatrix <- function(mat = matrix()) {
        ## Returns a list of functions that operate on cached objects

        matinv <- NULL             #initialize inverse matrix to null
        #Define set, get, setsolve, and getsolve functions
        set <- function(y) {   
                mat <<- y          #caches matrix object
                matinv <<- NULL    #set to null, flushes cached matinv object
        }
        get <- function() mat
        setsolve <- function(solve) matinv <<- solve
        getsolve <- function() matinv
        #Return list of defined functions(methods) and cached matrix
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function

## This function returns the inverse of a cached matrix and caches the result
## If the function is called again and the matrix has not been modified the
## cached result is returned, otherwise the inverse matrix is computed.

cacheSolve <- function(x, y <- x$get(), ...) {
        ## Return a matrix that is the inverse of 'x'
        ## NOTE: matrix in calling environment can be passed as a 2nd argument
        ##       to test for equality. If not passed the 2nd arg defaults to
        ##       the cached matrix, y=x$get()

        ## Check equality of matrix in the calling environment with cached
        ## matrix - if not identical then cache changed matrix with x$set(y)
        if(!identical(x$get(), y)) { x$set(y) }

        ## If cached immediately return cached result
        matinv <- x$getsolve()
        if(!is.null(matinv)) {
                message("getting cached data")
                return(matinv)
        }
        ## If not cached compute, cache it, and return result
        data <- x$get()
        matinv <- solve(data, ...)
        x$setsolve(matinv)
        matinv
}

