## Function to Cache the Inverse of a Matrix

## These functions are used to store a special matrix and cache the inverse of this special matrix
## This could be very beneficial when working with a matrix of which it wold be costly to compute the inverse each time you need it 

## The makeCacheMatrix function makes a special matrix that will
## set the the matrix, 
## get the matrix, 
## set the inverse of the matrix 
## and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL 
        }
        get <- function() x
        set_inverse <- function(inv) inverse <<- inv
        get_inverse <- function() inverse 
        list(set = set, 
             get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

 
## If the inverse of the matrix, created by makeCacheMatrix, has already been computed and cached
## the cacheSolve function will retrieve the cached inverse
## If not, the cacheSolve function will compute the inverse of the matrix and set this result in the cache
cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        inv_matrix <- x$get_inverse()
        if(!is.null(inv_matrix)) { 
                message("getting cached data")
                return(inv_matrix) 
        }
        data <- x$get() 
        inv_matrix <- solve(data) 
        x$set_inverse(inv_matrix) 
        inv_matrix 
}
