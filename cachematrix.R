## The makeCacheMatrix function defines a list of functions to set and get matix,
## cacheSolve function to return the inverse matrix of input matrix. If they are already in the cache, 
## the function returns the cache , otherwise computes the inverse matrix

## Initial the cache variables
cachedInverse <- NULL
cachedMatrix <- NULL

## makeCacheMatix defines a list of functions:
## set: to set the input matrix, and cache it to the cache variables, reset the inverse matrix to NULL
## get: to get the input matrix
## getcachedMatrix: to get the cached Matrix from the previous input
## setinverse: set the inverse matrix to the cached variable
## getinverse: get the cached inverse matrix
makeCacheMatrix <- function(x = matrix()) {
     set <- function(y) {
       x <<- y
       cachedMatrix <<- x
       cachedInverse <<- NULL
     }    
     get <- function() x 
     getcachedMatrix <- function() cachedMatrix
     setinverse <- function(inverse) cachedInverse <<- inverse
     getinverse <- function() cachedInverse
     list(set = set, get = get, getcachedMatrix = getcachedMatrix, setinverse = setinverse, getinverse = getinverse)
}


##cacheSolve function take the makeCacheMatix as the input, comparing the input matrix with the cached matrix. 
## If the matrix  are already in the cache, return the cached inverse matrix
## otherwise set the new input matrix, compute the inverse matrix, set the computed inverse matrix to the cache variable
## and return the computed inverse matrix
cacheSolve <- function(x, ...) {

    input_matrix <- x$get()
    inverse_matrix <- x$getinverse()
    cached_matrix <- x$getcachedMatrix()
    
    if(!is.null(inverse_matrix) && isTRUE(all.equal(cached_matrix,input_matrix))){
        message("getting the cached inverse matrix")
        return(inverse_matrix)
    }
    
    x$set(input_matrix)
    inverse_matrix <- solve(input_matrix, ...)
    x$setinverse(inverse_matrix)
    inverse_matrix
}
