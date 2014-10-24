## Put comments here that give an overall description of what your
## functions do
cachedInverse <- NULL
cachedMatrix <- NULL
## makeCacheMatix defines a list of functions to set and get matix, 
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


## cacheSolve function to returns the inverse matrix of input matrix. If they are already in the cache, 
## the function returns the cache , otherwise computes the inverse matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
