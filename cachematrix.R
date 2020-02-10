## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	  inv_mat <- NULL
        set <- function(z) {
                x <<- z
                inv_mat <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse_mat) inv_mat <<- inverse_mat
        getinverse <- function() inv_mat
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  inv_mat <- x$getinverse()
        if (!is.null(inv_mat)) {
                
                return(inv_mat)
        }
        m <- x$get()
        inv_mat <- solve(m, ...)
        x$setinverse(inv_mat)
        inv_mat

}
