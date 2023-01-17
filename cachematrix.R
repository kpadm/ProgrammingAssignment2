## Functions to calculate inverse of a matrix, and potentially save time
## by caching the value of already calculated inversions 
## Karthik Padmanabhan

# usage: mat_object <- makeCacheMatrix(M) where M is the matrix
# cacheSolve(mat_object)

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(mat = matrix()) {
        mat_inv <- NULL
        set <- function(y) {
                mat <<- y 
                mat_inv <<- NULL
        }
        get <- function() mat
        set_inverse <- function(solve) mat_inv <<- solve
        get_inverse <- function() mat_inv
        list(set=set, get=get, set_inverse=set_inverse, 
                get_inverse=get_inverse)

}


## computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above. If the inverse has already been 
## calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'mat'
        mat_inv <- mat$get_inverse()
        if(!is.null(mat_inv)){
                message("getting cached inverse")
                return(mat_inv)
        }
        data <- mat$get()
        mat_inv <- solve(data, ...)
        mat$set_inverse(mat_inv)
        mat_inv
}

# test 

M <- 1 * diag(10)
IM <- makeCacheMatrix(M)
cacheSolve(IM)
