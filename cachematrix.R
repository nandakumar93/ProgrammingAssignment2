##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        my_inv <- NULL  ##to get rid of the problem if inverse is already  set
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) my_inv <<- inverse
        getInverse <- function() my_inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes  inverse of the special "matrix" created by 
## makeCacheMatrix . If the inverse has already been calculated 
## then it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cache_fun_inv <- x$getInverse()
       if (!is.null(cache_fun_inv)) {           ##check if there's a cached value  
                message("getting cached data")
                return(cache_fun_inv)
        }
        mat <- x$get()                                 ## else get the matrix
        cache_fun_inv<- solve(mat, ...)
        x$setInverse(cache_fun_inv)
        cache_fun_inv
}
