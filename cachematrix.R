## Caching the Inverse of a Matrix:
## Below two functions that create and stores a matrix, calculate its inverse and cache its inverse.

## The makeCacheMatrix function creates a matrix that can cache its inverse.
## Steps are:

## 1, set the value of the matirx
## 2, get the value of the matrix
## 3, set the value of the inverse
## 4, get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The cacheSolve function below computes the inverse of the matrix created by the makeCacheMatrix function above.
## If the inverse has already been calculated (and the matrix has not changed),
## then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

