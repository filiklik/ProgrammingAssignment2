## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## The following function creates a special "matrix" object that can cache its inverse.

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


## This function calculates the inverse of the special "matrix" created 
## above. If the inverse has already been calculated (and the 
## matrix has not changed), then it must retrieve the inverse from the cache.

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
> #Testing My Functions
>  #source("ProgrammingAssignment2/cachematrix.R")
>  my_matrix <- makeCacheMatrix(matrix(3:6, 2, 2))
>  my_matrix$get()
     [,1] [,2]
[1,]    3    5
[2,]    4    6
> my_matrix$getInverse()
NULL
> cacheSolve(my_matrix)
     [,1] [,2]
[1,]   -3  2.5
[2,]    2 -1.5
> cacheSolve(my_matrix)
getting cached data
     [,1] [,2]
[1,]   -3  2.5
[2,]    2 -1.5
> my_matrix$getInverse()
     [,1] [,2]
[1,]   -3  2.5
[2,]    2 -1.5
> my_matrix$set(matrix(c(3, 5, 1, 2), 2, 2))
> my_matrix$get()
     [,1] [,2]
[1,]    3    1
[2,]    5    2
> my_matrix$getInverse()
NULL
> cacheSolve(my_matrix)
     [,1] [,2]
[1,]    2   -1
[2,]   -5    3
> cacheSolve(my_matrix)
getting cached data
     [,1] [,2]
[1,]    2   -1
[2,]   -5    3
> my_matrix$getInverse()
     [,1] [,2]
[1,]    2   -1
[2,]   -5    3
