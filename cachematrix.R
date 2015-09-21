## Cache the inverse of a matrix to avoid it to be calculated agaian and again

## makeCacheMatrix -- takes a matrix as an input and defines 4 functions
## 1. set    : to set the matrix to an object created by makeCacheMatrix function
## 2. get    : to get the matrix
## 3. setinv : to set the inversed matrix
## 4. getinv : to get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setinv  <- function(inv) inv_x <<- inv
        getinv <- function() inv_x
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Return cached inverse of matrix if it has been calcualted earlier 
## or else return the fresh inverse value of matrix

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
