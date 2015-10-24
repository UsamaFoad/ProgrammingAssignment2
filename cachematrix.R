## Caching the Inverse of a Matrix
## Two functions to create: a) special matrix. b) Its invers & cache the invers   
## to retrieve it when need it. The functions take advantage of the scoping
## rules of the R language to preserve state inside of an R object. 
##
## These functions was created for Programming Assignment 2

## makeCacheMatrix(x = matrix())
## Function to create special matrix object that can cache its inverse.
## Created in accordance with the example provided in the assignment (makeVector)
## So, the function is created as list containing functions to set/get the value
## of the matrix & set/get the value of the inverse 
## The function created with assumption that the matrix supplied is always 
## invertible, so there is no need to test the matrix with det() function.
## Also, the solve() function should be used to compute the inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {                            
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##
## cacheSolve(x, ...)
## Function to computes the inverse of the special matrix returned by makeCacheMatrix
## If the inverse has already been calculated and the matrix not changed then the
## cachesolve will retrieve the invers from the casche
## Created in accordance with the example provided in the assignment (cachemean)
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}