## These functions can be used to calculate the inverse of a given square matrix
## The inverse of a matrix is calculated using the "solve" function.
## Since this computation is costly, once the inverse has been calculated it is cached to be reused
## if the same inverse is requested again
## (as stated in the assignment we assume the matrix is invertible; no check is done to see whether it's true or not)

## example of usage:
## > x<-matrix(c(1:4), nrow=2, ncol=2)
## > x
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
##
## > XX <- makeCacheMatrix(x)
## > XX$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## 
## xInv<-cacheSolve(XX)
## > xInv
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## You can prove that the inverse has been properly calculated by multiplying x by xInv and checking
## that the result is the identity matrix
## > x %*% xInv
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1
##
## If we try to re-calculate the inverse, we see that the cached result is reused and the result is the same
## > xInv2 <- cacheSolve(XX)
## reusing cached result
## > xInv2
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5



## makeCacheMatrix creates an object that is used to calculate and store the cached inverse of a matrix
## The object is a list containing the functions to
## - set the value of the matrix
## - get the value of the matrix
## - set the inverse of the matrix
## - get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    
    # when initialised the inverse isn't calculated yet
    inv <- NULL
    
    # function sets the value of the matrix and, since it's a new value, says that the 
    # inverse isn't calculated yet  (note <<- is used!)
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    # function gets the value of the matrix
    get <- function() x
    
    # function sets the cached value of inverse matrix (note <<- is used!)
    setInverse <- function(invrs) inv <<- invrs
    
    # function gets the cached value of the inverse (returns NULL if not calculated yet)
    getInverse <- function() inv
    
    # all functions returned as a list
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
}


## cacheSolve calculated the inverse of the matrix
## "solve" function is used to perform the calculation, but before doing that a check is
## made to see if the inverse has already been calculated and can be retrieved from the
## cache. In that case the computation is not made again, otherwise inverse is calculated
## and the result is cached to be reused in case a new request is made

cacheSolve <- function(x, ...) {
    
    # get the value from the cache
    inv <- x$getInverse()
    # if cache is available, its content is used to return the value of the inverse matrix, 
    # skipping the re-calculation
    if (!is.null(inv)) {
        message("reusing cached result")
        return(inv)
    }
    # this code is executed only if cache was not available
    # in this case the inverse is calculated using the "solve" function, the result
    # is stored in the cache and also returned as result
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
