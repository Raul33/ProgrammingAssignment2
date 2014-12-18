## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function creates an object which contains a vector

makeCacheMatrix <- function(x = matrix()) {

	i <- NULL
    set <- function(y) {
            x <<- y
            i <<- NULL
    }

    get <- function() x
    setInv <- function(inv) i <<- inv
    getInv <- function() i
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
## This function can do two different things.
## First retrieves the value of i from the previous function (object), if is not null returns its value
## otherwise calculates the inverse of the matrix given, stores it in the previous object created with
## makeCacheMatrix function and returns the value of the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	 i <- x$getInv()
     if(!is.null(i)) {
     	message("getting cached data")
        return(i)
     }

    data <- x$get()
    i <- solve(data, ...) ## here it is where the inverse matrix is calculated
    x$setInv(i) ##  stores it in the previous object created with makeCacheMatrix
    i ## returns the value
}
