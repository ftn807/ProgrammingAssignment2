## Put comments here that give an overall description of what your
## functions do

## This function creates a special list containing functions that can:
## set the value of a matrix
## get the value of a matrix
## set the value of the inverse of a matrix
## get the value of the inverse of a matrix
## It also can hold the values of a passed-in matrix as well as the inverse of that matrix
makeCacheMatrix <- function(x = matrix()) {
        ## always initialize i as null
        i <- NULL
        ## sets the internal variable x to the passed in matrix; clears any old i value that might be present 
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ## returns the value of the matrix x
        get <- function() x
        ## sets the internal variable i to the inverse of the matrix x
        setInverse <- function(solve) i <<- solve
        ## returns the value of the internal variable i
        getInverse <- function() i
        ## returns a description of the functions encapsulated by the makeCacheMatrix function
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data 
## and sets the value of the inverse in the cache via the setInverse function.
cacheSolve <- function(x, ...) {
        ## Get the inverse variable 'i' from the passed-in makeCacheMatrix object
        i <- x$getInverse()
        ## Checks to see if i is not null
        if(!is.null(i)) {
                ## i is not null, so print out a message and then return the value i
                message("getting cached data")
                return(i)
        }
        ## i is null, so we'll need to calculate the inverse
        ## First get the matrix from the passed-in makeCacheMatrix object
        data <- x$get()
        ## Use the solve function to calculate the inverse
        i <- solve(data, ...)
        ## Set the inverse in the passed-in makeCacheMatrix object so it can be cached
        x$setInverse(i)
        ## return the inverse
        i
}
