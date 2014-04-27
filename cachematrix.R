## The problem is to set up an arrangement so that the inverse of
## a matrix (solve(x)) does not have to be called again if that has 
## previously been calculated. This script combines a "closure"
## (:= "function of functions"), which constructs a set of 
## anonymous functions, together with a second, "executive" 
## function, which together use R's scoping rules to establish 
## a process to automatically check that. 

## makeCacheMatrix: The "closure," it sets "anonymous" functions 
## set(), get(), setinverse() and getinverse(). Importantly, it also 
## creates variable "inv" which tracks whether the matrix's inverse
## already exists; initially set to NULL.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function () x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: The "executive," this function houses the key 
## process of checking whether the matrix inverse already exists, 
## retrieving it (with message) if it does, otherwise calculating 
## it using solve() and displaying whichever answer.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
