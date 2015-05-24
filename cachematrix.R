## Programming Assignment 2 - Lexical Scoping 
##
## The following two (2) functions will cache and compute the inverse  
## of a given matrix. 
##
## The first (1) function "makeCacheMatrix" will create a special object that can 
## cache its inverse. 
##
## 1st Function

makeCacheMatrix <- function(x = matrix()) { 
        mcache <- NULL 
        set <- function(y) { 
                x <<- y
                mcache <<- NULL 
        } 
        get <- function() x 
        setinverse <- function(inverse) mcache <<- inverse 
        getinverse <- function() mcache 
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
} 

## This second (2) function computes the inverse of the special "matrix" returned
## by first (1) function "makeCacheMatrix". 
## If the inverse has already been calculated (and the matrix has not changed), then 
## the function "cacheSolve" should retrieve the inverse from the cache. 
##
## 2nd Function

cacheSolve <- function(x, ...) { 
        mcache <- x$getinverse() 
        if(!is.null(mcache)) { 
                message("Getting cached data.") 
                return(mcache) 
        } 
        data <- x$get() 
        mcache <- solve(data, ...) 
        x$setinverse(mcache) 
        mcache 
} 
