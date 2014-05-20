## IR236
## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix creates new type of the matrix object which can be cached
makeCacheMatrix <- function(x = matrix()) {
        ix <- NULL
        set <- function(y){
                x <<- y
                ix <<- NULL
        }
        get <- function() x
        setc <- function(s) ix <<- s
        getc <- function() ix
        list(set = set, get = get, setc = setc,getc = getc)
        
}


## cacheSolve function is used to invoke solve with caching
## its argument is a special object created by makeCacheMatrix
## Solve returns a matrix that is the inverse of 'x'

cacheSolve <- function(x) {
        
        ## checking if there is anything in cache
        s <- x$getc()
        if(!is.null(s)){
                ## if there cache return it
                message("getting cached data")
                return (s)
        }
        ## otherwise get the data
        data <- x$get()
        ## calculate solve
        s <- solve(data)
        ## put it in the cache
        x$setc(s)
        ## and return solve
        s
}
