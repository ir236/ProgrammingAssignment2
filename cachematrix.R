## IR236
## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix creates new type of the matrix object which can be cached
makeCacheMatrix <- function(x = matrix()) {
        ix <- NULL
        set <- function (y){
                x <<- y
                ix <- NULL
        }
        get <- function () x
        setcache <- function(s) ix <<- s
        getcache <- function () ix
        list(set = set, get = get, setcache = setcache,getcache = getcache)
        
}


## cacheSolev function is used to inviole solve with caching
## its argument is a speciall object created by makeCacheMatrix

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        ## checking if there is anything in cache
        s <- x$getcache()
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
        x$setcache(s)
        ## and return solve
        s
}
