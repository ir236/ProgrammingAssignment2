## IR236
## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix creates new type of the matrix object which can be cached
makeCacheMatrix <- function(x = matrix()) {
        ## sets cache to NULL at initilization (init)
        ix <- NULL
        ## set function used for after init set of the object x
        set <- function(y){
                ## sets x in the environment of the makeCacheMatrix function
                x <<- y
                ## clears the cache 
                ix <<- NULL
        }
        ## returns the matrix set via init or set function
        get <- function() x
        ## setc sets the cache
        setc <- function(s) ix <<- s
        ## getc gets the cache
        getc <- function() ix
        ##returns list of functions
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
