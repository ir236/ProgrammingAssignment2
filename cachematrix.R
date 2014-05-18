## IR236
## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ix <- NULL
        set <- function (y){
                x <<- y
                ix <- NULL
        }
        get <- function () x
        setsolved <- function(s) ix <<- s
        getsolved <- function () ix
        list(set = set, get = get, setsolved = setsolved,getsolved = getsolved)
        
}


## Write a short comment describing this function

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolved()
        if(!is.null(s)){
                message("getting cached data")
                return (s)
        }
        data <- x$get()
        s <- solve(data)
        x$setsolved(s)
        s
}
