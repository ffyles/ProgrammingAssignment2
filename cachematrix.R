## function allows for the caching of the inverse of a matrix, to be retrieved for future use to prevent costly computer work

## Below function computes the inverse of a matrix (x) and stores it for future reference

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set =  set, get = get, setsolve = setsolve, getsolve=getsolve)
}


## cacheSolve checks whether the inverse of x is already within the global environment - if so it retrieves it from cached data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve
        if(!is.null(s)){
            message("getting cached data")
            return(s)
        }
        data <- x$get()
        s <- solve(data,...)
        x$setsovlve(s)
        s
}
