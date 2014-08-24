## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix is a set of functions that are used to set of get the cached result of the invert of a Matrix.
## However, the calculation of the invert is not done in this function

makeCacheMatrix <- function(x = matrix()) {
    ## Initiate the solved value s to NULL
    s <- NULL

    ## create set and get function. Note the <<
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x

    ## create setsolve and getsolve functions. Note the <<
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    
    ## This is where the lexical scoping happens
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## The CacheSolve function uses the system defined solve() function to calculate the invert of a Matrix, then cache it. 
## It is calculated only when the cache doesn't exist yet

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    ## if S is not NULL, it means its has been calculted and cached already. 
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    ## Otherwise, we get x, calculate is inverse with solve, store it in S and return s
    data <- x$get()
    s <- solve(data)
    x$setsolve(s)
    s
}
