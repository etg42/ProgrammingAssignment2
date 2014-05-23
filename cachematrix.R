## These functions cache the value of the inverse of a matrix
## so that it only has to be calculated once


## This function creates a list that contains a matrix and functions to 
## set the value of the matrix, get the value of the matrix, solve for the 
## inverse of the matrix, and get the inverse of the matrix if it has 
## been solved already

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL                                # initializes s (the inverse)
        set <- function(y) {
            x <<- y                              # sets the matrix
            s <<- NULL                           # resets the inverse to NULL
        }                                        #    when the matrix changes
        get <- function() x                      # returns the matrix
        setsolve <- function(solve) s <<- solve  # solves for the inverse
        getsolve <- function() s                 # returns the inverse
        list(set = set, get = get,               # creates a list of the 
             setsolve = setsolve,                #    above functions
             getsolve = getsolve)
}



## This function takes the list created above, checks to see if the inverse
## of the matrix has already been calculated, and either returns the 
## previously-calculated value or solves for the inverse of the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    s <- x$getsolve()                     # get the inverse
    if(!is.null(s)) {                     # if the inverse has already been
        message("getting cached data")    #    calculated, return the solution
        return(s)
    }
    data <- x$get()                       # get the matrix
    s <- solve(data, ...)                 # solve for the inverse
    x$setsolve(s)                         # set the inverse in the list
    s                                     # return the inverse
}
