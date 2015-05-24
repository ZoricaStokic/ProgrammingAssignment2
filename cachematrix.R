makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL              # sets the value of m to NULL
                             #-provides a default if cacheSolve has not 
                             #yet been used.
    set <- function(y) {     #set the value of the matrix.
        x <<- y              #caches the inputted matrix so that 
                             #cacheSolve can check whether it has changed.
        inv <<- NULL         # sets the value of inv.
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)   
                             #creates a list to house the four functions.
}
cacheSolve <- function(x, ...) {      # Compare matrix to what was there 
                                      #before.
    inv <- x$getinverse()             # if an inverse has already been 
 
                                      #calculated this gets it.
    if(!is.null(inv)) {               # getting cached data-check to see 
                                      #if cacheSolve has been run before.
        message
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)                # compute the value of the inverse of
                                      #the input matrix.
    x$setinverse(inv)                 # run the setinverse function on the 
                                      #inverse to cache the inverse.
    inv                               # return the inverse.
}

