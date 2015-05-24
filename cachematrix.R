



makeVector <- function(z = numeric()) {
        n <- NULL                     #begins by setting the mean to NULL 
                                       #as a placeholder for a future value
        set <- function(b) {          #set the value of the vector
                z <<- b               #assigning the value of z to y
                n <<- NULL
        }
        get <- function() z            #get the value of the vector
        setmean <- function(mean) n <<- mean
        getmean <- function() n
        list(set = set, get = get,    #returns the vector containing 
                                      #all of the functions just defined
             setmean = setmean,
             getmean = getmean)
}
cachemean <- function(z, ...) {
        n <- z$getmean()              #gets the mean from the cache 
                                      #and skips the computation
                                      #if the mean has already been calculated
        if(!is.null(n)) {

                message("getting cached data")
                return(n)
        }
        data <- z$get()
        n <- mean(data, ...)
       z$setmean(n)                   #it calculates the mean of 
                                      #the data and sets the value of the mean
                                     #in the cache via the setmean function
        n
}


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

