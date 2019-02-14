## In this R file, a function, cacheSolve, is created. This 
## function is used to cache the inverse of a matrix so that we
## don't need to compute this value repeatedly.

## makeCacheMatrix was created firstly in order to create the 
## special matrix can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set <-function(y){
                x<<- y
                m <<-NULL
        }
        get <-function() x
        setmatrix <-function(solve) m <<-solve
        getmatrix <- function() m
        list(set=set,get=get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
        
}


## This function gives the inverse of the special "matrix" 
## created by the function above. If the inverse has been 
## calculated, it should retrieve the value from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)){
                message("getting chched data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setmatrix(m)
        m
}

