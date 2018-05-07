##
## We're creating a method to cache the inverse of a numeric matrix so
## we don't recompute it every time we need it. The returned value is
## a list of four functions: to set and get the matrix as well as to
## set and get the inverse of the matrix


## This function creates a special "matrix" object that can cache its
## inverse. It returns a list of four functions.
##
makeCacheMatrix <- function(x = matrix())
{
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setinv <- function(inv) im <<- inv
    getinv <- function() im
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned
## by "makeCacheMatrix" above. If the inverse has already been
## calculated (and the matrix has not changed), then "cacheSolve"
## retrieves the inverse from the cache.
##
cacheSolve <- function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'
    im <- x$getinv()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setinv(im)
    im
}

## Testing ...
##
## Create a sample matrix, assign to "B"
B<-matrix(c(4,2,3, 7,5,5, 7,2,8),nrow=3,ncol=3)

## Create the the matrix list "object" for it
##
B1 <- makeCacheMatrix(B)


## Try it out the first time:
##
cacheSolve(B1) # inverse returned after computation

## Try it out a second time:
##
cacheSolve(B1) # inverse returned from cache

## Check if it's actually an inverse.
## You'll need to try this in an interactive session to produce the
## desired output.
NM <- B %*% cacheSolve(B1)
NM[] <- vapply(NM,round,numeric(1))
