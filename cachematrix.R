## These 2 functions were created to create a matrix object
## that can cache its inverse

## This first function creates the matrix which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<-y
                m <<- NULL
        }
        get <- function() x
        setm <- function(inverse) {m <<- inverse}
        getm <- function() m
        list (set = set, get = get, setm = setm, getm = getm)
}


## The second function computes the inverse of the matrix calculated by 
## our first function (makeCacheMatrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        mat <- x$get()
        m <- solve(mat, ...)
        x$setm(m)
        m
}
