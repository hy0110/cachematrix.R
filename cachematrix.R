## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { ##set the infomation of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x  ## output the matrix
        setsolve <- function(solve) m <<- solve ##set the Inverse matrix of the matrix
        getsolve <- function() m.               ##output the Inverse matrix
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) { ##check whether the matrix have existed
                message("getting cached data")
                return(m) ##if existed, no need to calculate
        }
        data <- x$get()   ##no existed, set the matrix
        m <- solve(data, ...) ##calculate the inverse matrix
        x$setsolve(m)     ##store the inverse matrix
        m                 ##output the inverse matrix
}
