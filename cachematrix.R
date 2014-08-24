## These two functions work together to optimize performance
## handling matrices. With large matrices that are not
## changing, this technique eliminates reptitive processing.
## The functions also demonstrate lexical scope.

## The function makeCacheMatrix takes a matrix as a parameter.
## It solves the matrix and stores it in the parent scope.



makeCacheMatrix <- function(x = matrix()) {

    m <- NULL

    set <- function(y){

        x <<- y

        m <<- NULL

    }

    get <- function() x

    setmatrix <- function(solve) m <<- solve

    getmatrix <- function() m

    list(set = set, get = get,

       setmatrix = setmatrix,

       getmatrix = getmatrix)

}

## The function cacheSolve cooperaters with makeCacheMatrix.
## If the matrix has been solved previously, which is
## tested by checking the list "m" for NULL, it posts
## the message and returns the solved matrix.  Otherwise
## it solves the matrix.



cacheSolve <- function(x=matrix(), ...) {

    m <- x$getmatrix()

    if(!is.null(m)) {

        message("getting cached matrix data")

        return(m)

    }

    datamatrix <- x$get()

    m <- solve(datamatrix, ...)

    x$setmatrix(m)

    m

}
