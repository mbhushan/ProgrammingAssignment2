## makeCacheMatrix creates a matrix object with get and set apis for the matrix itself and
## it has getMatrixInv & setMatrixInv to access the inverse of the matrix (if possible)
## --Its assumed the matrix would always be square matrix but its not assumed that the 
## determinant of the matrix would always be NON-ZERO!

## creates the matrix object and exposes the getter/setter functions for matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL

    set <- function(y) {
        x <<- y
        minv <<- NULL
    }

    get <- function() x

    setMatrixInv <- function(inv) minv <<- inv
    getMatrixInv <- function() minv

    list(set=set, get=get, getMatrixInv=getMatrixInv, setMatrixInv=setMatrixInv)
}


## calculates the inverse (if det(matrix) != 0) of the square matrix and caches the results.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minv <- x$getMatrixInv()
        if (!is.null(minv)) {
            message("getting cached matrix inverse")
            return(minv)
        }
        mat <- x$get()
        mdet <- det(mat)
        mdet <- round(mdet,3)
        if (mdet == 0) {
            message("determinant of the matrix is ZERO, hence no inverse possible")
            minv <- NULL
        } else {
            minv <- solve(mat)
        }
        x$setMatrixInv(minv)
        return(minv)
}
