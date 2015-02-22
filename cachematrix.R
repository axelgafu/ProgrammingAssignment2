#' The current .R file contains a set of functions that provide a mechanism to
#' make more efficient the work with matrices. The way in which that is
#' achieved is by caching the result of the marix operations. The result is
#' calculated only the first time and it remains in memory until the matrix is
#' replaced. Whenever the client code uses the CacheMatrix API.
#' 
#' @author Axel Garcia.

#' @name makeCacheMatrix
#' 
#' This function creates a CacheMatrix variable which is required by the
#' cacheSolve function.
#' 
#' @param x should be an invertible square matrix.
#' 
#' @return A CacheMatrix object with the matrix specified by \code{x}.
#' 
#' @examples
#' > mat <- matrix( data=rep( c(1, 2, 3, 4), times=9), 3, 3)
#' > cm <- makeCacheMatrix( mat )
#' 
#' @export
makeCacheMatrix <- function(x = matrix()) 
{
    inverse <- NULL
    set <- function( y )
    {
        x <<- y
        inverse <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function( pInverse ) inverse <<- pInverse 
    getInverse <- function() inverse
    
    list( set=set, getMatrix=getMatrix, 
          setInverse=setInverse, getInverse=getInverse )
}


#' @name cacheSolve
#' 
#' This function computes the inverse of the matrix passed as paremeter. The
#' result is returned to the caller and set to the cacheMatrix variable passed
#' as parameter. 
#' 
#' After calling this function, the inverse matrix can be retrieved from the
#' cacheMatrix variable by calling its cacheMatrix$getInverse() function.
#' 
#' If a message like the following is shown when executing cacheSolve, that
#' means the matrix was not invertible:
#'     > cmi <- cacheSolve( cm )
#'     Show Traceback
#' 
#'     Rerun with Debug
#'     Error en solve.default(x$getMatrix()) : 
#'         Lapack routine dgesv: system is exactly singular: U[2,2] = 0 >
#'     
#' @param x is a CacheMatrix data type.
#' 
#' @return The inverse matrix of cacheMatrix \code(x)
#' 
#' @examples
#' > mat <- matrix( data=rep( c(1, 2, 3, 4), times=9), 3, 3)
#' > cm <- makeCacheMatrix( mat )
#' > cmi <- cacheSolve( cm )
#' > cmi
#' [,1]        [,2]        [,3]
#' [1,] -0.19444444  0.05555556  0.36111111
#' [2,]  0.27777778 -0.22222222  0.05555556
#' [3,]  0.02777778  0.27777778 -0.19444444
#' 
#' ## Second time cacheSolve is called it will return the cahced inverse:
#' > cmi <- cacheSolve( cm )
#' Getting cached inverse...
#' > cmi
#' [,1]        [,2]        [,3]
#' [1,] -0.19444444  0.05555556  0.36111111
#' [2,]  0.27777778 -0.22222222  0.05555556
#' [3,]  0.02777778  0.27777778 -0.19444444
#' >
#' 
#' @export
cacheSolve <- function( x ) 
{
    if( is.null( x$getInverse() ) )
    {
        x$setInverse( solve( x$getMatrix() ) )
    }
    else
    {
        message( "Getting cached inverse..." )
    }
    
    invisible( x$getInverse() )
}
