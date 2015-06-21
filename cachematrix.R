##  Caching the Inverse of a Matrix
##  inverse a matix and save the result to cache
##  return the cache result unless the source matrix is changed
##  example:
##  mtx <- matrix(c(33, 4, 34, 5, 54, 65, 24, 52, 59), 3, 3)
##  mcm <- makeCacheMatrix(mtx)
##  cacheSolve(mcm)

##  makeCacheMatrix
##  params: x: <matrix>
##  return: a list of 4 functions ($get, $set, $setInverse, $getInverse)
##  example:
##  mcm <- makeCacheMatrix(matrix1) 
##  return maxtrix and cache: mcm.get()
##  to change matrix:  mcm$setInverse(matrix2)
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL

    set <- function(y) {
        x <<- y
        i <<- NULL
    }

    get <- function() x

    setInverse <- function(inv) i <<- inv

    getInverse <- function() i

    list(set = set, 
         get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)

}

##  cacheSolve
##  params: x: <list> (return from makeCacheMatrix)
##  return: inversed matrix
##  notes: 
##  calcute matrix inverse for first time
##  return cached inverse value if call again
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()

    if(!is.null(i)) {
        message('getting cached data')
        return(i)
    }

    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
