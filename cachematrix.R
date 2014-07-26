# These functions are for caching a matrix and its inverse so we don't have
# to needlessly compute the solution over again.


# This takes a matrix (x) and returns a list of 4 functions which are meant
# to, respectively: cache the matrix, return the matrix, cache its inverse,
# and return the inverse.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL  # Inverse stored here
    set <- function(y) {
        x <<- y  # Change the matrix
        i <<- NULL  # Reset the inverse
    }
    get <- function() x
    # setInver sets the inverse to whatever value it's given, so it's only
    # really meant to be called by cacheSolve
    setInver <- function(inver) i <<- inver
    getInver <- function() i
    list(set=set, get=get, setInver=setInver, getInver=getInver)
}


# This takes a list (x) produced by makeCacheMatrix and returns the cached
# inverse of the matrix.  If there isn't any, then it calculates one, caches
# it, and returns that.
#
# (If you want, you can also pass extra args to solve() via the ...
# argument, but that'll only make a difference when the inverse isn't
# already cached.  See ?solve.)
cacheSolve <- function(x, ...) {
    i <- x$getInver()
    if(!is.null(i)) {
        message("getting cached data 4 u, baby")
        return(i)
    }
    data <- x$get()
    # We make sure that solve()'s formal argument b is missing because we
    # only want to _invert_ matrices.
    i <- solve(data, b=, ...)
    x$setInver(i)
    i
}
