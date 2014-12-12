### This pair of functions calculate and cache the inverse of a specified matrix, assuming it is already
### square. Caching allows for efficient retrieval of the inverse in cases where repeated usage are warranted.

## This function reads an inputted matrix and caches its inverse for future reference
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # inv is the inverted matrix initialized to "NULL"
        
        get <- function() {x}                           # returns the value of the original matrix
        
        setinv <- function(inverse) { inv <<- inverse } # caches the value of the inverse
        
        getinv <- function() { inv }                    # fetches the cached inverse of the original matrix
        
        list(get = get, setinv = setinv, getinv = getinv)
}


## This function either calculates or retrieves the inverse of a matrix preprocessed by the makeCacheMatrix
## function. It retrieves the cached inverse if it has been cached or otherwise calculates it and returns
## the inverted matrix.
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        
        if(!is.null(inv)) {   # checks to see if the inverse has already been calculated and cached.
                
                message("You're in luck, it's already cached! Fetching...") # alerts user to when the cached version is being used
                return(inv)   # if the inverse exists it merely returns it.
        } else {              # if the inverse has not already been calculated and cached the following occurs:
                data <- x$get()     # gets the matrix x
                inv <- solve(data)  # calculates the inverse of matrix x
                x$setinv(inv)       # caches the calculated inverse of matrix x
                inv                 # displays the inverse of matrix x
        }
        
}
