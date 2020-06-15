## makeCacheMatrix creates an object which contains 4 functions. 
## To use it, first define an square matrix. E.g Test <- rbind(c(4,5.6),c(7.6, 02))
## now define Ans <-makeCacheMatrix(Test)
## to return the inverse cacheSolve(Ans)
## you can set the value of the inverse by Ans$setinv(square matrix)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #Placeholder for future value  
  set <- function(y){ x <<- y; m <<- NULL} #Reset the inv to NULL
  get <- function() {x}                    #its a way to write get <- function() {x}
  setinv <- function(inv) {m <<- inv}   #assign inv to m
  getinv <- function() {m}                #Return m
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }else {
    data <- x$get()
    message("Calculating inv")
    m    <- solve(data, ...)
    x$setinv(m)
    m
  }
}  

