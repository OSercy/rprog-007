cacheSolve <- function(x,...){
  inv <- x$getinv()
  if(!is.null(inv)){
    //if the inverse matrix is already in cache
    message("getting cached inverse matrix")
    return(inv)
  }
  data<- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}
