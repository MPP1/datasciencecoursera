### following you´ll find a pair of functions, which will cache the inverse of a given matrix ###


### creates a special matrix object that can cache its inverse ###
makeCacheMatrix <- function(m = matrix()) 

 {
   ### initialize the inverse ###
   i <- NULL

   ### method to set the matrix ###
     set <- function(matrix)
     {
       m <<- matrix
       i <<- NULL
     }

     ### method the get the matrix ###
     get <- function()
       {
         ### returning the matrix ###
         m
       }

     ### method to set the inverse of the matrix ###
     setInverse <- function(inverse)
       {
         i <<- inverse
       }

     ### method to get the inverse of the matrix ###
     getInverse <- function()
       {
         ### returning the inverse ###
         i
       }

     ### returning a list of the methods ###
     list(set = set, get = get,
     setInverse = setInverse,
     getInverse = getInverse)
 }


 ### generate the inverse of the special matrix which is returned by function "make_cache_matrix" ###
 ### above. If the inverse has already been calculated and the matrix is not changed              ###
 ### then the "cachesolve" should retrieve the inverse from the cache                             ###

cacheSolve <- function(x, ...)
 {
   ### return a matrix that is the inverse of 'x' ###
   m <- x$getInverse()

   ### return the inverse if its already set ###
   if(!is.null(m))
     {
       message("getting cached data")
       return(m)
     }

   ### get the matrix from our object ###
   data <- x$get()

   ### calculate the inverse using matrix multiplication ###
   m <- solve(data) %*% data

   ### setting inverse to the object ###
   x$setInverse(m)

   ### returning the matrix ###
   m
 }
