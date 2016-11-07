## this function calculates for the inverse of x and also sets the answer into a cache
## so that when it is run again and the answer already exists, the cache is used and time is saved

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



cacheAnswer <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) 
  {
    message("fetching the cached result")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  message("difference detected, fetching a new calculation")
  m
}


## here I pass the values for the test to makeCacheMatrix and assign result to CachedMatrix
## then I call the CacheAnswer fun() to see if the answer is the same already (cached) or new

CachedMatrix = makeCacheMatrix(diag(7,10))
cacheAnswer(CachedMatrix)

> CachedMatrix = makeCacheMatrix(diag(7,10))
> cacheAnswer(CachedMatrix)
difference detected, fetching a new calculation
           [,1]      [,2]      [,3]      [,4]      [,5]
 [1,] 0.1428571 0.0000000 0.0000000 0.0000000 0.0000000
 [2,] 0.0000000 0.1428571 0.0000000 0.0000000 0.0000000
 [3,] 0.0000000 0.0000000 0.1428571 0.0000000 0.0000000
 [4,] 0.0000000 0.0000000 0.0000000 0.1428571 0.0000000
 [5,] 0.0000000 0.0000000 0.0000000 0.0000000 0.1428571
 [6,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
 [7,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
 [8,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
 [9,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
[10,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
           [,6]      [,7]      [,8]      [,9]     [,10]
 [1,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
 [2,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
 [3,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
 [4,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
 [5,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
 [6,] 0.1428571 0.0000000 0.0000000 0.0000000 0.0000000
 [7,] 0.0000000 0.1428571 0.0000000 0.0000000 0.0000000
 [8,] 0.0000000 0.0000000 0.1428571 0.0000000 0.0000000
 [9,] 0.0000000 0.0000000 0.0000000 0.1428571 0.0000000
[10,] 0.0000000 0.0000000 0.0000000 0.0000000 0.1428571

## if I call just the cacheAnswer without supply a new value, it will see that it is the same
## provide the else of the if conditional

cacheAnswer(CachedMatrix)

fetching the cached result
           [,1]      [,2]      [,3]      [,4]      [,5]
 [1,] 0.1428571 0.0000000 0.0000000 0.0000000 0.0000000
 [2,] 0.0000000 0.1428571 0.0000000 0.0000000 0.0000000
 [3,] 0.0000000 0.0000000 0.1428571 0.0000000 0.0000000
 [4,] 0.0000000 0.0000000 0.0000000 0.1428571 0.0000000
 [5,] 0.0000000 0.0000000 0.0000000 0.0000000 0.1428571
 [6,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
 [7,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
 [8,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
 [9,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
[10,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
           [,6]      [,7]      [,8]      [,9]     [,10]
 [1,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
 [2,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
 [3,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
 [4,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
 [5,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
 [6,] 0.1428571 0.0000000 0.0000000 0.0000000 0.0000000
 [7,] 0.0000000 0.1428571 0.0000000 0.0000000 0.0000000
 [8,] 0.0000000 0.0000000 0.1428571 0.0000000 0.0000000
 [9,] 0.0000000 0.0000000 0.0000000 0.1428571 0.0000000
[10,] 0.0000000 0.0000000 0.0000000 0.0000000 0.1428571

## for the most part all I did was swap the solve() for mean() and added an additional print msg 
## within the else, also executing in debug() mode allows to see all the steps of the execution
## and that helps in understanding of the logic flow

debug(cacheAnswer)

> debug(cacheAnswer)

> cacheAnswer(CachedMatrix)
debugging in: cacheAnswer(CachedMatrix)
debug at #1: {
    m <- x$getinverse()
    if (!is.null(m)) {
        message("fetching the cached result")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    message("difference detected, fetching a new calculation")
    m
}
Browse[2]> 
debug at #2: m <- x$getinverse()
Browse[2]> 
debug at #3: if (!is.null(m)) {
    message("fetching the cached result")
    return(m)
}
Browse[2]> 
debug at #5: message("fetching the cached result")
Browse[2]> 
fetching the cached result
debug at #6: return(m)
Browse[2]> ls()
[1] "m" "x"
Browse[2]> m
           [,1]      [,2]      [,3]      [,4]      [,5]
 [1,] 0.1428571 0.0000000 0.0000000 0.0000000 0.0000000
 [2,] 0.0000000 0.1428571 0.0000000 0.0000000 0.0000000
 [3,] 0.0000000 0.0000000 0.1428571 0.0000000 0.0000000
 [4,] 0.0000000 0.0000000 0.0000000 0.1428571 0.0000000
 [5,] 0.0000000 0.0000000 0.0000000 0.0000000 0.1428571
 [6,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
 [7,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
 [8,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
 [9,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
[10,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
           [,6]      [,7]      [,8]      [,9]     [,10]
 [1,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
 [2,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
 [3,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
 [4,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
 [5,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
 [6,] 0.1428571 0.0000000 0.0000000 0.0000000 0.0000000
 [7,] 0.0000000 0.1428571 0.0000000 0.0000000 0.0000000
 [8,] 0.0000000 0.0000000 0.1428571 0.0000000 0.0000000
 [9,] 0.0000000 0.0000000 0.0000000 0.1428571 0.0000000
[10,] 0.0000000 0.0000000 0.0000000 0.0000000 0.1428571
Browse[2]> x
$set
function (y) 
{
    x <<- y
    m <<- NULL
}
<environment: 0x108340388>

$get
function () 
x
<environment: 0x108340388>

$setinverse
function (solve) 
m <<- solve
<environment: 0x108340388>

$getinverse
function () 
m
<environment: 0x108340388>

Browse[2]> 
exiting from: cacheAnswer(CachedMatrix)
