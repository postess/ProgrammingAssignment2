##These two functions interact to calculate the inverse of a matrix, cache the value of the inverse, and return that value. 
## When cacheSolve() calculates an inverse, setinv() stores the value in the enclosing environment of makeCacheMatrix.  

## The first function is a constructor function like the make.power function in the scoping rules video lecture. 
## makeCacheMatrix is used to create four other functions that reach into the environment of makeCacheMatrix to get 
## and set values for x (the matrix) and inv (the inverse). makeCacheMean() returns those functions in a list that users 
## and other functions (like cacheSolve) can retrieve using the $ operator. 

makeCacheMatrix <- function(x = matrix()) {         ## makeCacheMatrix is a function that takes as its argument a matrix, x.
        inv <- NULL                                 ## inv, which will be our name for the inverse eventually, is defined as NULL, so inv here is a placeholder.
        set <- function(y)  {                       ## the set() function resets the value of the matrix when the matrix changes, if a user runs set(). For y that is passed through set(), 
                x <<- y                             ## set() obliterates the stored value in matrix x, resets them to the new values passed to the function, and resets the inverse to NULL. 
                                                    ## This prevents the function from returning the wrong inverse from the cache when cacheSolve is called.
                inv <<- NULL                         
        }  
        
        get <- function() x                          ## get is a function that returns the matrix, x. CacheSolve will use it to calculate the inverse if an inverse value has not been cached for x.
        setinv <- function(new_inv) inv <<- new_inv  ## setinv() is a function that takes the argument new_inv. The <<- operator stores the previously calculated inverse in the enclosing enviroment 
        getinv <- function() inv                     ## getinv() returns the inverse, either from the cache if setinv() stored it from an earlier execution, or from the calculation in cacheSolve().
                                                     ## so it can be accessed after the inverse is calculated.
        
        list(set = set, get = get, setinv = setinv, getinv = getinv) }     ## returns a list of the functions created in makeCacheMatrix

## the cacheSolve function retrieves the inverse from the cache if it has already been calculated and computes the inverse of the matrix returned by makeCacheMatrix() if it can't find a cached inverse value.  

cacheSolve <- function(x,...) {                ## cacheSolve is a function that takes x as an argument. This is a DIFFERENT x than x in make makeCache Matrix. "..." allows additional paramters to be passed to solve() below. 
        inv <- x$getinv()                      ## inv is assigned the value returned by the getinv() function. This is an example of the cacheSolve() function reaching into the enclosing environment of makeCacheMatix using lexical scoping.
                                               ## getinv() is accessed from the list of functions returned by makeCacheMatrix with the $ operator, so it's using values from the makeCacheMatrix enclosing environment.
        if(!is.null(inv)) {                    ## the if/else statement checks if there is a cached value for inv. 
                                               ##If this is the first time cacheSolve() was called for x, then there will be no cached value and
                                               ## inv will be NULL. If cacheSolve has been called for x, then setinv() will have stored it as inv and cacheSolve() will return that cached value.
                
                message("getting cached data")  ## returns a message to the user that R is retrieving stored value for inv rather than calculating it
                return(inv)                     ## returns inv value from cache
        }
        data <- x$get()                        ## this is the else part of the if/else statement. If inv was NULL because there was no stored value for inv (or it was invalidated by set())
                                               ## then the function calculates the inverse by retrieving the vector, x, from makeCacheMatrix environment with x$get(), 
                                               ## which calls the function get() from the list returned by makeCacheMatrix to retrieve the matrix, x
        inv <- solve(data,...)                 ## and calls solve() on the matrix, x, returned by get(), with additional, if any, parameters from cacheSolve() 
        x$setinv(inv)                          ## then stores the value for the inverse returned by solve()
        inv                                    ## and finally returns the calcuated inv
}
