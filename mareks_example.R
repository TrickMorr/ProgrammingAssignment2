new_counter <- function() {
        i <- 0
        function() {
                i <<- i +1
                function() {
                        i <<- i + 1
                }
        }
}


newcounter <- function() {
        i <- 0
        i <- i +1
        i
}