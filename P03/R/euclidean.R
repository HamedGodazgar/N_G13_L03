
#' @title Greatest Common Divisor of two numbers.
#' @author Hamed
#' @export euclidean
#' @name euclidean
#' @title euclidean
#' @param num1 A number.
#' @param num2 A number.
#' @description Find the Greatest Common Divisor of two given numbers
#' @return The Greatest Common Divisor 
#' @references {https://en.wikipedia.org/wiki/Euclidean_algorithm}
#' @details Should assert that the arguments are numeric scalars or integers.
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)

euclidean <- function(num1,num2){
  num1 <- abs(num1)
  num2 <- abs(num2)
  while (num1!=num2) {
    if (num1>num2) {
      num1 <- num1 - num2
    }else{num2 <- num2 - num1}
  }
  return(num1)
}








