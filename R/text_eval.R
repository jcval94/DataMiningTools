#' Evaluates an string and compile it
#'
#' @param a a string containing R code
#'
#' @return the string evaluated
#' @export
#'
#' @examples
#'
#' #A sum
#' text_eval("1+1")
#'
#' #Assign random numbers into a variable
#' set.seed(31109)
#' text_eval("A<-runif(30);A")
#'
#' #Plot an histogram
#' text_eval("hist(rnorm(30))")
text_eval <- function(a) {
    if (class(a) != "character") {
        return(invisible())
    }
    eval(parse(text = a))
}
