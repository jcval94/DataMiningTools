#' Transpose data.frame object
#'
#' @param df data.frame or tibble object
#'
#' @return transposed data.frame
#' @export
#'
#' @importFrom dplyr as_tibble
#'
#' @examples
#'
#' data(iris)
#' Transpose <- dft(iris)
#'
#' View(Transpose)
#'
#'
dft <- function(df) {
    cl<-class(df)
    if(! "data.frame" %in% cl){warning("object must be a data.frame")
        return(invisible())
        }
    if (cl %in% "tibble") {
        Tib <- TRUE
    }
    else {
        Tib <- FALSE
    }
    df_1 <- as.data.frame(t(df))
    names(df_1) <- row.names(df)
    if (Tib) {
        return(dplyr::as_tibble(df_1))
    }
    else {
        return(df_1)
    }
}
