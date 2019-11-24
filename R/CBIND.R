#' Combine two or more data.frame, tibble obects and vectors by column adding new rows and columns if necesary and filling missing values with NA
#'
#' @param df1 first data.frame or matrix to be join
#' @param df2 second data.frame or matrix to be join
#'
#' @return New data.frame with merged cells
#' @export
#'
#'
#' @importFrom tibble as_tibble
#'
#' @examples
#'
#' data(iris)
#' d_1 <- iris[, 5]
#' df2 <- d_1
#' df1 <- rbind(iris, iris)
#' df3<-CBIND(df1, df2, new.col=FALSE)
#' df3
#' dim(df3)
#'
CBIND <- function(df1, df2,...) {
    is_df <- sapply(list(df1, df2), is.data.frame)
    if (any(is_df)) {
        df1 <- tibble::as_tibble(df1)
        df2 <- tibble::as_tibble(df2)
    }
    nd1 <- nrow(df1)
    nd2 <- nrow(df2)
    if (nd1 == nd2) {
        return(cbind(df1, df2,...))
    }
    else {
        mxm <- c(nd1, nd2) != max(c(nd1, nd2))
        dmx <- list(df1, df2)[mxm][[1]]
        dmn <- list(df1, df2)[!mxm][[1]]
        df_rw <- c(nd1, nd2)[mxm] - c(nd1, nd2)[!mxm]
        dmx <- rbind(dmx, tibble::as_tibble(matrix(NA, -df_rw, ncol(dmx), dimnames = list(rep(NA, -df_rw), names(dmx)))))
        if (is_df[1]) {
            return(cbind(dmn, dmx,...))
        }
        else {
            return(cbind(dmx, dmn,...))
        }
    }
}
