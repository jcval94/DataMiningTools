#' Title
#'
#' @param df data.frame to be split
#' @param split percentage of the train chunk
#'
#' @return a list and ...
#' @export
#'
#' @examples
#'
#' data(iris)
#' set.seed(31109)
#' TrainTestSplit(iris)
#'
#' head(train);dim(train)
#'
#' head(test);dim(test)
#'
TrainTestSplit <- function(df, split = 0.75) {
    if (!any("data.frame" %in% class(df))) {
        exit("must be a data.frame object")
    }
    tr <- sample(1:nrow(df),floor(nrow(df)*split))
    assign("train",df[tr, ],envir = globalenv())
    assign("test",df[-tr, ],envir = globalenv())
    list(df[tr, ], df[-tr, ])
}
