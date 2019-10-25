#' Drop rows/columns whose NA percentage be greater than max_na
#'
#' @param df data.frame object
#' @param axis 0 or 1, if 1 drop columns
#' @param max_na maximum number of NAs allowed per column
#'
#' @return
#' @export
#'
#' @examples
#'
#' reducir.df.na(iris, max_na=0.5)
#'
reducir.df.na <- function(df,axis=1, max_na=0.05) {
  acond<-axis==0
  if(acond){
    df<-dft(df)
  }
    if (max_na > 1 | max_na < 0) {
        return(NULL)
    }
  if(acond){
    return(dft(df[, purrr::map_lgl(df, ~length(.x[is.na(.x)])/length(.x) < max_na)]))
  }else{
    return(df[, purrr::map_lgl(df, ~length(.x[is.na(.x)])/length(.x) < max_na)])
  }
}



