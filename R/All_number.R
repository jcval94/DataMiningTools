#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @importFrom purrr map
#' @importFrom dplyr as_tibble
#'
#' @examples
all_number <- function(df) {
    df <- as_tibble(df)
    classes <- purrr::map_chr(df, class)
    nnm <- !classes %in% "numeric"
    no_num <- df[, names(df)[nnm]]
    no_mun_2 <- categorizar(no_num, ALL = F)
    df_n <- cbind(df[, !nnm], no_mun_2)
    df_n
}
