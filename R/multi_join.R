#' Applies left
#'
#' @param list_dfs
#' @param join_type
#' @param by
#' @param ...
#'
#' @return
#' @export
#'
#' @importFrom dplyr inner_join
#' @importFrom dplyr left_join
#' @importFrom dplyr right_join
#' @importFrom dplyr full_join
#' @importFrom dplyr semi_join
#' @importFrom dplyr nest_join
#' @importFrom dplyr anti_join
#'
#' @examples
#'
#' multi_join(list_dfs=list(band_members,band_instruments),join_type="nest")
#'
#'
multi_join <- function(list_dfs, join_type = "left", by, ...) {
    join_fun <- get(paste0(join_type, "_join"))
    Apl_jn <- function(df1, df2) {
        join_fun(df1, df2, by)
    }
    if(missing(by)){
        names_<-purrr::map(list_dfs,names)
        sv<-c()
        for (i in 1:(length(names_)-1)) {
            for(j in 1:length(names_[[1]])){
                try(do.call("c",names_[[-1]]),silent = T)
                #concluir después
                if(names_[[i]][j] %in% names_[[i+1]]){
                    sv<-c(sv,names_[[i]][j])
                }
            }
        }
    }
    for (kt in 1:(length(list_dfs) - 1)) {
        if (kt == 1) {
            df_jn <- list_dfs[[kt]]
        }
        df_jn <- Apl_jn(df_jn, list_dfs[[kt + 1]])
    }
    df_jn
}
