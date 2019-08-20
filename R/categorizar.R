#' Transform any number of categorical values into dumy variables
#'
#' @param df data.frame object
#' @param vect names od df by defaultlt
#' @param porcent 1.5% by default, represents the minimum percentage for a category
#' @param ALL TRUE by default, If TRUE, the function will return the original data frame merged with the dummy variables, otherwise just return the dummy variables
#' @param disc_value maximum number of categories which a categorical value must have
#'
#' @return
#' @export
#'
#' @examples
#'
#' data(cars)
#'
#' categorizar(cars,vect = "speed")
#'
categorizar <- function(df, vect = names(df), porcent = 0.015, ALL = TRUE, disc_value = 85) {
    df <- as.data.frame(df)
    nombrs_1 <- names(df)
    if(!any(purrr::map_lgl(vect,~.x %in% nombrs_1))){warning(paste0("vect parameter not well defined"));return(invisible())}
    if(0 %in% dim(df[,vect])){warning(paste0("data frame empty"));return(invisible())}
    for (vector in vect) {
        print(vector)
        variable <- df[, vector]
        if (length(unique(variable)) > disc_value) {
            (next)()
        }
        uvp <- unique(variable)
        if (length(uvp) == 2) {
            posicion <- as.character(variable[1])
            df[, paste0(vector, "_", posicion, " & ", uvp[uvp != posicion], "")] <- ifelse(as.character(df[, vector]) == posicion[1], 1, 0)
        }
        else {
            nombres_po <- as.character(unique(variable))
            prp <- names(table(variable))[table(variable)/length(variable) > porcent]
            nombres <- nombres_po[nombres_po %in% prp]
            nomb1 <- paste0(vector, "_", nombres)
            nombres_Otros <- nombres_po[!nombres_po %in% prp]
            for (y in 1:length(nombres)) {
                df[, nomb1[y]] <- ifelse(as.character(df[, vector]) == nombres[y], 1, 0)
            }
            if (length(nombres_Otros) != 0) {
                df[, paste0(vector, "_Otros")] <- 0
                for (y in 1:length(nombres_Otros)) {
                  df[, paste0(vector, "_Otros")] <- c(df[, paste0(vector, "_Otros")] + ifelse(as.character(df[, vector]) == nombres_Otros[y], 1, 0))
                }
            }
        }
    }
    if (ALL) {
        return(df)
    }
    else {
        return(df[, !names(df) %in% nombrs_1])
    }
}
