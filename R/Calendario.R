#' Title
#'
#' @param ts time series object
#' @param ma_af Mobile Average model parammeter
#'
#' @return
#' @export
#'
#' @examples
#'
#' dates<-Calendario(USAccDeaths)
#'
#'
Calendario <- function(ts, ma_af = 4) {
    if(!"ts" %in% class(ts)){
        warning("ts argument must be a time series")
        return(invisible())
    }
    Dates<-time(ts)
    #Value<-as.numeric(ts)
    PP <- try(periodicidad(frq[[2]]), silent = TRUE)
    if (inherits(PP, "try-error")) {
        PP <- 7
    }
    Periodo <- round(PP[[1]], 0)[1]
    names(frq)[sapply(frq, is.numeric)] <- "Valor"
    names(frq)[!sapply(frq, is.numeric)] <- "Fecha"
    frq[["Fecha.0"]] <- frq[["Fecha"]] - as.numeric(frq[["Fecha"]])%%Periodo
    frqpv <- reshape2::dcast(frq, Fecha.0 ~ ., sum, value.var = "Valor")
    frqpv[["Div_est"]] <- row.names(frqpv)
    frqpv[["mensual"]] <- data.table::month(frqpv[["Fecha.0"]])
    frqpv[["anual"]] <- data.table::year(frqpv[["Fecha.0"]])
    ansdm <- c()
    for (k in 0:(nrow(frqpv) - 1)) {
        ansdm <- c(ansdm, mean(frqpv[["."]][(1 + k):min(ma_af + k, nrow(frqpv))]))
    }
    frqpv[["MA_n"]] <- ansdm
    kcl <- kmeans(frqpv[["MA_n"]], min(3, length(unique(ansdm))))
    frqpv[["Clu_n"]] <- kcl$cluster
    clcntrs <- as.data.frame(kcl$centers)
    Clu_cont_n <- ifelse(kcl$cluster == order(clcntrs[[1]])[1], 1, ifelse(kcl$cluster == order(clcntrs[[1]])[2], 2, 3))
    frqpv$Clu_n <- Clu_cont_n
    ansdm <- c()
    for (k in 0:(nrow(frqpv) - 1)) {
        ansdm <- c(ansdm, mean(Clu_cont_n[(1 + k):min(ma_af + k, nrow(frqpv))]))
    }
    frqpv[["Clu_cont_n"]] <- ansdm
    return(frqpv)
}
