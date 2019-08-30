#' Shows all relevant information of a numeric vector
#'
#' @param b
#' @param fit
#' @param p.value
#' @param anch
#'
#' @return
#' @export
#'
#' @examples
#'
#' library(moments)
#'
estad <- function(b, anch = 100) {
    n1 <- length(b)
    b <- na.omit(b)
    if (!is.numeric(b)) {
        b <- as.character(b)
        bb <- as.numeric(gsub(",", "", b))
        b <- bb
    }
    if (length(b) < anch) {
        fit <- "No hay suficientes datos o muchos NAs"
        p.value <- "No hay suficientes datos o muchos NAs"
    }
    if (n1 > 0) {
        if (((n1 - length(b))/n1) > 0.15) {
            fit <- "No hay suficientes datos o muchos NAs"
            p.value <- "No hay suficientes datos o muchos NAs"
        }
    }
    if (is.null(fit) | is.null(p.value)) {
        fit = ajustar(b, mus = 1)[[1]][1]
        p.value = ajustar(b, mus = 1)[[2]][1]
    }
    if (length(b) == 0) {
        b <- NA
    }
    est1<-as.list(quantile(b, na.rm = TRUE))
    histo<-hist(b,breaks = 50)
    est2<-list(mean = mean(b, na.rm = T),
               sd = sd(b, na.rm = T),
               len = length(b),
               nas = n1 - length(b),
               mode=histo$breaks[histo$density==max(histo$density)],
               kurtosis=moments::kurtosis(b),
               skewness=moments::skewness(b),
               bimodal_coef=,
               interquantile_rangue=est1[[2]]-est1[[4]],
               dist = as.character(fit),
               unique.values = ,
               rpart.cuts = ,
               max.likelihood = ,
               dist.p.value = as.character(p.value))
    est3<-t.test(b)
    est3<-list(conf.int.inf=est3$conf.int[1],conf.int.sup=est3$conf.int[2],
               t.statistic=est3$statistic,
               t.p.vlaue=est3$p.value)
}
