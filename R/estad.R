#' Shows all relevant information of a numeric vector
#'
#' @param b numeric sample
#' @param p.value
#' @param anch
#' @param ... internal functions parammeter control
#'
#' @return
#' @export
#'
#' @examples
#'
#' library(moments)
#' b<-rnorm(100)
#'
estad <- function(b, anch = 100,...) {
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
    Db<-density(b)
    Un<-unique(b)
    Kb<-kmeans(sort(b),2)
    est2<-list(mean = mean(b, na.rm = T),
               sd = sd(b, na.rm = T),
               len = length(b),
               nas = n1 - length(b),
               mode=histo$breaks[histo$density==max(histo$density)],
               kurtosis=moments::kurtosis(b),
               skewness=moments::skewness(b),
               bimodal_coef= modes::bimodality_coefficient(b),
               interquantile_rangue=est1[[2]]-est1[[4]],
               dist = as.character(fit),
               Discrete = all(floor(b)==b),
               count.values = table(b),
               kmeans.centers = Kb$centers,
               kmeans2split = mean(Kb$centers),
               max.likelihood = Db[["x"]][Db[["y"]]==max(Db[["y"]])],
               dist.p.value = as.character(p.value))
    est3<-t.test(b)
    est3<-list(conf.int.inf=est3$conf.int[1],conf.int.sup=est3$conf.int[2],
               t.statistic=est3$statistic,
               t.p.vlaue=est3$p.value)
}
