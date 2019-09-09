text_eval <- function(a) {
    if (class(a) != "character") {
        return(invisible())
    }
    eval(parse(text = a))
}
