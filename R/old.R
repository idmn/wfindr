# tries to turn input to a regex of type
# [<list of characters>]
toCharListRegex <- function(x){
    x <- as.character(x)
    x <- strsplit(x, "") %>%
        unlist() %>%
        unique() %>%
        sort() %>%
        paste(collapse = "")
    if (x == "") return(x)
    return(paste0("[", x, "]"))
}

# turns model with specified allowed and banned characters to
# a single regex
modelToRegex <- function(model = "*", allow = lttrs, ban = "",
                         mode = "usual"){
    allow %<>% toCharListRegex()
    ban %<>% toCharListRegex()
    charListRegex <- gsub(ban, "", allow)

    ##check if model is valid

    model %>%
        gsub("\\.", charListRegex, .) %>%
        gsub("\\*", paste0(charListRegex,"*"), .) %>%
        paste0("^", ., "$")
}
