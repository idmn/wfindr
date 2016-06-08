# (?=^([^a]*a[^a]*){3}$)(?=^([^b]*b[^b]*){3}$)pa.*

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

charCount <- function(x){
    chars <- x %>% unlist() %>% strsplit("") %>% unlist()
    if(length(chars) == 0) return(data.frame(char = character(0),
                                             count = integer(0)))

    chars %>% table %>% as.data.frame() %>% `names<-`(c('char','count'))
}

f <- function(allow = letters, ban = character(0), mode = "usual"){
    allow %<>% charCount()
    ban %<>% charCount()
    names(allow) <- c("char", "allow.count")
    names(ban) <- c("char", "ban.count")
    allow_ban <- merge(allow, ban, all.x = T)
    allow_ban[is.na(allow_ban)] <- 0
    switch(mode,
           "usual" =
           )
    allow_ban

}



