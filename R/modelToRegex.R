# (?=^([^a]*a[^a]*){3}$)(?=^([^b]*b[^b]*){3}$)pa.*
model_to_regex <- function(model = "*", allow = letters, ban = character(0),
                         type = "usual"){
    # check model validity
    model.msg <- "Wrong model. Model should be a string. Allowed characters are '.', '*' and lower case letters."
    model.allowed <- c(letters, ".", "*")
    if(!is.character(model)) stop(model.msg)
    if(length(model)!=1)     stop(model.msg)
    chars <- strsplit(model, "") %>% unlist() %>% unique()
    if (sum(!(chars %in% model.allowed)) > 0) stop(model.msg)
    # check mode validity
    type <- match.arg(type, c("usual", "scrabble", "anagram"))

    allow %<>% char_count()
    ban %<>% char_count()
    names(allow) <- c("char", "allow")
    names(ban) <- c("char", "ban")
    char.spec <- merge(allow, ban, all.x = T)
    char.spec[is.na(char.spec)] <- 0

    if (type == "usual"){
        char.spec %<>% mutate(max = allow & !ban)
    }else{
        char.spec %<>% mutate(max = pmax(allow - ban, 0))
    }
    char.spec %<>% select(char, max)
    char.spec %<>% filter(max > 0)

    chars.r <- paste(char.spec$char, collapse = "") %>%
        paste0("[", ., "]")

    regex <- model %>%
        gsub("\\.", chars.r, .) %>%
        gsub("\\*", paste0(chars.r,"*"), .) %>%
        paste0("^", ., "$")

    if (type != "usual"){
        char.spec %<>% mutate(
            look.ahead = paste0("([^", char, "]*", char, "[^", char, "]*)"))
    }
    switch (type,
        scrabble = {
            char.spec %<>% mutate(
                look.ahead = paste0("(?=^((", look.ahead, "{", 1, ",", max, "})|([^",
                                    char, "]*))$)"))
        },
        anagram = {
            char.spec %<>% mutate(
                look.ahead = paste0("(?=^", look.ahead, "{", max, "}$)"))
        }
    )

    regex <- paste(char.spec$look.ahead, collapse = "") %>% paste0(regex)
    regex
}
