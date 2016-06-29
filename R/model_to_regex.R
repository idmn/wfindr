#' Build a regular expression to fit chosen parameters
#' @param model - pattern that a word should match. Consists of letters and characters
#' denoting unknown characters. Dot \code{.} stands for unknown
#' character. It may be followed by \{...\} repetition quantifier
#'  (i.e. .\{n\}, .\{n,\}, .\{n,m\}). Asterisk \code{*} stands for unknown number of
#'  unknown characters.
#' @param allow - characters allowed to fill gaps in a word. Can be listed in a
#' single string or in a vector. By default is set to \code{letters}.
#' @param ban - characters not allowed to fill gaps in a word.
#' @param type - can be \code{"usual"}, \code{"scrabble"}, or \code{"anagram"}.
#' Abbreviated input is allowed: e.g. \code{"u"}, \code{"s"},
#'  or \code{"a"}. See details. (?)
#'
#' @details
#'
#'
#' @export
#' @examples
#' model_to_regex("w..d")
#' model_to_regex("p.{2,3}", ban = "aorp")
#' model_to_regex(allow = "thing", type = "scrabble")
#' model_to_regex(allow = "thing", type = "anagram")
model_to_regex <- function(model = "*", allow = letters, ban = character(0),
                         type = "usual"){
    check_model(model)
    type <- match.arg(type, c("usual", "scrabble", "anagram"))

    allow.count <- char_count(allow)
    ban.count <- char_count(ban)
    names(allow.count) <- c("char", "allow")
    names(ban.count) <- c("char", "ban")
    char.spec <- merge(allow.count, ban.count, all.x = T)
    char.spec[is.na(char.spec)] <- 0

    if (type == "usual"){
        char.spec %<>% dplyr::mutate(max = allow & !ban)
    }else{
        char.spec %<>% dplyr::mutate(max = pmax(allow - ban, 0))
    }
    char.spec %<>% dplyr::select(char, max)
    char.spec %<>% dplyr::filter(max > 0)

    chars.r <- paste(char.spec$char, collapse = "") %>%
        paste0("[", ., "]")
    regex <- model %>%
        gsub("\\.", chars.r, .) %>%
        gsub("\\*", paste0(chars.r,"*"), .) %>%
        paste0("^", ., "$")
    if (type != "usual"){
        model.char.spec <- char_count(model) %>%
            filter(!grepl("\\*|\\.|,|\\{|\\}|\\d",char))
        names(model.char.spec) <- c("char", "max")
        char.spec %<>% rbind(model.char.spec) %>%
            group_by(char) %>%
            summarise(max = sum(max)) %>%
            ungroup()
        char.spec %<>% dplyr::mutate(
            look.ahead = paste0("([^", char, "]*", char, "[^", char, "]*)"))
    }
    switch (type,
        scrabble = {
            char.spec %<>% dplyr::mutate(
                look.ahead = paste0("(?=^((", look.ahead, "{", 1, ",", max,
                                    "})|([^", char, "]*))$)"))
        },
        anagram = {
            char.spec %<>% dplyr::mutate(
                look.ahead = paste0("(?=^", look.ahead, "{", max, "}$)"))
        }
    )
    regex <- paste(char.spec$look.ahead, collapse = "") %>% paste0(regex)
    regex
}

check_model <- function(model){
    model.msg <- "Wrong model. Model should be a single string with following characters allowed:
    '.' possibly followed by {...} repetition quantifier, lowercase letters and '*'."
    model.regex <- "^([a-z]|\\*|(\\.(\\{[0-9]*,?[0-9]*\\})?))*$"
    if(!is.character(model)) stop(model.msg)
    if(length(model)!=1)     stop(model.msg)
    if(!grepl(model.regex, model)) stop(model.msg)
}




