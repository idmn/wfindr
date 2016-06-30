#' Build a regular expression to fit chosen parameters
#' @param model pattern that a word should match. Consists of letters and
#' unknown characters specifications. Dot \code{.} stands for unknown
#' character. It may be followed by \{...\} repetition quantifier
#' (i.e. .\{n\}, .\{n,\}, .\{n,m\}). Asterisk \code{*} stands for unknown number of
#' unknown characters. See examples.\cr
#' By default \code{model} is set to "*".
#' @param allow characters allowed to fill gaps in a word. Can be listed in a
#' single string or in a vector. By default is set to \code{letters}.
#' @param ban characters not allowed to fill gaps in a word.
#' @param type can be \code{"usual"}, \code{"scrabble"}, or \code{"anagram"}.
#' Abbreviated input is allowed: e.g. \code{"u"}, \code{"s"}, or \code{"a"}.\cr
#' \code{type} defines how often allowed characters can be used to fill the gaps.
#' Say, character appears \code{n} times in \code{allow} and
#' \code{m} times in \code{ban}. If \code{d = n - m} is less or equal to zero,
#' whatever the \code{type} is, this character won't be used to fill the gaps.
#' Suppose \code{d > 0}.\cr
#' \itemize{
#'  \item If \code{type} is \code{"usual"} then the character is allowed to fill the
#'  gaps \strong{unlimited} number of times. \cr
#'  \item If \code{type} is \code{"scrabble"} then the character is allowed to
#'  fill the gaps \strong{no more} than d times.
#'  \item If \code{type} is \code{"anagram"} then the character should be used
#'  \strong{exactly} d times.
#' }
#'
#' @details
#'
#' @section Warning:
#' If \code{type = "scrabble"} or \code{"anagram"}, output regex will contain
#' perl-like syntax. So, to use it in \code{grep} or \code{gsub} for example,
#' set \code{perl} parameter to \code{TRUE}.
#'
#' @examples
#' ## Regular expression to match all the 5-letter words starting with "c".
#' model_to_regex("c.{4}")
#' ## Disallow "a" and "b".
#' model_to_regex("c.{4}", ban = "ab")
#' ## Allow only "a" and "b" to fill the gap.
#' model_to_regex("c.{4}", allow = "ab")
#' ## Allow "a", "b", and "c", but then ban "c" (result is the same as the previous example)
#' model_to_regex("c.{4}", allow = "abc", ban = "c")
#'
#' ## Regex to match all words that start with "p" and end with "zed".
#' model_to_regex("p*zed")
#'
#' ## Regex to match all the words that can be constructed of the word "thing".
#' model_to_regex(allow = "thing", type = "scrabble")
#' ## Get at lest 4-letter words.
#' model_to_regex(".{4,}", allow = "thing", type = "scrabble")
#'
#' ## Regex to match anagrams of the word "thing"
#' model_to_regex(allow = "thing", type = "anagram")
#'
#' @seealso \code{\link{find_word}} \code{\link{scrabble}} \code{\link{anagram}}
#'
#' @export
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




