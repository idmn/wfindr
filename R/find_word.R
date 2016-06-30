#' Find words that fit the chosen parameters.
#' @description Uses regex constructed by \code{\link{model_to_regex}} to search
#' words. By default the search is done among \code{\link{words.eng}}.\cr
#' \code{find_word} returns a vector of found words, \code{find_word_l} returns
#' a logical vector that can be used for subsetting.
#'
#' @inheritParams model_to_regex
#'
#' @param words vector of words to search within. By default is set to
#' \code{\link{words.eng}}.
#' @seealso \code{\link{scrabble}}, \code{\link{anagram}}
#' @export
find_word <- function(model = "*", allow = letters, ban = character(0),
                      type = "usual", words = words.eng){
    grep(model_to_regex(model, allow, ban, type), words, value = T, perl = T)
}

#' @rdname find_word
#' @export
find_word_l <- function(model = "*", allow = letters, ban = character(0),
                        type = "usual", words = words.eng){
    grepl(model_to_regex(model, allow, ban, type), words, perl = T)
}


#' Find words that can be constructed from the specified letters
#' @description \code{scrabble} function finds the words that can be constructed
#' from the specified set of letters. \cr
#' \code{anagram} function finds the words that are permutations of the specified
#' set of letters. Usually this set of letters is a word itself.\cr
#' \code{scrabble} and \code{anagram} are functions built on top of
#' the \code{\link{find_word}} function with parameter \code{type} set to \code{"scrabble"}
#' or \code{"anagram"} respectively and \code{allow} parameter moved to the first place
#' to simplify the usage (see examples).
#' @seealso \code{\link{find_word}}
#' @export
scrabble <- function(allow, model = "*", ban = character(0), words = words.eng){
    find_word(model = model, allow = allow, ban = ban, type = "scrabble", words = words)
}

#' @rdname scrabble
#' @export
anagram <- function(allow, model = "*", ban = character(0), words = words.eng){
    find_word(model = model, allow = allow, type = "anagram", words = words)
}











