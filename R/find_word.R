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
#' @description
#' @seealso \code{\link{find_word}}, \code{\link{anagram}}
#' @export
scrabble <- function(allow, model = "*", ban = character(0), words = words.eng){
    find_word(model = model, allow = allow, ban = ban, type = "scrabble", words = words)
}

#' Find anagrams ...
#' @seealso \code{\link{find_word}}, \code{\link{scrabble}}
#' @export
anagram <- function(allow, model = "*", words = words.eng){
    find_word(model = model, allow = allow, type = "anagram", words = words)
}











