#' Find words that fit the chosen parameters.
#' @description Uses regex constructed by \code{\link{model_to_regex}} to search
#' words. By default search is done among \code{\link{words.eng}}.
#'
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
#' @param words vector of words to search within. By default is set to
#' \code{\link{words.eng}}.
#'
#' @return Character vector of found words.
#'
#' @export
find_word <- function(model = "*", allow = letters, ban = character(0),
                      type = "usual", words = words.eng){
    grep(model_to_regex(model, allow, ban, type), words, value = T, perl = T)
}

#' @describeIn find_word ada
#' @export
find_word_l <- function(model = "*", allow = letters, ban = character(0),
                        type = "usual", words = words.eng){
    grepl(model_to_regex(model, allow, ban, type), words, perl = T)
}


#' Find words that can be constructed from the specified letters
#' @export
#' @examples
#' scrabble("abll")
scrabble <- function(allow, model = "*", ban = character(0), words = words.eng){
    find_word(model = model, allow = allow, ban = ban, type = "scrabble", words = words)
}

#' Find anagrams ...
#' @export
#' @examples
#' anagram("thing")
anagram <- function(allow, model = "*", words = words.eng){
    find_word(model = model, allow = allow, type = "anagram", words = words)
}











