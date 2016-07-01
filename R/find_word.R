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
#'
#' @examples
#' ## Search 4-letter words starting with "c".
#' find_word("c.{3}")
#' ## Disallow "a" and "b".
#' find_word("c.{3}", ban = "ab")
#' ## Allow only "a" and "b" to fill the gap.
#' find_word("c.{3}", allow = "ab")
#' ## Allow "a", "b", and "c", but then ban "c" (result is the same as the previous example)
#' find_word("c.{3}", allow = "abc", ban = "c")
#'
#' ## Find no more than 4-letter words that have "th" bigram
#' library(magrittr)
#' find_word(".{0,4}") %>% find_word("*th*", words = .)
#' ## count words that start with "th"
#' sum(find_word_l("th*"))
#' length(find_word("th*"))
#'
#' ## Find words that can be constructed of the "thing" word's letters.
#' find_word(allow = "thing", type = "scrabble")
#' ## Get at lest 4-letter words.
#' find_word(".{4,}", allow = "thing", type = "scrabble")
#'
#' ## Find anagrams of the word "thing"
#' find_word(allow = "thing", type = "anagram")
#' @seealso \code{\link{scrabble}}, \code{\link{anagram}}
#' @export
find_word <- function(model = "*", allow = letters, ban = character(0),
                      type = "usual", words = wfindr::words.eng){
    grep(model_to_regex(model, allow, ban, type), words, value = T, perl = T)
}

#' @rdname find_word
#' @export
find_word_l <- function(model = "*", allow = letters, ban = character(0),
                        type = "usual", words = wfindr::words.eng){
    grepl(model_to_regex(model, allow, ban, type), words, perl = T)
}


#' Find words that can be constructed from the specified letters
#' @description \code{scrabble} finds words that can be constructed
#' from the specified set of letters. \cr
#' \code{anagram} finds words that are permutations of the specified
#' set of letters. Usually this set of letters is a word itself.
#'
#' @param allow characters allowed to use to construct words.
#' @inheritParams model_to_regex
#' @inheritParams find_word
#' @details \code{scrabble} and \code{anagram} are functions built on top of
#' the \code{\link{find_word}} function with parameter \code{type} set to \code{"scrabble"}
#' or \code{"anagram"} respectively and \code{allow} parameter moved to the first place
#' to simplify usage (see the first example).
#' @seealso \code{\link{find_word}}
#' @examples
#' ## Find all words that can be constructed of the "thing" word letters
#' scrabble("thing")
#' ## same as
#' find_word(allow = "thing", type = "s")
#' ## take at least 4-letter words
#' scrabble("thing", ".{4,}")
#' ## same as
#' find_word(".{4,}", "thing", type = "s")
#'
#' ## Pick 8 random letters and find words that can be constructed of them.
#' library(magrittr)
#' sample(letters, 8, TRUE) %>% list(letters = ., words = scrabble(.))
#'
#' ## Find anagrams of the word "thing"
#' anagram("thing")
#'
#' @export
scrabble <- function(allow, model = "*", ban = character(0), words = wfindr::words.eng){
    find_word(model = model, allow = allow, ban = ban, type = "scrabble", words = words)
}

#' @rdname scrabble
#' @export
anagram <- function(allow, model = "*", ban = character(0), words = wfindr::words.eng){
    find_word(model = model, allow = allow, type = "anagram", words = words)
}
