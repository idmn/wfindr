#' Find words that fit the chosen parameters
#' @export
#' @examples
#' find_word("w..d")
#' find_word(allow = "thing", type = "scrabble")
#' find_word(allow = "thing", type = "anagram")
find_word <- function(model = "*", allow = letters, ban = character(0),
                      type = "usual", words = words.eng){
    grep(model_to_regex(model, allow, ban, type), words, value = T, perl = T)
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

find_word_l <- function(model = "*", allow = letters, ban = character(0),
                      type = "usual", words = words.eng){
    grepl(model_to_regex(model, allow, ban, type), words, perl = T)
}








