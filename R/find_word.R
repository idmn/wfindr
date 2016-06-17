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
scrabble <- function(x, model = "*"){
    find_word(model = model, allow = x, type = "scrabble")
}

#' Find anagrams ...
#' @export
#' @examples
#' anagram("thing")
anagram <- function(x){
    find_word(allow = x, type = "anagram")
}

