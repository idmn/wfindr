find_word <- function(model = "*", allow = letters, ban = character(0),
                      type = "usual", words = words.eng){
    grep(model_to_regex(model, allow, ban, type), words, value = T, perl = T)
}

scrabble <- function(x, model = "*"){
    find_word(model = model, allow = x, type = "scrabble")
}

anagram <- function(x){
    find_word(allow = x, type = "anagram")
}

