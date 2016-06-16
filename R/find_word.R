find_word <- function(model = "*", allow = letters, ban = character(0),
                      type = "usual", words = words.eng){
    grep(model_to_regex(model, allow, ban, type), words, value = T, perl = T)
}

find_word_l <- function(model = "*", allow = letters, ban = character(0),
                      type = "usual", words = words.eng){
    grepl(model_to_regex(model, allow, ban, type), words, perl = T)
}



scrabble <- function(allow, model = "*", ban = character(0), words = words.eng){
    find_word(model = model, allow = allow, ban = ban, type = "scrabble", words = words)
}

anagram <- function(allow, model = "*", words = words.eng){
    find_word(model = model, allow = allow, type = "anagram", words = words)
}

