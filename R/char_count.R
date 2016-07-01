#' Characters count
#' @description Calculates character frequencies in a vector.
#' @param x character vector, or a list that can be unlisted to a character vector.
#' @return data.frame with two columns: \code{char} - character and \code{count} -
#' number of it's occurencies.
#'
#' @export
#'
#' @examples
#' char_count("character")
#' char_count(words.eng)
char_count <- function(x){
    chars <- x %>% unlist()
    if (!is.character(chars)) stop("Input must a character vector or a list that can be unlisted to a character vector.")
    chars %<>% strsplit("") %>% unlist()
    if(length(chars) == 0) return(data.frame(char = character(0),
                                             count = integer(0)))
    chars %>% table %>% as.data.frame(stringsAsFactors = F) %>%
        `names<-`(c('char','count'))
}
