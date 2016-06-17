#' Characters count
#'
#' @param x - character vector
#' @return data.frame with two columns: charater and number of its occurencies.
#'
#' @export
#'
#' @examples
#' char_count("character")
#' char_count(words.eng)
char_count <- function(x){
    chars <- x %>% unlist() %>% strsplit("") %>% unlist()
    if(length(chars) == 0) return(data.frame(char = character(0),
                                             count = integer(0)))
    chars %>% table %>% as.data.frame(stringsAsFactors = F) %>%
        `names<-`(c('char','count'))
}
