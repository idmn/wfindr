char_count <- function(x){
    chars <- x %>% unlist() %>% strsplit("") %>% unlist()
    if(length(chars) == 0) return(data.frame(char = character(0),
                                             count = integer(0)))

    chars %>% table %>% as.data.frame(stringsAsFactors = F) %>%
        `names<-`(c('char','count'))
}
