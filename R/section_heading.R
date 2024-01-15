# Obnoxious section headings:
library(stringr)


title_head <- function(title){

  formatted <- title %>%
    str_to_upper() %>%
    str_replace_all("(.{1})", "\\1 ") %>% # insert spaces after every two digits
    str_trim() %>%
    paste("\n#    ", ., "    #\n", sep = "")

  cat(rep("#", (nchar(formatted)-2)), sep = "")
  cat(paste("\n#", paste(rep(" ", nchar(formatted)-4), collapse = ""), "#", sep = ""))
  cat(formatted)
  cat(paste("#", paste(rep(" ", nchar(formatted)-4), collapse = ""), "#\n", sep = ""))
  cat(rep("#", (nchar(formatted)-2)), sep = "")

}


section_head <- function(title){

  formatted <- title %>%
    str_to_upper() %>%
    str_replace_all("(.{1})", "\\1 ") %>% # insert spaces after every two digits
    str_trim() %>%
    paste("\n#    ", ., "    #\n", sep = "")

  cat(rep("#", (nchar(formatted)-2)), sep = "")
  cat(formatted)
  cat(rep("#", (nchar(formatted)-2)), sep = "")

}
