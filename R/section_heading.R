# Obnoxious section headings:

title_head <- function(title){

  formatted <- title %>%
    stringr::str_to_upper() %>%
    stringr::str_replace_all("(.{1})", "\\1 ") %>% # insert spaces after every two digits
    stringr::str_trim()

  formatted <- paste("\n#    ", formatted, "    #\n", sep = "")



  cat(rep("#", (nchar(formatted)-2)), sep = "")
  cat(paste("\n#", paste(rep(" ", nchar(formatted)-4), collapse = ""), "#", sep = ""))
  cat(formatted)
  cat(paste("#", paste(rep(" ", nchar(formatted)-4), collapse = ""), "#\n", sep = ""))
  cat(rep("#", (nchar(formatted)-2)), sep = "")

}


section_head <- function(title){

  formatted <- title %>%
    stringr::str_to_upper() %>%
    stringr::str_replace_all("(.{1})", "\\1 ") %>% # insert spaces after every two digits
    stringr::str_trim()

  formatted <- paste("\n#    ", formatted, "    #\n", sep = "")

  cat(rep("#", (nchar(formatted)-2)), sep = "")
  cat(formatted)
  cat(rep("#", (nchar(formatted)-2)), sep = "")

}
