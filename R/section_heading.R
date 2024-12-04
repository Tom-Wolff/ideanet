# Obnoxious section headings:

title_head <- function(title){

  formatted <- title %>%
    stringr::str_to_upper() %>%
    stringr::str_replace_all("(.{1})", "\\1 ") %>% # insert spaces after every two digits
    stringr::str_trim()

  formatted <- paste("\n#    ", formatted, "    #\n", sep = "")

message(paste0(paste0(rep("#", (nchar(formatted)-2)), collapse = ""),
  paste("\n#", paste(rep(" ", nchar(formatted)-4), collapse = ""), "#", sep = ""),
  formatted,
  paste("#", paste(rep(" ", nchar(formatted)-4), collapse = ""), "#\n", sep = ""),
  paste0(rep("#", (nchar(formatted)-2)), collapse = ""),
  collapse = ""))

}


section_head <- function(title){

  formatted <- title %>%
    stringr::str_to_upper() %>%
    stringr::str_replace_all("(.{1})", "\\1 ") %>% # insert spaces after every two digits
    stringr::str_trim()

  formatted <- paste("\n#    ", formatted, "    #\n", sep = "")

  message(paste0(paste0(rep("#", (nchar(formatted)-2)), collapse = ""),
                 formatted,
                 paste0(rep("#", (nchar(formatted)-2)), collapse = "")),
          collapse = "")

}
