ext <- function(file) {
 stringr::str_extract(file, "(?<=\\.)[[:alnum:]]+$")
}
