ext <- function(file) {
 stringr::str_extract(file, "(?<=\\.)[[:alnum:]]+$")
}

load_internal_file <- function(name, stratify_by = NULL) {
  system.file(name, strata[[stratify_by]], package = "bbsBayes") %>%
    readr::read_csv(show_col_types = FALSE, progress = FALSE)
