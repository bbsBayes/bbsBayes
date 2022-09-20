
check_model <- function(model) {
  if(is.null(model)) stop("No model specified", call. = FALSE)
  model <- tolower(model)
  m <- c("slope", "firstdiff", "gam", "gamye")
  if(!model %in% m) {
    stop("Invalid model specified. Must be one of ", paste0(m, collapse = ", "),
         call. = FALSE)
  }
}

check_basis <- function(basis) {
  if(is.null(basis)) stop("No basis specified", call. = FALSE)
  basis <- tolower(basis)
  b <- c("original", "mgcv")
  if(!basis %in% b) {
    stop("Invalid basis specified. Must be one of ", paste0(b, collapse = ", "),
         call. = FALSE)
  }
}
