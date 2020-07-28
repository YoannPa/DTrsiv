
#' Check if any column contains exclusively NAs and returns colnames if any.
#'
#' @param data An \code{array}, a \code{dataframe} or a \code{matrix}.
#' @return A \code{list} with 'fullNA.col' containing the column names that contain
#'         exclusively NAs, and 'is.fullNA' a logical TRUE if the array has at
#'         least 1 column contaning exclusively NAs, FALSE if it has none.
#' @author Yoann Pageaud.
#' @export

allNA.col <- function(data){
  is.fullNA <-
    apply(X = apply(X = data, MARGIN = 2, FUN = is.na), MARGIN = 2, FUN = all)
  fullNA <- names(is.fullNA[is.fullNA == TRUE])
  if(length(unique(is.fullNA)) == 2){
    warning(paste("Columns", paste(
      paste0("'", fullNA, "'"), collapse = ", "), "are empty!"))
    res <- list("fullNA.col" = fullNA, "is.fullNA" = TRUE)
  } else if(length(unique(is.fullNA)) > 2){
    stop("Some columns could not be analyzed correctly")
  } else if(length(unique(is.fullNA)) == 1){
    cat("No columns containing exclusively NAs could be found.\n")
    res <- list("fullNA.col" = fullNA, "is.fullNA" = FALSE)
  }
  return(res)
}
