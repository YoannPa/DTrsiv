
#' Checks if any column contains exclusively NAs and returns colnames if any.
#'
#' @param data An \code{array}, a \code{dataframe} or a \code{matrix}.
#' @return A \code{list} with 'fullNA.col' containing the column names that
#'         contain exclusively NAs, and 'is.fullNA' a logical TRUE if the array
#'         has at least 1 column contaning exclusively NAs, FALSE if it has
#'         none.
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


#' Looks for the best merging operation(s) between two data.tables trying a set
#' of columns from the second one.
#'
#' @param dt.x  A \code{data.table}.
#' @param dt.y  Another \code{data.table}.
#' @param by.x  A \code{character} specifying a single column name from 'dt.x'.
#' @param try.y                  A \code{character} vector specifying multiple
#'                               column names from 'dt.y' to be tried for the
#'                               merging. If NULL, all columns from 'dt.y' will
#'                               be tried for the merging
#'                               (Default: try.y = NULL). Columns with a
#'                               different type from the one specified in 'by.x'
#'                               will raise an error by default
#'                               (See 'skip.incompatible.type').
#' @param skip.incompatible.type A \code{logical} specifying whether potential
#'                               'dt.y' columns of incompatible type specified
#'                               in 'try.y' should be automatically skipped
#'                               (skip.incompatible.type = TRUE) or not
#'                               (skip.incompatible.type = FALSE).
#' @return A \code{list} containing:
#'          \itemize{
#'           \item{'best.merged.dt': a \code{data.table} resulting of the best
#'                 merging operation if a single merging operation performed
#'                 the best. If multiple merging operations gave best results,
#'                 operation names are given as a \code{character} vector.}
#'           \item{'merging.results': a \code{list} of the merging operation
#'                 results. Each result contains 2 elements:
#'                 \itemize{
#'                  \item{'merge.res': the \code{data.table} resulting from the
#'                        merging operation.}
#'                  \item{'NA.count': an \code{integer} vector giving the number
#'                        of NAs contained in each columns from 'dt.y' after the
#'                        merging.}
#'                 }
#'           }
#'          }
#' @author Yoann Pageaud.
#' @export

best.merged.dt <- function(
  dt.x, dt.y, by.x, try.y = NULL, skip.incompatible.type = FALSE){
  #If no columns specified to for the try, take all columns from dt.y
  if(is.null(try.y)){ try.y <- colnames(dt.y) }
  #Get type of by.x column
  type.by.x <- typeof(dt.x[[by.x]])
  #Check that all columns in try.y have the same type
  dty.type <- dt.y[, .(lapply(X = .SD, FUN = typeof), try.y), .SDcols = try.y]
  if(nrow(dty.type) != nrow(dty.type[V1 == type.by.x])){
    if(skip.incompatible.type){
      warning("Incompatible type columns will be skipped for merging.")
      try.y <- dty.type[V1 == type.by.x]$try.y
    } else {
      stop(paste(
        "Some columns specified in 'try.y' have a different type than the",
        "'by.x' column one. To automatically skip these columns set",
        "'skip.incompatible.type' to TRUE."))
    }
  }
  #Try different merging operations
  list.merged.dt <- lapply(X = try.y, FUN = function(i){
    merged.dt <- merge(x = dt.x, y = dt.y, by.x = by.x, by.y = i, all.x = TRUE)
    count.na.col <- apply(X = apply(
      X = merged.dt[, -colnames(dt.x), with = FALSE],
      MARGIN = 2, FUN = is.na), MARGIN = 2, FUN = sum, na.rm = TRUE)
    list("merge.res" = merged.dt, "NA.count" = count.na.col)
  })
  names(list.merged.dt) <- paste0("Merged by '", by.x, "' & '", try.y,"'")
  ls.minNA <- lapply(X = list.merged.dt, FUN = function(i){
    min(i$NA.count, na.rm = TRUE)
  })
  minNA <- unlist(ls.minNA)
  bst.mtch <- minNA[minNA == min(minNA)]
  #If there is a best match, returns best match.
  if (length(bst.mtch) > 1) {
    best.match <- names(bst.mtch)
    warning("More than one match performed the best for the selected columns.")
  } else { #Else, returns names of ex aequo matches
    best.match <- list.merged.dt[[names(bst.mtch)]]$merge.res
  }
  res <- list("best.merged.dt" = best.match, "merging.results" = list.merged.dt)
  return(res)
}
