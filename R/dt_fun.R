
#' In-place pattern matching and replacement in a data.table.
#'
#' @param DT          A \code{data.table}.
#' @param pattern     A \code{character} string containing a regular expression
#'                    (or character string for fixed = TRUE) to be matched in
#'                    the given character vector. Coerced by as.character to a
#'                    character string if possible. If a character vector of
#'                    length 2 or more is supplied, the first element is used
#'                    with a warning.
#' @param replacement A \code{character} string replacement for matched pattern.
#'                    Coerced to character if possible. For fixed = FALSE this
#'                    can include backreferences "\1" to "\9" to parenthesized
#'                    subexpressions of pattern. For perl = TRUE only, it can
#'                    also contain "\U" or "\L" to convert the rest of the
#'                    replacement to upper or lower case and "\E" to end case
#'                    conversion. If a character vector of length 2 or more is
#'                    supplied, the first element is used with a warning. If NA,
#'                    all elements in the result corresponding to matches will
#'                    be set to NA.
#' @param ignore.case A \code{logical}. If FALSE, the pattern matching is case
#'                    sensitive and if TRUE, case is ignored during matching.
#' @param perl        A \code{logical}. Should Perl-compatible regexps be used?
#' @param fixed       A \code{logical}. If TRUE, pattern is a string to be
#'                    matched as is. Overrides all conflicting arguments.
#' @param useBytes    A \code{logical}. If TRUE the matching is done
#'                    byte-by-byte rather than character-by-character.
#' @return A \code{data.table}.
#' @author Yoann Pageaud.
#' @export
#' @examples
#' dtbl<-data.table(col1 = rev(seq(16)), col2=rep(x = c("hello", "world"), 8))
#' dt.sub(DT = dtbl, pattern = "hello", replacement = "goodbye")

dt.sub <- function(DT, pattern, replacement, ignore.case = FALSE, perl = FALSE,
                 fixed = FALSE, useBytes = FALSE){
  col.blck <- DT[, .(lapply(
    X = .SD, FUN = grepl, pattern = pattern, ignore.case = ignore.case,
    perl = perl, fixed = fixed, useBytes = useBytes),
    lapply(X = .SD, FUN = typeof), colnames(DT))][, .(
      lapply(X = V1, FUN = any), V2, V3)][V1 == TRUE, c("V3", "V2")]
  if(nrow(col.blck) != 0){
    #If column of type list
    if(nrow(col.blck[V2 == "list"]) != 0){
      DT[, (col.blck[V2 == "list"]$V3) := lapply(X = .SD, FUN = function(i){
        lapply(X = i, FUN = gsub, pattern = pattern, replacement = replacement,
               ignore.case = ignore.case, perl = perl, fixed = fixed,
                 useBytes = useBytes)
        }), .SDcols = col.blck[V2 == "list"]$V3]
    }
    #Any other type of column
    if(nrow(col.blck[!V2 %in% "list"]) != 0){
      DT[, (col.blck[!V2 %in% "list"]$V3) := lapply(
        X = .SD, FUN = gsub, pattern = pattern, replacement = replacement,
        ignore.case = ignore.case, perl = perl, fixed = fixed,
        useBytes = useBytes), .SDcols = col.blck[!V2 %in% "list"]$V3]
    }
  } else {
    warning("Pattern not found in data.table object.")
  }
}


#' Replaces data.table columns of type list to a column of type vector in-place.
#'
#' @param DT           A \code{data.table}.
#' @param column.names A \code{character} vector containing column names you
#'                     want to convert from list type to vectors.
#'                     If no column names are specified, the function read
#'                     through the entire data.table
#'                     (Default: column.names = NULL).
#' @return A \code{data.table}.
#' @author Yoann Pageaud.
#' @export
#' @examples
#' dtbl<-data.table(
#'   col1 = rev(seq(16)),
#'   col2=as.list(rep(x = c("hello", "world"), 8))) #'col2' is of type 'list'.
#' dt.ls2c(DT = dtbl) #All columns of type 'list' are converted into vectors.
#' dt.ls2c(DT = dtbl, column.names = "col2") #Only 'col2' is converted into a vector.

dt.ls2c <- function(DT, column.names = NULL){
  #Check if all colnames given are in the data.table
  if(any(column.names %in% colnames(DT) == FALSE)){
    stop("Some values in 'colnames' do not match colnames in the data.table.")
  } else {
    if(is.null(column.names)){
      DT[, names(DT) := lapply(X = .SD, FUN = unlist)]
    } else {
      DT[, (column.names) := lapply(X = .SD, FUN = unlist),
         .SDcols = column.names]
    }
  }
}


#' Removes duplicated column content from a data.table.
#'
#' @param DT     A \code{data.table}.
#' @param ignore A \code{character} or \code{integer} vector specifying columns
#'               that should be ignored during duplication removal.
#' @return A \code{data.table}.
#' @author Yoann Pageaud.
#' @export
#' @examples
#' dtbl<-data.table(
#'   col1 = rep(x = c("hello", "world"), 8),
#'   col2 = rep(x = c("hello", "world"), 8)) #'col2' is a duplicate of 'col1'.
#' dt.rm.dup(DT = dtbl) #Only 'col1' remains.
#' #You can ignore specific columns that will not be remove if duplicated:
#' dt.rm.dup(DT = dtbl,ignore = "col2")

dt.rm.dup <- function(DT, ignore = NULL){
  dup.cols <- names(duplicated(t(DT))[duplicated(t(DT)) == TRUE])
  if(length(dup.cols) != 0){
    DT <- DT[, -c(dup.cols[!dup.cols %in% ignore]), with = FALSE]
  } else {
    warning("No duplicated column content found in data.table object.")
  }
  return(DT)
}

#' Removes columns exclusively containing NAs from a data.table.
#'
#' @param DT     A \code{data.table}.
#' @param ignore A \code{character} or \code{integer} vector specifying columns
#'               that should be ignored during duplication removal.
#' @return A \code{data.table}.
#' @author Yoann Pageaud.
#' @export

dt.rm.allNA <- function(DT, ignore = NULL){
  na.cols <- suppressWarnings(allNA.col(data = DT)$fullNA.col)
  if(length(na.cols) != 0){
    DT <- DT[, -c(na.cols[!na.cols %in% ignore]), with = FALSE]
  } else {
    warning("No column exclusively containing NAs found in data.table object.")
  }
  return(DT)
}

#' Converts columns of 'double.integer64' type into 'character' type in-place.
#'
#' @param DT           A \code{data.table}.
#' @param column.names A \code{character} vector containing column names you
#'                     want to convert from 'double.integer64' type to
#'                     'character'.
#'                     If no column names are specified, the function read
#'                     through the entire data.table
#'                     (Default: column.names = NULL).
#' @return A \code{data.table}.
#' @author Yoann Pageaud.
#' @export

dt.int64tochar <- function(DT, column.names = NULL){
  if(any(column.names %in% colnames(DT) == FALSE)){
    stop("Some values in 'colnames' do not match colnames in the data.table.")
  } else {
    if(is.null(column.names)){
      DT[, lapply(X = .SD, FUN = function(i){
        if(typeof(i) == "double"){ as.character(as.numeric(i)) } else { i } })]
    } else {
      DT[,(column.names) := lapply(X = .SD, FUN = function(i){
        if(typeof(i) == "double"){ as.character(as.numeric(i)) } else { i } }),
        .SDcols = column.names]
    }
  }
}


#' Combines 2 columns from a datatable into a 1 column data.table.
#'
#' @param DT      A \code{data.table}.
#' @param cols    A \code{character} vector of length 2 matching columns names
#'                of DT.
#' @param mrg.col A \code{character} to be used to name the resulting combined
#'                column.
#' @param keep.colname An \code{integer} equals to 1, or 2, or NULL. If equals
#'                     to 1, the resulting combined column will be named after
#'                     the first column in 'cols'. If equals to 2, the resulting
#'                     combined column will be named after the second column in
#'                     'cols'. If NULL, keep.colname is not used for the naming
#'                     of the resulting combined column
#'                     (Default: keep.colname = NULL).
#' @return A \code{data.table} with 1 column resulting from the merging of the
#'         partially duplicated columns of the input.
#' @author Yoann Pageaud.
#' @keywords internal

dt.combination <- function(DT, cols, mrg.col, keep.colname = NULL){
  DT.comp <- DT[, cols, with = FALSE]
  #Add index col
  DT.comp <- cbind(idx.row = seq(nrow(DT.comp)), DT.comp)
  #Check if all non-missing data are the same
  DT.val <- DT.comp[complete.cases(DT.comp)]
  if(length(duplicated(t(DT.val))[duplicated(t(DT.val)) == TRUE]) ==
     ncol(DT.comp)-2){
    #Check if some rows contain at least one NA
    DT.na <- DT.comp[!complete.cases(DT.comp)]
    if(nrow(DT.na) > 0){
      #Remove NAs with leading and trailing whitespaces
      # res <- trimws(
      #   gsub(pattern = "[^a-zA-Z0-9\\-]*NA[^a-zA-Z0-9\\-]*", replacement = " ",
      #        x = DT.na[, do.call(what = paste, DT.na[, -1, ])]))
      res <- trimws(
        gsub(pattern = "[^a-zA-Z0-9\\-]NA|NA[^a-zA-Z0-9\\-]|NA\\sNA",
             replacement = " ", x = DT.na[, do.call(
               what = paste, DT.na[, -1, ])]))
      #Replace empty strings by NAs
      res <- sub(pattern = "^$", replacement = NA, x = res)
      #Split non-NA values if any
      res <- strsplit(x = res, split = " ")
      #Check if value unique for each row, and length of unique value is 1 for
      # all rows
      is.unique <- lapply(X = res, FUN = unique)
      if(unique(lapply(X = is.unique, FUN = length)) == 1){
        DT.new <- rbind(DT.val[, c(1, 2), ], data.table(
          DT.na$idx.row, unlist(is.unique)), use.names = FALSE)
        #Re-order rows following index
        DT.new <- DT.new[order(idx.row)][, 2]
        if(is.null(keep.colname)){
          colnames(DT.new) <- mrg.col
        } else if(keep.colname == 1){
          colnames(DT.new) <- cols[1]
        } else if(keep.colname == 2){
          colnames(DT.new) <- cols[2]
        } else { stop("Unsupported value for 'keep.colname'.") }
        DT.new
      }
    } else { #All values are the same and there is no NA between columns
      DT.new <- DT.val[, 2]
      if(is.null(keep.colname)){
        colnames(DT.new) <- mrg.col
      } else if(keep.colname == 1){
        colnames(DT.new) <- cols[1]
      } else if(keep.colname == 2){
        colnames(DT.new) <- cols[2]
      } else { stop("Unsupported value for 'keep.colname'.") }
      DT.new
    }
  } else { stop("Not all partially duplicated columns are equals.") }
}


#' Combines values of partially duplicated columns from a data.table into new
#' columns.
#'
#' @param DT           A \code{data.table} resulting from a merge() operation.
#'                     By default, partially duplicated columns (some values are
#'                     duplicated at some position of columns, while at other
#'                     positions NAs are present in only one of the columns) are
#'                     automatically detected using their colnames suffixes '.x'
#'                     and '.y', and combined into new columns (thus, reducing
#'                     the amount of missing values). Original duplicated
#'                     columns are then removed.
#' @param col1         A \code{character} specifying a data.table column name
#'                     that you suspect to be the partial duplicate from the
#'                     column col2. If col1 is NULL, dt.combine() will look for
#'                     duplicated columns (Default: col1 = NULL).
#' @param col2         A \code{character} specifying a data.table column name
#'                     that you suspect to be the partial duplicate from the
#'                     column col1. If col2 is NULL, dt.combine() will look for
#'                     duplicated columns (Default: col2 = NULL).
#' @param keep.colname An \code{integer} equals to 1, or 2, or NULL. If equals
#'                     to 1, the resulting combined column will be named after
#'                     'col1'. If equals to 2, the resulting combined column
#'                     will be named after 'col2'. If NULL, keep.colname is not
#'                     used for the naming of the resulting combined column
#'                     (Default: keep.colname = NULL).
#' @return A \code{data.table} with duplicated columns removed, and resulting
#'         combined columns appended on the right.
#' @author Yoann Pageaud.
#' @export
#' @examples
#' dtbl1<-data.table(col1 = rev(seq(16)),
#'                   col2=c(rep(x = c("hello", "world"), 4), rep(NA, 8)))
#' dtbl2<-data.table(col1 = rev(seq(16)),
#'                   col2=c(rep(NA, 4), rep(x = c("hello", "world"), 6)))
#' #'dtbl1' and 'dtbl2' are both missing different values in 'col2'.
#'
#' dtbl.mrg<-merge(x = dtbl1, y = dtbl2, by = "col1")
#' #The colname of the 2nd column of 'dtbl1' and 'dtbl2' is the same.
#' #merge() appends '.x' and '.y' respectively to 'col2' in 'dtbl1' and 'dtbl2'.
#'
#' #Are 'col2.y' and 'col2.x' partially duplicated ?
#' dt.combine(dtbl.mrg) #Yes! 'col2.x' and 'col2.y' have been combined into
#' 'col2'.
#' @references

dt.combine <- function(DT, col1 = NULL, col2 = NULL, keep.colname = NULL){
  if(is.null(col1) | is.null(col2)){ #If no columns provided scan the data.table
    #Search & list potential duplicated columns
    cnames <- strsplit(x = names(DT), split = "\\.[xy]")
    dupcol <- unique(cnames[duplicated(cnames) | duplicated(
      cnames, fromLast = TRUE)])
    ls.dt <- lapply(X = dupcol, FUN = function(i){
      dt.combination(
        DT = DT, cols = names(DT)[grepl(pattern = i, x = names(DT))],
        mrg.col = i, keep.colname = keep.colname)
    })
    DT.new <- do.call(cbind, ls.dt)
    colrm <- names(DT)[duplicated(cnames) | duplicated(cnames, fromLast = TRUE)]
  } else {
    #Check if merged column name can take rootname of the 2 input columns
    if(length(unique(strsplit(x = c(col1, col2), split = "\\.[xy]"))) == 1){
      mrgcolname <- unlist(unique(strsplit(x = c(col1, col2),
                                           split = "\\.[xy]")))
    } else {
      mrgcolname <- paste0("comb.", paste0(c(col1, col2), collapse = "_"))
    }
    DT.new <- dt.combination(
      DT = DT, cols = c(col1, col2), mrg.col = mrgcolname,
      keep.colname = keep.colname)
    colrm <- c(col1, col2)
  }
  return(cbind(DT[, -colrm, with = FALSE], DT.new))
}
