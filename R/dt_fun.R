##FUNCTIONS

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
#' @references

dt.sub<-function(DT, pattern, replacement, ignore.case = FALSE, perl = FALSE,
                 fixed = FALSE, useBytes = FALSE){
  col.blck<-DT[, .(lapply(.SD, grepl, pattern=pattern, ignore.case=ignore.case,
                          perl = perl, fixed = fixed, useBytes = useBytes),
                   lapply(.SD, typeof),colnames(DT))][, .(lapply(V1,any),V2,V3)
                                                      ][V1==TRUE,c("V3","V2")]
  if(nrow(col.blck) != 0){
    #If column of type list
    if(nrow(col.blck[V2=="list"]) != 0){
      DT[, (col.blck[V2=="list"]$V3) := lapply(
        .SD, FUN = function(i){
          lapply(X = i, gsub, pattern = pattern, replacement = replacement,
                 ignore.case=ignore.case, perl = perl, fixed = fixed,
                 useBytes = useBytes)
        }), .SDcols=col.blck[V2=="list"]$V3]
    }
    #Any other type of column
    if(nrow(col.blck[!V2 %in% "list"]) != 0){
      DT[,(col.blck[!V2 %in% "list"]$V3) := lapply(
        .SD,gsub, pattern = pattern, replacement = replacement,
        ignore.case=ignore.case, perl = perl, fixed = fixed, useBytes=useBytes),
        .SDcols=col.blck[!V2 %in% "list"]$V3]
    }
  } else {
    warning("Pattern not found in data.table object.")
  }
}


#' Replaces data.table columns of type list to a column of type vector.
#' 
#' @param DT           A \code{data.table}.
#' @param column.names A \code{character} vector containing column names you
#'                     want to convert from list type to vectors.
#' @return A \code{data.table}.
#' @author Yoann Pageaud.
#' @export
#' @examples
#' @references

dt.ls2c<-function(DT, column.names=NULL){
  #Check if all colnames given are in the data.table
  if(any(column.names %in% colnames(DT) == FALSE)){
    stop("Some values in 'colnames' do not match colnames in the data.table.")
  } else {
    if(is.null(column.names)){
      DT[, names(DT) := lapply(X = .SD, FUN = unlist)]
    } else {
      DT[,(column.names) := lapply(X = .SD, FUN = unlist), .SDcols=column.names] 
    }  
  }
}


#' Removes duplicated column content in a data.table.
#' 
#' @param DT A \code{data.table}.
#' @param ignore A \code{character} or \code{integer} vector specifying columns
#'               that should be ignored during duplication removal.
#' @return A \code{data.table}.
#' @author Yoann Pageaud.
#' @export
#' @examples
#' @references

dt.rm.dup<-function(DT, ignore=NULL){
  dup.cols<-names(duplicated(t(DT))[duplicated(t(DT))==TRUE])
  if(length(dup.cols)!=0){
    DT<-DT[,-c(dup.cols[!dup.cols %in% ignore]), with=FALSE]
  } else {
    warning("No duplicated column content found in data.table object.")
  }
  return(DT)
}


#' Converts columns of 'double.integer64' type into 'character' type.
#' 
#' @param DT           A \code{data.table}.
#' @param column.names A \code{character} vector containing column names you
#'                     want to convert from 'double.integer64' type to
#'                     'character'.
#' @return A \code{data.table}.
#' @author Yoann Pageaud.
#' @export
#' @examples
#' @references

dt.int64tochar<-function(DT, column.names=NULL){
  if(any(column.names %in% colnames(DT) == FALSE)){
    stop("Some values in 'colnames' do not match colnames in the data.table.")
  } else {
    if(is.null(column.names)){
      DT[, lapply(X = .SD, FUN = function(i){
        if(typeof(i) == "double"){ as.character(as.numeric(i)) } else { i } })]
    } else {
      DT[,(column.names) := lapply(X = .SD, FUN = function(i){
        if(typeof(i) == "double"){ as.character(as.numeric(i)) } else { i } }),
        .SDcols=column.names] 
    }  
  }
}


#' Combines information using duplicated colnames of a data.table resulting from
#' merge(). 
#' 
#' @param DT A \code{data.table} resulting from a merge() operation. Partially
#'           duplicated columns (some values are duplicated at some position of
#'           columns, while at other positions NA values are present in only one
#'           of the columns) are automatically detected using their colnames
#'           suffixes '.x' and '.y', and combined into new columns (thus
#'           reducing the amount of missing values). original duplicated columns
#'           are then removed. 
#' @return A \code{data.table} with duplicated columns removed, and resulting
#'         combined columns appended on the right.
#' @author Yoann Pageaud.
#' @export
#' @examples
#' @references

dt.combine<-function(DT){
  cnames<-strsplit(x = names(DT), split = "\\.[xy]")
  dupcol<-unique(cnames[duplicated(cnames) | duplicated(cnames, fromLast=TRUE)])
  
  invisible(lapply(X = dupcol, FUN = function(i){
    DT.comp<-DT[,names(DT)[grepl(pattern = i, x = names(DT))], with=FALSE]
    #Add index col
    DT.comp<-cbind(idx.row = seq(nrow(DT.comp)), DT.comp)
    #Check if all non-missing data are the same
    DT.val<-DT.comp[complete.cases(DT.comp)]
    if(length(duplicated(t(DT.val))[duplicated(t(DT.val)) == TRUE]) ==
       ncol(DT.comp)-2){
      #Get rows containing at least one NA
      DT.na<-DT.comp[!complete.cases(DT.comp)]
      res<-strsplit(x = DT.na[, do.call(what=paste, DT.na[,-1,])],split = " NA")
      if(unique(lapply(res, length))==1){
        DT.new<-rbind(
          DT.val[,c(1,2),], data.table(DT.na$idx.row, res), use.names=FALSE)
        #Re-order rows following index
        DT.new<-DT.new[order(idx.row)][,2]
        colnames(DT.new)<-i
        DT<<-cbind(DT,DT.new)
      } } else { stop("Not all partially duplicated columns are equals.") }
  }))
  colrm<-names(DT)[duplicated(cnames) | duplicated(cnames, fromLast=TRUE)]
  return(DT[,-colrm, with=FALSE])
}