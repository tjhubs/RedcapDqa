#'@name audit
#'@rdname auditRedcapDqa
#'@export
#'
setGeneric("audit", function(obj, ...) standardGeneric("audit"))



#'@name audit.RedcapDqa
#'@title Concordance Checks
#'@description Run crude concordance checks
#'@concept RedcapDqa
#'@rdname auditRedcapDqa
#'@export
#'@param obj A \code{RedcapDqa} object
#'@seealso \code{\link{RedcapDqa}}
#'@include RedcapDqa.R
#'
#'@return A \code{RedcapDqaResult}.
#'
#'This contains:
#'
#'\describe{
#'\item{concordance}{Overall concordance}
#'\item{record.concordance}{Concordance for each record}
#'\item{field.concordance}{Concordance for each variable}
#'\item{matrix.concordance}{Boolean matrix of concordance for each data cell}
#'}
#'
#'@details This utility function instantiates the DQA process.
#'
#'It pulls the record ids from both repositories and matches the common identifiers.
#'
#'Data is then streamed from both repos and then matched for common fields.
#'
#'These datasets are used to create an instance of a \code{RedcapDqa} object.

setMethod(f = "audit", signature(obj = "RedcapDqa"), function(obj, ...) {
  if (!validObject(obj)) stop("object not valid")
  .D = obj@dqaData
  .R = obj@repoData
  .obj = list()
  . = matrix(NA, ncol = ncol(.D), nrow = nrow(.D))
  for (i in 1:nrow(.D)) {
    for (j in 1:ncol(.D)) {
      .d = str_trim(tolower(.D[i, j]))
      .r = str_trim(tolower(.R[i, j]))
      if (isTRUE("" == str_trim(.d))) .d = NA
      if (isTRUE("" == str_trim(.r))) .r = NA
      if (is.na(.d) && is.na(.r)) .v = T else .v = isTRUE(.d == .r)
      .[i, j] = .v
    }
  }
  dimnames(.) = list(Observations = obj@identifiers, Fields = colnames(.D))
  .obs = apply(., 1, function(x) sum(x, na.rm = T) / ncol(.))
  names(.obs) = obj@identifiers
  .fld = apply(., 2, function(x) sum(x, na.rm = T) / nrow(.))
  names(.fld) = colnames(.D)
  .all = sum(., na.rm = T) / prod(dim(.))
  .obj$concordance = .all
  .obj$record.concordance = .obs
  .obj$field.concordance = .fld
  .obj$matrix.concordance = .
  class(.obj) = "RedcapDqaResult"
  .obj
})

#'@name print.RedcapDqaResult
#'@rdname printRedcapDqaResult
#'@title print DQA Results
#'@description print DQA Results
#'@concept RedcapDqa
#'@details Generic print method
#'@export
#'@include RedcapDqa.R
print.RedcapDqaResult = function(x, ...) {
  cat(paste0(c(
      "Redcap Data Quality Assurance Concordance",
      paste0("Overall Concordance: ", round(x$concordance*100, 2), "%")
      ), collapse = "\n"), "\n")
}

#'@name show.RedcapDqaResult
#'@rdname showRedcapDqaResult
#'@title show DQA Results
#'@description show DQA Results
#'@concept RedcapDqa
#'@details Generic show method
#'@export
#'@include RedcapDqa.R

show.RedcapDqaResult = function(x) {
  print(x)
}

#'@name summary.RedcapDqaResult
#'@rdname summaryRedcapDqaResult
#'@title summary DQA Results
#'@description summary DQA Results
#'@concept RedcapDqa
#'@details Generic summary method
#'@export
#'@include RedcapDqa.R
summary.RedcapDqaResult = function(x, ...) {
  . = "Redcap DQA Concordance Analysis Results\n\nOverall Concordance:"
  . = c(., paste0(round(x$concordance*100,2), "%"))
  . = c(., "\nRecord Concordance:")
  tmp = x$record.concordance
  tmp2 = range(tmp)
  tmp2 = paste0(round(tmp2 * 100, 2), "%")
  tmp2 = paste0(tmp2, collapse = " - ")
  if (3 < length(tmp))
    tmp = tmp[1:3]
  tmp = paste0(paste0(tmp, collapse = ", "), " ...")
  . = c(., paste0(tmp2, ": ", tmp))
  . = c(., "\nField Concordance:")
  tmp = x$field.concordance
  tmp2 = range(tmp)
  tmp2 = paste0(round(tmp2 * 100, 2), "%")
  tmp2 = paste0(tmp2, collapse = " - ")
  if (3 < length(tmp))
    tmp = tmp[1:3]
  tmp = paste0(paste0(tmp, collapse = ", "), " ...")
  . = c(., paste0(tmp2, ": ", tmp))
  . = paste0(., collapse = "\n")
  . = paste0(., "\n")
  cat(.)
}
