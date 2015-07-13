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
  if (("stratified" %in% names(list(...))))
    stratified = unlist(list(...)["stratified"])
  else 
    stratified = FALSE
  if (is.logical(stratified) && stratified)
    stratified = TRUE
  else
    stratified = FALSE
  if (!validObject(obj)) stop("object not valid")
  if (stratified && is.na(obj@strata))
    stratified = FALSE
  .D = as.data.frame(obj@dqaData)
  .R = obj@repoData
  if (stratified) {
    .ids <- c(list(Overall = obj@identifiers), split(obj@identifiers, as.data.frame(.D)[, obj@strata]))
    .strata_nums <- names(.ids)
    .D <- c(list(Overall = .D), split(.D, as.data.frame(.D)[, obj@strata]))
    .D <- lapply(.D, function(x) {
      if (isTRUE(obj@strata %in% colnames(x)))
        x[, -which(obj@strata %in% colnames(x))]
      x
    })
    .R <- c(list(Overall = .R), split(.R, as.data.frame(.R)[, obj@strata]))
    .R <- lapply(.R, function(x) {
      if (isTRUE(obj@strata %in% colnames(x)))
        x[, -which(obj@strata %in% colnames(x))]
      x
    })
  } else {
    .ids <- list(obj@identifiers)
    .strata_nums <- "Overall"
    .D = list(.D)
    .R = list(.R)
  }
  .auditImpl <- function(d, r, obsLabs = NULL, colLabs = NULL) {
    .obj = list()
    . = matrix(NA, ncol = ncol(d), nrow = nrow(d))
    for (i in 1:nrow(d)) {
      for (j in 1:ncol(d)) {
        .d = str_trim(tolower(d[i, j]))
        .r = str_trim(tolower(r[i, j]))
        if (isTRUE("" == str_trim(.d))) .d = NA
        if (isTRUE("" == str_trim(.r))) .r = NA
        if (is.na(.d) && is.na(.r)) .v = T else .v = isTRUE(.d == .r)
        .[i, j] = .v
      }
    }
    dimnames(.) = list(Observations = obsLabs, Fields = colLabs)
    .obs = apply(., 1, function(x) sum(x, na.rm = T) / ncol(.))
    names(.obs) = obsLabs
    .fld = apply(., 2, function(x) sum(x, na.rm = T) / nrow(.))
    names(.fld) = colLabs
    .all = sum(., na.rm = T) / prod(dim(.))
    .obj$concordance = .all
    .obj$record.concordance = .obs
    .obj$field.concordance = .fld
    .obj$matrix.concordance = .
    class(.obj) = "RedcapDqaResult"
    .obj
  }
  res <- list()
  for (i in 1:length(.D)) {
    res <- c(res, list(.auditImpl(.D[[i]], .R[[i]], .ids[[i]], colnames(.D[[1]]))))
  }
  names(res) <- .strata_nums
  class(res) <- "RedcapDqaResults"
  res
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
    paste0("Concordance: ", round(x$concordance*100, 2), "%")
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
#'@description Summarize DQA Results
#'@concept RedcapDqa
#'@details Generic summary method
#'@export
#'@include RedcapDqa.R
summary.RedcapDqaResult = function(x, ...) {
  . = "Redcap DQA Concordance Analysis Results\n\nTotal Concordance:"
  . = c(., paste0(round(x$concordance*100,2), "%"))
  . = c(., "\nRecord Concordance:")
  tmp = x$record.concordance
  tmp2 = range(tmp)
  tmp2 = paste0(round(tmp2 * 100, 2), "%")
  tmp2 = paste0(tmp2, collapse = " - ")
  if (3 < length(tmp))
    tmp = tmp[1:3]
  tmp = paste0(round(tmp * 100, 2), "%")
  tmp = paste0(paste0(tmp, collapse = ", "), " ...")
  . = c(., paste0(tmp2, ": ", tmp))
  . = c(., "\nField Concordance:")
  tmp = x$field.concordance
  tmp2 = range(tmp)
  tmp2 = paste0(round(tmp2 * 100, 2), "%")
  tmp2 = paste0(tmp2, collapse = " - ")
  if (3 < length(tmp))
    tmp = tmp[1:3]
  tmp = paste0(round(tmp * 100, 2), "%")
  tmp = paste0(paste0(tmp, collapse = ", "), " ...")
  . = c(., paste0(tmp2, ": ", tmp))
  . = paste0(., collapse = "\n")
  . = paste0(., "\n")
  cat(.)
}

#'@name print.RedcapDqaResults
#'@rdname printRedcapDqaResults
#'@title print DQA Results
#'@description print DQA Results
#'@concept RedcapDqa
#'@details Generic print method
#'@export
#'@include RedcapDqa.R
print.RedcapDqaResults = function(x, ...) {
  x <- unclass(x)
  if (1 == length(x)) {
    print(x[[1]])
  } else {
    namz <- paste(names(x)[-1], ":")
    linez <- sapply(namz, function(x) {
      paste0(rep("-", nchar(x)), collapse = "")
    })
    namz <- paste('\n', namz)
    res <- c("Redcap Data Quality Assurance Concordance", "", "Overall:", paste0(rep("-", nchar("Overall:")), collapse = ""))
    res <- c(res, paste0("Data:\t\t\t", nrow(x[[1]]$matrix.concordance), " records"))
    res <- c(res, paste0("Total Concordance:\t", round(x[[1]]$concordance*100, 2), '%'))
    tmp <- round(range(x[[1]]$record.concordance, na.rm = T) * 100, 2)
    res <- c(res, paste0("Record Concordance:\t", paste0(paste0(tmp , '%'), collapse = '-')))
    tmp <- round(range(x[[1]]$field.concordance, na.rm = T)*100, 2)
    res <- c(res, paste0("Field Concordance:\t", paste0(paste0(tmp , '%'), collapse = '-')))
    tmp <- x[[1]]$field.concordance
    tmp <- names(sort(tmp, decreasing = TRUE))[1:5]
    res <- c(res, paste0("Top 5 Best fields:\t", paste0(tmp , collapse = ", ")))
    tmp <- x[[1]]$field.concordance
    tmp <- names(sort(tmp, decreasing = FALSE))[1:5]
    res <- c(res, paste0("Top 5 Worst fields:\t", paste0(tmp , collapse = ", ")))
    x <- x[-1]
    tmp <- lapply(x, function(d) {
      res <- paste0("Data:\t\t\t", nrow(d$matrix.concordance), " records")
      res <- c(res, paste0("Total Concordance:\t", round(d$concordance*100, 2), '%'))
      tmp <- round(range(d$record.concordance, na.rm = T) *100, 2)
      res <- c(res, paste0("Record Concordance:\t", paste0(paste0(tmp , '%'), collapse = '-')))
      tmp <- round(range(d$field.concordance, na.rm = T)*100, 2)
      res <- c(res, paste0("Field Concordance:\t", paste0(paste0(tmp , '%'), collapse = '-')))
      tmp <- d$field.concordance
      tmp <- names(sort(tmp, decreasing = TRUE))[1:5]
      res <- c(res, paste0("Top 5 Best fields:\t", paste0(tmp , collapse = ", ")))
      tmp <- d$field.concordance
      tmp <- names(sort(tmp, decreasing = FALSE))[1:5]
      res <- c(res, paste0("Top 5 Worst fields:\t", paste0(tmp , collapse = ", ")))
      res <- paste0(res, collapse = "\n")
    })
    tmp <- paste(namz, linez, tmp, sep = "\n")
    res <- c(res, tmp)
    res <- paste0(res, collapse = '\n')
    cat(res)
  }
}

#'@name show.RedcapDqaResults
#'@rdname show.RedcapDqaResults
#'@title show DQA Results
#'@description show DQA Results
#'@concept RedcapDqa
#'@details Generic show method
#'@export
#'@include RedcapDqa.R
show.RedcapDqaResults = function(x, ...) {
  print.RedcapDqaResults(x, ...)
}

#'@name summary.RedcapDqaResults
#'@rdname summaryRedcapDqaResults
#'@title summary DQA Results
#'@description Summarize DQA Results
#'@concept RedcapDqa
#'@details Generic summary method
#'@export
#'@include RedcapDqa.R
summary.RedcapDqaResults = function(x, ...) {
  len <- length(unclass(x))
  if (1 == len)
    summary(unclass(x)[[1]])
  else
    print(x)
}
