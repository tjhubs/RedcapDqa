#'@name RedcapDqa
#'@title Redcap Data Quality Assurance
#'@description Class to hold DQA datasets and other allied objects
#'@rdname RedcapDqa
#'@concept RedcapDqa
#'@details This class holds the DQA and repository datasets as well other objects that are created in the process of DQA.
#'
#'It stores these allied objects in its own cache and has a log to track events that occur in the process of DQA
#'
#'@slot dqaData DQA dataset
#'@slot repoData Repository dataset
#'@slot cache Temporary storage
#'
#'@export
#'@include RedcapDqaPackage.R

RedcapDqa = setClass(
  "RedcapDqa",
  slots = c(
    identifiers = "integer",
    metaData = "data.frame",
    dqaData = "data.frame",
    repoData = "data.frame",
    strata = "character"
    ),
  prototype = prototype(
    identifiers = integer(),
    metaData = data.frame(),
    dqaData = data.frame(),
    repoData = data.frame(),
    strata = NA_character_
    ),
  validity = function(object) {
    if (!all(dim(object@dqaData) == dim(object@repoData)))
      return("dimensions of dqa and repo data differ")
    if (!all(colnames(object@dqaData) %in% colnames(object@repoData)))
      return("columns of dqa and repo data do not match")
    if (length(object@identifiers) != nrow(object@repoData))
      return("no of identifiers must match dataset row count")
    if (!is.na(object@strata) && !all(object@strata %in% names(object@dqaData)))
      return("strata variable not in DQA dataset")
    if (!is.na(object@strata) && !all(object@strata %in% names(object@repoData)))
      return("strata variable not in repo dataset")
    TRUE
  })


#'@name print
#'@rdname printRedcapDqa
setGeneric("print")

#'@name print.RedcapDqa
#'@rdname printRedcapDqa
#'@title Print DQA object
#'@description Print DQA object
#'@concept RedcapDqa
#'@details Generic Print method
#'@export
#'@include RedcapDqaPackage.R

setMethod(f = "print", signature(x = "RedcapDqa"), function(x, ...) {
  . = "Redcap Data Quality Assurance"
  tmp = dim(x@dqaData)
  . = c(., paste0("Data: ", tmp[1], " rows & ", tmp[2], " columns"))
  if (!is.na(x@strata))
    . = c(., paste0("Stratification: ", paste0(x@strata, collapse = ', ')))
  . = paste0(., collapse = "\n")
  . = paste0(., "\n")
  cat(.)
})

#'@name show.RedcapDqa
#'@rdname showRedcapDqa
#'@title Show DQA object
#'@description Show DQA object
#'@concept RedcapDqa
#'@details Generic show method
#'@export
#'@include RedcapDqaPackage.R

setMethod("show", signature(object = "RedcapDqa"), function(object) {
  print(object)
})
