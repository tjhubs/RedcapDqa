#'@name validate_inputs
#'@title Validate inputs
#'@description Check the inputs that control the dqa process.
#'@concept RedcapDqa
#'@rdname ValidateInput
#'@param cxt Context representionf the current context of the function execution. Can be a list or an environment.
#'@seealso \code{\link{redcap_dqa}}
#'
#'@details This utility function checks the input fed into the wrapper function that instantiates the RedcapDqa object.
#'
#'It checks the validity of the input and flags any errors encountered.
#'
#'This is to separate the concerns of the wrapper function and make it leaner

validate_input <- function(env) {
  copy_from_env(env, environment())
  
  if (1 < length(min_date)) {
    warning('only one minimum date needed. taking the first value')
    min_date = min_date[1]
  }
  if (1 < length(max_date)) {
    warning('only one maximum date needed. taking the first value')
    max_date = max_date[1]
  }
  if (!is_date(min_date)) stop('invalid minimum date')
  if (!is_date(max_date)) stop('invalid maximum date')
  if (1 < length(date_var)) {
    warning('only one date variable needed. taking the first value')
    date_var = date_var[1]
  }
  if (data_missing(min_date))
    min_date <- as.Date(NA)
  if (data_missing(max_date))
    max_date <- as.Date(NA)
  
  min_date = as.Date(min_date); max_date = as.Date(max_date)
  if (!data_missing(min_date) && !data_missing(max_date) && 
      min_date > max_date) 
    stop('minimum date greater than maximum date')
  
  if (1 < length(id_var)) {
    warning('only one identifier variable needed. taking the first value')
    id_var <- id_var[1]
  }
  
  if (all(sapply(strata, data_missing)))
    strata <- NA
  
  suppressMessages({
    if (!data_missing(meta_file_location)) {
      if (!file.exists(meta_file_location))
        stop("metadata file location not found")
      if (!grepl(".csv$", as.character(meta_file_location)))
        stop("metadata must be a csv")
      meta_data = read.csv(meta_file_location, stringsAsFactors = F)
    } else {
      meta_data = get_redcap_data(api = repo_api_url, 
                                  token = repo_token, 
                                  content = "metadata", 
                                  local = repo_local
      )
    }
    if (!data_missing(id_var)) {
      if (!grepl(id_var, meta_data[, 1]))
        stop("identifier variable not in metadata")
    } else {
      id_var = meta_data[1, 1]
    }
    if (!data_missing(date_var)) {
      if (!any(grepl(date_var, meta_data[, 1])))
        stop("date variable not in metadata")
    }
    if (!data_missing(strata)) {
      if (!any(grepl(strata, meta_data[, 1])))
        stop("stratification variable not in metadata")
    }
    if (any(bool <- !sapply(site_id, data_missing)))
      site_id <- site_id[bool]
      if (data_missing(strata))
        stop("stratification variable is needed when specifying site id")
    site_id = str_trim(as.character(site_id))
    if (!is_int(site_id))
      stop("site identifier must be an integer")
  })
  copy_from_env(environment(), env)
}

#'@name copy_from_env
#'@title Copy environments
#'@description Copy all entries in one environment and append to another.
#'@concept RedcapDqa
#'@rdname CopyEnvs
#'@param from Environment to copy from.
#'@param to Environment to append to.
#'@param objs Objects to copy. If not specified, this generalizes to all objects in the from environment
#'
#'@details This function copies objects from one environment to another.
#'
#'This helps in transfer of data as functions may be nested inside each other but may need to manipulate each other's data.
#'
#'This is not in line with FP due to side effects and mutations that can arise but we have implemented this for computational convinience and separation of concerns


copy_from_env <- function(from, to) {
  stopifnot(is.environment(from) && is.environment(to))
  objs <- ls(from, all.names=T)
  if (length(objs)) {
    for (o in objs) {
      to[[o]] <- from[[o]]
    }
  }
}

#'@name get_audit_ids
#'@title Get IDs to be used in audit
#'@description Get IDs to be used in the dqa audit process.
#'@concept RedcapDqa
#'@rdname GetAuditIds
#'@param env Environment of the calling function.
#'@seealso \code{\link{redcap_dqa}}
#'
#'@details This utility function gets the ids matching to the corresponding records that should be included in the validation process.
#'
#'Only these records will be be pulled from the repositories.
#'
#'This is convinient is case of large datasets or network latency
#'
#'This also separates the concerns of the main wrapper function in addition to making it leaner

get_audit_ids <- function(env) {
  copy_from_env(env, environment())
  suppressMessages({
    record_id = meta_data[1, 1]
    tmp <- record_id
    if (!isTRUE(str_trim(id_var) == str_trim(record_id)))
      tmp <- c(tmp, id_var)
    if (isTRUE(!data_missing(min_date)) || isTRUE(!data_missing(max_date))) 
      tmp <- c(tmp, date_var)
    if (isTRUE(!data_missing(site_id)))
      tmp <- c(tmp, strata)
    tmp <- paste0(tmp, collapse=",")
    dqa_ids = as.matrix(
      get_redcap_data(api = dqa_api_url, 
                      token = dqa_token,
                      local = dqa_local,
                      fields = tmp
      ))
    repo_ids = as.matrix(
      get_redcap_data(api = repo_api_url, 
                      token = repo_token,
                      local = repo_local,
                      fields = tmp
      ))
    if (!data_missing(min_date) && date_var %in% colnames(repo_ids)) {
      dqa_ids <- dqa_ids[sapply(dqa_ids[, date_var], is_date), ]
      dqa_ids <- dqa_ids[as.Date(dqa_ids[, date_var]) >= min_date, ]
    }
    if (!data_missing(max_date) && date_var %in% colnames(repo_ids)) {
      dqa_ids <- dqa_ids[sapply(dqa_ids[, date_var], is_date), ]
      dqa_ids <- dqa_ids[as.Date(dqa_ids[, date_var]) <= max_date, ]
    }
    repo_ids <- repo_ids[repo_ids[,record_id] %in% dqa_ids[,record_id], ]
    
    if (!data_missing(site_id) && strata %in% colnames(repo_ids)) {
      repo_ids <- repo_ids[str_trim(repo_ids[, strata]) %in% str_trim(site_id), ]
      dqa_ids <- dqa_ids[str_trim(dqa_ids[, strata]) %in% str_trim(site_id), ]
      strata <- NA
    }
    dqa_ids[, record_id] <- str_trim(dqa_ids[, record_id])
    repo_ids[, record_id] <- str_trim(repo_ids[, record_id])
    tmp = unique(intersect(dqa_ids[, record_id], repo_ids[, record_id]))
    if (0 == length(tmp))
      stop("No matching records found!")
    dqa_ids = dqa_ids[, record_id][which(dqa_ids[, record_id] %in% tmp)]
    repo_ids = repo_ids[,record_id][which(repo_ids[, record_id] %in% tmp)]
    if (length(dqa_ids) != length(repo_ids))
      stop("There are duplicates in identifier variable")
    ids_dqa = as.integer(na.omit(intersect(dqa_ids, repo_ids)))
  })
  copy_from_env(environment(), env)
}

#'@name get_audit_data
#'@title Get data to be used in audit
#'@description Get data to be used in the dqa audit process.
#'@concept RedcapDqa
#'@rdname GetAuditData
#'@param env Environment of the calling function.
#'@seealso \code{\link{redcap_dqa}}
#'
#'@details This utility function gets the data corresponding to the records that should be included in the validation process.
#'
#'Only these records will be be pulled from the repositories.
#'
#'This is convinient is case of large datasets or network latency
#'
#'This also separates the concerns of the main wrapper function in addition to making it leaner

get_audit_data <- function(env) {
  copy_from_env(env, environment())
  suppressMessages({
    dqa_data = get_redcap_data(api = dqa_api_url, 
                               token = dqa_token,
                               local = dqa_local,
                               ids_to_pull = ids_dqa
    )
    repo_data = get_redcap_data(api = repo_api_url, 
                                token = repo_token,
                                local = repo_local,
                                ids_to_pull = ids_dqa
    )
  })
  copy_from_env(environment(), env)
}

#'@name wrangle_audit_data
#'@title Wrangle audit data
#'@description Wrangle the data to be used for the audit process.
#'@concept RedcapDqa
#'@rdname WrangleAuditData
#'@param env Environment of the calling function.
#'@seealso \code{\link{redcap_dqa}}
#'
#'@details This utility function wrangles the data to be used in the validation process.
#'
#'This includes subsetting to only include fields included in both datasets in addition to the inclusion and exclusion of specific fields as specified in the wrapper function
#'
#'This also separates the concerns of the main wrapper function in addition to making it leaner

wrangle_audit_data <- function(env) {
  copy_from_env(env, environment())
  cols_dqa = intersect(colnames(dqa_data), colnames(repo_data))
  if (0 < length(fields_to_include) && any(tmp <- !sapply(fields_to_include, data_missing))) {
    fields_to_exclude <- NA
    fields_to_include <- fields_to_include[tmp]
    fields_to_include <- do.call(c, lapply(fields_to_include, function(x) grep(x, names(repo_data), v = T)))
    cols_dqa = cols_dqa[!cols_dqa %in% fields_to_include]
  }
  if (0 < length(fields_to_exclude) && any(tmp <- !sapply(fields_to_exclude, data_missing))) {
    fields_to_exclude <- fields_to_exclude[tmp]
    fields_to_exclude <- do.call(c, lapply(fields_to_exclude, function(x) grep(x, names(repo_data), v = T)))
    cols_dqa = cols_dqa[!cols_dqa %in% fields_to_exclude]
  }
  #if (is.element(id_var, cols_dqa))
  #  cols_dqa = cols_dqa[-grep(paste0("^", id_var, "$"), cols_dqa)]
  dqa_data = dqa_data[, cols_dqa]
  repo_data = repo_data[, cols_dqa]
  if (0 == nrow(dqa_data))
    stop("No records in DQA dataset")
  if (0 == nrow(repo_data))
    stop("No records in repo dataset")
  copy_from_env(environment(), env)
}

#'@name create_audit_object
#'@title Create RedcapDqa Audit Object
#'@description Create a prototype object abstracting away the dqa audit process.
#'@concept RedcapDqa
#'@rdname CreateAuditObject
#'@param env Environment of the calling function.
#'@seealso \code{\link{redcap_dqa}}
#'
#'@details This utility function creates the audit object from the other preceding steps in the wrapper function.
#'
#'This also separates the concerns of the main wrapper function in addition to making it leaner

create_audit_object <- function(env) {
  copy_from_env(env, environment())
  obj = new("RedcapDqa")
  obj@identifiers = ids_dqa
  obj@metaData = meta_data
  obj@dqaData = dqa_data
  obj@repoData = repo_data
  obj@strata = as.character(strata)
  copy_from_env(environment(), env)
}
