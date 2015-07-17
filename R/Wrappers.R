#'@name redcap_dqa
#'@title Instantiate DQA object
#'@description Wrapper function for instantiating DQA objects
#'@concept RedcapDqa
#'@return A \code{RedcapDqa} object loaded with data.
#'@rdname RedcapDqaWrapper
#'@export
#'@param repo_file_location Repository data file location. If specified, data is not pulled from the repository.
#'@param dqa_file_location DQA data file location. If specified, data is not pulled from the repository.
#'@param repo_token Data repository's token.
#'@param dqa_token DQA repository's token.
#'@param repo_api_url Url to data repo's api.
#'@param dqa_api_url Url to DQA repo's api.
#'@param local Flag as to whether the repo's are local to the R instance.
#'@param repo_local Flag as to whether the data repo is local to the R instance.
#'@param dqa_local Flag as to whether the dqa repo is local to the R instance.
#'@param fields_to_exclude Fields to be excluded from the audit process. For example, timestamps.
#'@param id_var Field to be used to identify records across the two repositories.
#'@param min_date Minimum date used to constrain data.
#'@param max_date Maximum date used to constrain data.
#'@seealso \code{\link{RedcapDqa}}
#'@include RedcapDqaPackage.R
#'
#'@details This utility function instantiates the DQA process.
#'
#'Data is either pulled from the repositories or read in from files.
#'
#'The datasets are then matched for common fields and identifiers.
#'
#'The data and other metadata relevant to the audit process are then stored into an object for consequent analysis.
#'
#'This results to an instance of a \code{RedcapDqa} object.

redcap_dqa = function(
  repo_file_location = NA,
  dqa_file_location = NA,
  meta_file_location = NA,
  repo_token = NA,
  dqa_token = NA,
  repo_api_url = "http://localhost/redcap/api/",
  dqa_api_url = repo_api_url,
  local = T,
  dqa_local = local,
  repo_local = local,
  fields_to_exclude = NA,
  id_var = NA,
  min_date = NA,
  max_date = NA,
  date_var = 'date_today',
  strata = NA
) {
  if (!is_date(min_date)) stop('invalid minimum date')
  if (!is_date(max_date)) stop('invalid maximum date')
  if (1 < length(date_var)) {
    warning('only one date variable needed. taking the first value')
    date_var = date_var[1]
  }
  if (1 < length(min_date)) {
    warning('only one minimum date needed. taking the first value')
    min_date = min_date[1]
  }
  if (1 < length(max_date)) {
    warning('only one maximum date needed. taking the first value')
    max_date = max_date[1]
  }
  if (1 < length(id_var)) {
    warning('only one identifier variable needed. taking the first value')
    id_var = id_var[1]
  }
  min_date = as.Date(min_date); max_date = as.Date(max_date)
  if (!is.na(min_date) && !is.na(max_date) && 
      min_date > max_date) 
    stop('minimum date greater than maximum date')
  if (all(sapply(strata, is.na)))
    strata = NA
  suppressMessages({
    if (!is.na(meta_file_location)) {
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
    if (!is.na(id_var)) {
      if (!grepl(id_var, meta_data[, 1]))
        stop("identifier variable not in metadata")
    } else {
      id_var = meta_data[1, 1]
    }
    record_id = meta_data[1, 1]
    if (isTRUE(str_trim(id_var) == str_trim(record_id))) {
      dqa_ids = as.integer(unlist(
        get_redcap_data(api = dqa_api_url, 
                        token = dqa_token,
                        local = dqa_local,
                        fields = id_var
                        )))
        repo_ids = as.integer(unlist(
          get_redcap_data(api = repo_api_url, 
                          token = repo_token,
                          local = repo_local,
                          fields = id_var
                          )))
    } else {
      browser()
      tmp = paste(id_var, record_id, sep = ",")
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
      tmp = unique(intersect(dqa_ids[, 1], repo_ids[, 1]))
      dqa_ids = dqa_ids[, 2][order(which(dqa_ids[, 1] %in% tmp))]
      repo_ids = repo_ids[, 2][order(which(repo_ids[, 1] %in% tmp))]
      if (length(dqa_ids) != length(repo_ids))
        stop("There are duplicates in idenfifier variable")
      
    }
    ids_dqa = intersect(dqa_ids, repo_ids)
    dqa_data = get_redcap_data(api = dqa_api_url, 
                               token = dqa_token,
                               local = dqa_local,
                               ids_to_pull = ids_dqa
    )
    if (!is.na(strata) && !all(strata %in% names(dqa_data))) 
      stop('strata variable(s) not in DQA dataset')
    if (!date_var %in% names(dqa_data)) stop('date variable not in DQA dataset')
    date_var_tmp = dqa_data[, date_var]
    if (!all(sapply(date_var_tmp, is_date))) stop('invalid date variable')
    if (!is.na(min_date)) dqa_data = dqa_data[date_var_tmp >= min_date, ]
    if (!is.na(max_date)) dqa_data = dqa_data[date_var_tmp <= max_date, ]
    repo_data = get_redcap_data(api = repo_api_url, 
                                token = repo_token,
                                local = repo_local,
                                ids_to_pull = ids_dqa
    )
    if (!is.na(strata) && 
          !all(strata %in% names(repo_data))) 
      stop('some strata variable(s) not in repository dataset')
    if (!date_var %in% names(repo_data)) stop('date variable not in repository dataset')
    date_var_tmp = repo_data[, date_var]
    if (!all(sapply(date_var_tmp, is_date))) stop('invalid date variable')
    if (!is.na(min_date)) repo_data = repo_data[date_var_tmp >= min_date, ]
    if (!is.na(max_date)) repo_data = repo_data[date_var_tmp <= max_date, ]
  })
  cols_dqa = intersect(colnames(dqa_data), colnames(repo_data))
  if (!is.na(fields_to_exclude) && 0 < length(fields_to_exclude)) {
    fields_to_exclude <- do.call(c, lapply(fields_to_exclude, function(x) grep(x, names(repo_data), v = T)))
    cols_dqa = cols_dqa[!cols_dqa %in% fields_to_exclude]
  }
  if (is.element(id_var, cols_dqa))
    cols_dqa = cols_dqa[-grep(paste0("^", id_var, "$"), cols_dqa)]
  dqa_data = dqa_data[, cols_dqa]
  repo_data = repo_data[, cols_dqa]
  if (0 == nrow(dqa_data))
    stop("No records in DQA dataset")
  if (0 == nrow(repo_data))
    stop("No records in repo dataset")
  obj = new("RedcapDqa")
  obj@identifiers = ids_dqa
  obj@metaData = meta_data
  obj@dqaData = dqa_data
  obj@repoData = repo_data
  obj@strata = as.character(strata)
  invisible(obj)
}