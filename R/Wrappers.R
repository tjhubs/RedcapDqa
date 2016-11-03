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
#'@param fields_to_exclude Fields to be excluded from the audit process. For example, timestamps. If fields to be included are specified, then this variable is ignored
#'@param fields_to_include Fields to be includes in the audit process. If included, then the fields to be excluded will be ignored.
#'@param id_var Field to be used to identify records across the two repositories.
#'@param min_date Minimum date used to constrain data.
#'@param max_date Maximum date used to constrain data.
#'@seealso \code{\link{RedcapDqa}}
#'@include RedcapDqaPackage.R
#'@include Helper.R
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
  fields_to_include = NA,
  fields_to_exclude = NA,
  id_var = NA,
  min_date = NA,
  max_date = NA,
  date_var = 'date_today',
  strata = NA,
  site_id = NA
) {
  validate_input(environment())
  get_audit_ids(environment())
  get_audit_data(environment())
  wrangle_audit_data(environment())
  create_audit_object(environment())
  invisible(obj)
}