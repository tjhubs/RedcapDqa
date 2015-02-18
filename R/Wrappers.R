#'@name redcap_dqa
#'@title Instantiate DQA
#'@description Wrapper function for instantiating DQA objects
#'@concept RedcapDqa
#'@return A \code{RedcapDqa} object loaded with data.
#'@rdname RedcapDqaWrapper
#'@export
#'@param repo_token Data repository's token
#'@param dqa_token DQA repository's token
#'@param repo_api_url Url to data repo's api
#'@param dqa_api_url Url to DQA repo's api
#'@param local Flag as to whether the repo's are local to the R instance
#'@param repo_local Flag as to whether the data repo is local to the R instance
#'@param dqa_local Flag as to whether the dqa repo is local to the R instance
#'@seealso \code{\link{RedcapDqa}}
#'@include RedcapDqaPackage.R
#'
#'@details This utility function instantiates the DQA process.
#'
#'It pulls the record ids from both repositories and matches the common identifiers.
#'
#'Data is then streamed from both repos and then matched for common fields.
#'
#'These datasets are used to create an instance of a \code{RedcapDqa} object.

redcap_dqa = function(
  repo_token,
  dqa_token,
  repo_api_url = "http://localhost/redcap/api/",
  dqa_api_url = repo_api_url,
  local = T,
  dqa_local = local,
  repo_local = local
  ) {
  suppressMessages({
    id_var = get_redcap_data(api = repo_api_url, 
                             token = repo_token, 
                             content = "metadata", 
                             local = repo_local,
    )[1, 1]
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
    ids_dqa = intersect(dqa_ids, repo_ids)
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
  cols_dqa = intersect(colnames(dqa_data), colnames(repo_data))
  if (is.element(id_var, cols_dqa))
    cols_dqa = cols_dqa[-grep(id_var, cols_dqa)]
  dqa_data = dqa_data[, cols_dqa]
  repo_data = repo_data[, cols_dqa]
  obj = new("RedcapDqa")
  obj@identifiers = ids_dqa
  obj@dqaData = dqa_data
  obj@repoData = repo_data
  invisible(obj)
}