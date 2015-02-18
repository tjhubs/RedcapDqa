% Package Documentation

\name{RedcapDqaPackage}
\alias{RedcapDqaPackage}
\docType{package}
\title{
Data Quality Assurance of Redcap Repositories
}
\description{
Data Quality Assurance of Redcap Repositories
}
\details{
\tabular{ll}{
Package: \tab RedcapDqa\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-02-18\cr
License: \tab GPL >= 2\cr
}
Initialize DQA process using \code{\link{redcap_dqa}} function. This loads data into memory pulling only the record ids that match for both datasets. Save this to an object, say \code{dqaObj}.

Use the dqa object to perform concordance analysis using its member function \code{check_concordance}. For example \code{dqaObj$check_concordance()}. This returns a \code{dqaResults} object. you can then view the results using \code{\link{print}} or \code{\link{summary}}

}
\author{
Boniface Makone

Maintainer: Boniface Makone <bonifacemakone@gmail.com>
}

\code{\link[Github]{https://github.com/bonmac/RedcapDqa}}

\examples{
library(RedcapDqa)

dqaObj = redcap_dqa(
  repo_token = "repo-token", 
  dqa_token = "dqa-token", 
  repo_api_url = "http://localhost/redcap/api/", 
  local = F
  )

dqaRes = audit(dqaObj)
dqaRes
summary(dqaRes)
}
