# RedcapData: Data Quality Assurance of Redcap Repositories

## Preface

> Data quality is an important albeit often ignored aspect of data management. 

> Data entry should be validated and a corresponding error report generated to strengthen the validation process. This should also be supported by periodic DQA. 

> During DQA, records are sampled after which data is entered into a separate repository. The next step is to perform concordance analysis to verify whether data was properly captured. In this case, the DQA repository serves as the gold standard.

## Package Details

### Dependencies

> This package depends on the **RedcapData** package. This is used to pull data from REDCap.

> To install this package:

```r
  if (!is.element("devtools", .packages(all=T))) install.packages("devtools")
  if (!is.element("RedcapData", .packages(all=T))) install_github("bonmac/RedcapData")
```

### Installation

> To install this package:

```r
if (!is.element("RedcapDqa", .packages(all=T))) install_github("bonmac/RedcapDqa")
```

### Example

> This is a simple example. Provide the correct tokens and api urls.

```r

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

```