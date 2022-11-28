usePackage <- function(p)
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
pckg <- c("data.table", "jsonlite", "httr", "keyring", "ggthemes")

# In case one or more of the packages are not installed they will be installed
sapply(pckg, usePackage)

library(data.table)
library(ggplot2)

read.excel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

#******************************************************************************
#* Wrapping relevant EnvisionRisk Market Risk-as-a-Service API calls into R-functions
#******************************************************************************


