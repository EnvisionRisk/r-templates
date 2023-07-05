options(scipen=999)

usePackage <- function(p)
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

# In case one or more of the packages are not installed they will be installed
pckg <- c("devtools", "usethis", "data.table", "jsonlite", "httr", "rstudioapi", "stringi")
sapply(pckg, usePackage)

# EnvisionRiskRaaS
if(!is.element("EnvisionRiskRaaS", installed.packages()[,1])){
  require(devtools)
  devtools::install_github("EnvisionRiskRaaS")
}

# Helper functions
read.excel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

IdGenerator <- function(n, pattern_len = c(2, 4, 2), pattern = c("[A-Z]", "[0-9]", "[a-z]")){
  stopifnot(length(pattern_len) == length(pattern))
  do.call(paste0, base::Map(stringi::stri_rand_strings, 
                            n = n, 
                            length = pattern_len, 
                            pattern = pattern))
}

# get_script_path <- function(){
#   script_path <- rstudioapi::getSourceEditorContext()$path 
#   script_path <- sub('[^/]+$', '', script_path)
#   return(script_path)
# }

#usethis::use_course('https://github.com/envisionrisk/r-templates/archive/main.zip', destdir = getwd())