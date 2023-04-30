#' This is an init file for the system. It is mostly meant for deploying to a server. 
#' This file makes sure all the needed packages are installed.

###########################################################

# Update this line with the R packages to install:
my_packages = c("shiny", 
                "shinydashboard", 
                "shinyWidgets", 
                "shinydashboardPlus",
                "config",
                "caTools",
                "XML",
                "rJava",
                "rsample", 
                "tidyverse",
                "tidytext",
                "tm",
                "qdap",
                "SnowballC",
                "text2vec",
                "lsa",
                "glmnet",
                "caret",
                "AUC",
                "DMwR", 
                "waffle", 
                "anytime", 
                "wesanderson", 
                "tidyverse", 
                "ggthemes", 
                "vip", 
                "highcharter", 
                "jsonlite", 
                "httr", 
                "openxlsx",
                "DT",
                "compareDF",
                "htmlTable",
                "e1071",
                "pool",
                "odbc",
                "d3Tree"
)

###########################################################


install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
  else {
    cat(paste("Skipping already installed package:", p, "\n"))
  }
}
invisible(sapply(my_packages, install_if_missing))

if ("shinyPagerUI" %in% rownames(installed.packages()) == FALSE) {
  devtools::install_github('wleepang/shiny-pager-ui')
}