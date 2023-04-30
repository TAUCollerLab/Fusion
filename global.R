#' This file holds all the globally loaded variables. 
#' It also loads all the different modules of the system.

library(config)
db_config <- config::get("dataconnection")

## System Modules
#source("components/lime.R")
source("components/global_models.R")
source("components/simple_model_building.R")
source("components/model_building.R")
source("components/data_acquisition.R")
source("components/lexicon_building.R")

if (db_config$driver=='SQLite3'){
  source("components/db.R")
} else {
  source("components/db_postgres.R")
}

source("components/projects.R")
# source("components/concepts.R")
# source("components/text_visualization.R")
source("components/reflection.R")
source("components/new_insights.R")
# source("components/tools.R")
source("components/about.R")
## ML models
source("components/models/supervised_smart_lexicon.R")
source("components/models/supervised_lexicon.R")
source("components/models/unsupervised_lexicon.R")
source("components/models/unsupervised_smart_lexicon.R")
source("components/models/baseline.R")


GlobalModelSeed = 1234
## This loads a preprocessed version of the wikipedia enhancement corpus. 
load(file = "www/wikipedia_enhanced.RData")

## initialize DB if none was created yet
if (!dbExistsTable(appDB, "users")){
  initializeDBTables(appDB)
}

### This is a temporary hack for when we only have one user in the system. We don't have yet user management interface.
### It should be removed from the 'global' file later, in production, and managed dynamically per each user
current_user_id = reactiveVal(1)

user_projects <-getProjects(1)
if (is.null(user_projects)){
  current_project_id = reactiveVal(0)
} else {
  ## if user has existing projects, select the first one, otherwise select ID=0 which marks it as "no projects existing"
  if (nrow(user_projects)==0){
    current_project_id = reactiveVal(0)
  } else {
    current_project_id = reactiveVal(user_projects$id[1])
  }
}