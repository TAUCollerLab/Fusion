#' This is the DB storage module. It is responsible to store and retrieve any data needed for
#' the application. Relatively easily we can switch DB technologies (SQLite to Postgres for example) by just changing this file.



library(odbc)
library(pool)


#' Opens a pooled-connection and creates a database using the config variables from config.yml file.
#' 
appDB <- dbConnect(odbc::odbc(),
                   Driver = db_config$driver,
                   Server = db_config$server,
                   UID    = db_config$uid,
                   PWD    = db_config$pwd,
                   Port   = db_config$port,
                   Database = db_config$database,
                   sslmode = 'require'
)



#' This function initializes a local DB file. It builds all the needed tables (as empty tables).
#' For reference on which tables are created, look at the Entity Relationship diagram in the
#' design document. This function should be called once per new SQLite DB file.
#' 
#' @param db_connection a DBI connection object
#' 
#' @return NULL
#' 
initializeDBTables <- function(db_connection){
  
  ## creates the 'users' table
  db_response <- dbExecute(db_connection, "CREATE TABLE IF NOT EXISTS `users` (
                            `id` INTEGER PRIMARY KEY AUTOINCREMENT,
                            `full_name` VARCHAR(255),
                            `email` VARCHAR(320),
                            `role` INTEGER DEFAULT 1,
                            `created_at` TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                        )")
  
  ### temporarily add fake user accounts
  db_response <- dbExecute(db_connection, "INSERT INTO users (full_name, email, role) VALUES ('User', 'user@gmail.com', 1)")
  ######################################
  
  ## creates the 'data' table
  db_response <- dbExecute(db_connection, "CREATE TABLE IF NOT EXISTS `data` (
                            `id` INTEGER PRIMARY KEY AUTOINCREMENT,
                            `serialized_data` BLOB NOT NULL,
                            `serialized_enhancement_corpus` BLOB,
                            `positive_label` TEXT,
                            `negative_label` TEXT,
                            `created_at` TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                        )")
  
  ## creates the 'lexicons' table
  db_response <- dbExecute(db_connection, "CREATE TABLE IF NOT EXISTS `lexicons` (
                            `id` INTEGER PRIMARY KEY AUTOINCREMENT,
                            `serialized_lexicon` BLOB,
                            `project_id` INTEGER,
                            `created_at` TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                            CONSTRAINT fk_project_id
                            FOREIGN KEY (project_id)
                            REFERENCES projects(id)
                        )")
  
  ## creates the 'concepts' table
  db_response <- dbExecute(db_connection, "CREATE TABLE IF NOT EXISTS `concepts` (
                            `id` INTEGER PRIMARY KEY AUTOINCREMENT,
                            `name` VARCHAR(255),
                            `description` TEXT,
                            `rule` TEXT,
                            `project_id` INTEGER,
                            `created_at` TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                            CONSTRAINT fk_project_id
                            FOREIGN KEY (project_id)
                            REFERENCES projects(id)
                        )")

  ## creates the 'projects' table
  db_response <- dbExecute(db_connection, "CREATE TABLE IF NOT EXISTS `projects` (
                            `id` INTEGER PRIMARY KEY AUTOINCREMENT,
                            `user_id` INTEGER,
                            `name` VARCHAR(255),
                            `description` TEXT,
                            `data_id` INTEGER,
                            `lexicon_id` INTEGER,
                            CONSTRAINT fk_users
                            FOREIGN KEY (user_id)
                            REFERENCES users(id),
                            CONSTRAINT fk_data
                            FOREIGN KEY (data_id)
                            REFERENCES data(id),
                            CONSTRAINT fk_lexicon
                            FOREIGN KEY (lexicon_id)
                            REFERENCES lexicons(id)
                        )")
  
  
  ## creates the 'models' table
  db_response <- dbExecute(db_connection, "CREATE TABLE IF NOT EXISTS `models` (
                            `id` INTEGER PRIMARY KEY AUTOINCREMENT,
                            `user_id` INTEGER,
                            `project_id` INTEGER,
                            `model_type` TEXT,
                            `serialized_model` BLOB NOT NULL,
                            CONSTRAINT fk_users
                            FOREIGN KEY (user_id)
                            REFERENCES users(id),
                            CONSTRAINT fk_projects
                            FOREIGN KEY (project_id)
                            REFERENCES projects(id)
                        )")
  
  ## creates the 'model_run_info' table
  db_response <- dbExecute(db_connection, "CREATE TABLE IF NOT EXISTS `model_run_info` (
                            `model_id` INTEGER,
                            `iteration_number` INTEGER DEFAULT 1,
                            `auc` REAL,
                            `leave_out_auc` REAL,
                            `serialized_confusion_matrix` TEXT,
                            `serialized_assessment_set` BLOB,
                            `serialized_word_scores` BLOB,
                            `lexicon_id` INTEGER,
                            `created_at` TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                            CONSTRAINT fk_models
                            FOREIGN KEY (model_id)
                            REFERENCES models(id),
                            CONSTRAINT fk_lexicons
                            FOREIGN KEY (lexicon_id)
                            REFERENCES lexicons(id)
                        )")
  return(NULL)
}



#' Returns a data frame with all the projects of the given user.
#' 
#' @param user_id the user_id of the user for the request
#' 
#' @return a data frame with all the projecst associated to this user_id. Returns NULL if
#' table 'projects' doesn't exist.
#' 
getProjects <- function(user_id){
  if (!dbExistsTable(appDB, "projects")){
    return(NULL)
  }
  db_response <- dbGetQuery(appDB, paste("SELECT * FROM projects WHERE user_id==", user_id, sep = ""))
  return(db_response)
}



#' Adds a new project that will be assigned to the given user ID.
#' 
#' @param user_id
#' @param project_name
#' @param project_description
#' 
#' @return NULL
#' 
addNewProject <- function(user_id, project_name, project_description){
  project <- list(
    user_id,
    name = project_name,
    description = project_description
  )
  
  sql_query <- paste("INSERT INTO projects (user_id, name, description)
                     VALUES (?, ?, ?)",
                     sep = "")
  
  db_response <- dbExecute(appDB, sql_query, project)
  inserted_row <- dbGetQuery(appDB, "SELECT * FROM projects WHERE id==last_insert_rowid()")
  return(inserted_row$id)
}



#' Returns user info, such as name, email, created_at, for a given user_id.
#' 
#' @param user_id the user_id of the user for the request
#' 
#' @return a data frame that holds user's info for the given user_id. If such user_id doesn't
#' exist, it will return an empty data frame (with 0 rows). If table 'users' doesn't exist it
#' will return NULL
#' 
getUserInfo <- function(user_id){
  if (!dbExistsTable(appDB, "users")){
    return(NULL)
  }
  user_info <- dbGetQuery(appDB, paste("SELECT id, full_name, email, role, datetime(created_at,'localtime') as created_at 
                                       FROM users WHERE id == ", user_id, sep = ""))
  return(user_info)
}



#' Returns the data stored in the database based on given data ID. There is a different function below that can
#' get data based on project_id.
#' 
#' @param data_id
#' 
#' @return a list with the data items (i.e., data, corpus, and created_at) saved for the 
#' given project. Returns NULL if 'data' table doesn't exist, or if this project doesn't have any data associated to it.
#' 
getDataByID <- function(data_id){
  if (!dbExistsTable(appDB, "data")){
    return(NULL)
  }
  ## the data was stored as a serialized raw data, thus it needs to be unserialized first
  serialized <- dbGetQuery(appDB, paste("SELECT id, serialized_data, serialized_enhancement_corpus, positive_label, negative_label, datetime(created_at,'localtime') as created_at 
                                        FROM data WHERE id == ", data_id, sep = ""))
  sdata <- unserializeObject(serialized$serialized_data)
  scorpus <- unserializeObject(serialized$serialized_enhancement_corpus)
  
  l <- list(
    data = sdata,
    corpus = scorpus,
    classification_labels = tibble(
      positive = serialized$positive_label,
      negative = serialized$negative_label
    ),
    created_at = serialized$created_at
  )
  return(l)
}



#' Returns the lexicon stored in the database based on given lexicon ID. There is another (different) function below that can
#' get lexicon based on project_id.
#' 
#' @param data_id
#' 
#' @return a list with the lexicon attributes (i.e., lexicon dataframe and created_at) saved for the given project. 
#' Returns NULL if 'lexicons' table doesn't exist, or if this project doesn't have any lexicons associated to it.
#' 
getLexiconByID <- function(lexicon_id){
  if (!dbExistsTable(appDB, "lexicons")){
    return(NULL)
  }
  ## the lexicon was stored as a serialized raw data, thus it needs to be unserialized first
  serialized <- dbGetQuery(appDB, paste("SELECT id, serialized_lexicon, project_id, datetime(created_at,'localtime') as created_at 
                                        FROM lexicons WHERE id == ", lexicon_id, sep = ""))
  slexicon <- unserializeObject(serialized$serialized_lexicon)
  
  l <- list(
    lexicon = slexicon,
    created_at = serialized$created_at
  )
  return(l)
}


#' Returns a boolean value whether a project has data associated to it or not.
#' 
#' @param project_id
#' 
#' @return returns TRUE if the given project has data associated to it, and FALSE otherwise.
#' Also returns FALSE if the table 'projects' is empty.
#' 
doesProjectDataExist <- function(project_id){
  if (!dbExistsTable(appDB, "projects")){
    return(FALSE)
  }
  project_info <- dbGetQuery(appDB, paste("SELECT data_id FROM projects WHERE id == ", project_id, sep = ""))
  if (nrow(project_info)==0){
    return(FALSE)
  }
  inquiry <- ifelse(is.na(project_info$data_id), 
                    FALSE, 
                    TRUE
                    )
  return(inquiry)
}



#' Returns the data associated with the given project. A different function can return
#' based on data_id.
#' 
#' @param project_id
#' 
#' @return a list with the data items (i.e., data, lexicon, and created_at) saved for the 
#' given project. Returns NULL if 'projects' or 'data' tables don't exist, or if this 
#' project don't have any data associated to it.
#' 
getProjectData <- function(project_id){
  if (!dbExistsTable(appDB, "projects")){
    return(NULL)
  }
  project_info <- dbGetQuery(appDB, paste("SELECT data_id FROM projects WHERE id == ", project_id, sep = ""))
  data_id <- project_info$data_id
  
  if (length(data_id)==0){
    return(NULL)
  }
  
  if (is.na(data_id)){
    return(NULL)
  }
  return(getDataByID(data_id))
}



#' Returns the lexicon associated with the given project. A different function can return
#' based on lexicon_id.
#' 
#' @param project_id
#' 
#' @return a list with the lexicon attributes (i.e., lexicon and created_at) saved for the 
#' given project. Returns NULL if 'projects' or 'lexicons' tables don't exist, or if this 
#' project don't have any lexicons associated to it.
#' 
getProjectLexicon <- function(project_id){
  lexicon_id <- getProjectLexiconID(project_id)
  
  if (is.null(lexicon_id)){
    return(NULL)
  }
  return(getLexiconByID(lexicon_id))
}



#' Returns the lexicon_id (not the lexicon itself) associated with the given project.
#' 
#' @param project_id
#' 
#' @return a lexicon_id for the lexicon currntly assigned to this project, or NULL if 'projects' table doesn't exist
#' or if no lexicon is assigned to the project.
#' 
getProjectLexiconID <- function(project_id){
  if (!dbExistsTable(appDB, "projects")){
    return(NULL)
  }
  project_info <- dbGetQuery(appDB, paste("SELECT lexicon_id FROM projects WHERE id == ", project_id, sep = ""))
  lexicon_id <- project_info$lexicon_id
  
  if (is.na(lexicon_id)){
    return(NULL)
  }
  return(lexicon_id)
}



#' Returns all the lexicons associated with the given project. Note that the lexion object itself will be serialized --
#' use the helper function below to unserialize it.
#' 
#' @param project_id
#' 
#' @return a dataframe with all the lexicons that belong to this project, or NULL if no lexicons exist or table doesn't exist.
#' 
getAllLexiconsInProject <- function(project_id){
  if (!dbExistsTable(appDB, "lexicons")){
    return(NULL)
  }
  lexicons_in_project <- dbGetQuery(appDB, paste("SELECT id, serialized_lexicon, project_id, datetime(created_at,'localtime') as created_at FROM lexicons WHERE project_id == ", project_id, sep = ""))
  
  return(lexicons_in_project)
}



#' Assigns the given data from 'work_env' to the given project_id, and stores it in the database.
#' 
#' @param project_id the project_id to assign the data to
#' @param work_env the work environment to take the data from (data, external corpus, etc.)
#' 
#' @return the data_id index of the assigned data
#' 
setProjectData <- function(project_id, work_env){
  # first the data has to be serialized in order to be saved inside a table row
  serialized <- list(data=I(list(serialize(work_env$data(), NULL))),
                           corpus=I(list(serialize(work_env$corpus(), NULL))),
                           positive = work_env$classification_labels$positive,
                           negative = work_env$classification_labels$negative
  )
  
  # inserts the data in 'data' table
  sql_query <- paste("INSERT INTO data (serialized_data, serialized_enhancement_corpus, positive_label, negative_label) VALUES (?, ?, ?, ?)",
                     sep = "")
  db_response <- dbExecute(appDB, sql_query, serialized)
  
  # updates the 'projects' table to be aware that a new data has been assigned to this project
  inserted_row <- dbGetQuery(appDB, "SELECT * FROM data WHERE id==last_insert_rowid()")
  db_response <- dbExecute(appDB, paste("UPDATE projects SET data_id = ", inserted_row$id, " WHERE id == ", project_id, sep = ""))
  return(inserted_row$id)
}



#' Assigns the given lexicon to the given project_id, and stores it in the database.
#' 
#' @param project_id the project_id to assign the data to
#' @param lexicon
#' 
#' @return the lexicon_id index of the assigned lexicon
#' 
addLexiconToProject <- function(project_id, lexicon){
  # first the lexicon has to be serialized in order to be saved inside a table row
  serialized <- list(lexicon_in=I(list(serialize(lexicon, NULL))),
                           project_id=project_id
  )
  
  # inserts the data in 'data' table
  sql_query <- paste("INSERT INTO lexicons (serialized_lexicon, project_id) VALUES (?, ?)",
                     sep = "")
  db_response <- dbExecute(appDB, sql_query, serialized)
  
  # updates the 'projects' table to be aware that new data has been assigned to this project
  inserted_row <- dbGetQuery(appDB, "SELECT * FROM lexicons WHERE id==last_insert_rowid()")
  switchProjectLexicon(project_id, inserted_row$id)
  return(inserted_row$id)
}



#' A function that switches between the lexicons that exist in a project to the given lexicon_id. This helps
#' to switch between various version of lexicons in the project (e.g., revert to an older version). 
#' The lexicons must have been added to the project prior to running this function. The function doesn't verify if the
#' lexicon_id given is valid.
#' 
#' @param project_id
#' @param newly_assigned_lexicon_id
#' 
#' @return NULL
#' 
switchProjectLexicon <- function(project_id, newly_assigned_lexicon_id){
  # updates the 'projects' table to be aware that a different lexicon_id has been assigned to this project
  db_response <- dbExecute(appDB, paste("UPDATE projects SET lexicon_id = ", newly_assigned_lexicon_id, " WHERE id == ", project_id, sep = ""))
  return(NULL)
}



#' Function that stores the given model run info (iteration, auc, confusion_matrix, etc...) 
#' in an aggregated table 'model_run_info'. If table exists it will append data to it, if not
#' it will create it.
#' 
#' @param model_id
#' @param iteration_number
#' @param auc
#' @param leave_out_auc
#' @param confusion_matrix
#' @param assessment_set
#' @param word_scores
#' @param lexicon_id
#' 
#' @return NULL
#' 
storeModelRunInfo <- function(model_id, iteration_number, auc, leave_out_auc, confusion_matrix, assessment_set, word_scores, lexicon_id){
  
  ## prepare the info first, serialize the confusion matrix as it a complex object
  model_info <- list(
    model_id,
    iteration_number,
    auc,
    leave_out_auc,
    serialized_confusion_matrix = rawToChar(serialize(confusion_matrix, NULL, ascii = TRUE)),
    serialized_assessment_set = I(list(serialize(assessment_set, NULL))),
    serialized_word_scores = I(list(serialize(word_scores, NULL))),
    lexicon_id = ifelse(is.null(lexicon_id), NA, lexicon_id)
  )
  
  sql_query <- paste("INSERT INTO model_run_info (model_id, iteration_number, auc, leave_out_auc, serialized_confusion_matrix, serialized_assessment_set, serialized_word_scores, lexicon_id) 
                     VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
                     sep = "")
  db_response <- dbExecute(appDB, sql_query, model_info)
  return(NULL)
}



#' Function that deletes a model and all of its run info based on the model_id given.
#' 
#' @param model_id
#'  
#' @return NULL
#'   
deleteModel <- function(model_id){
  ## delete model run-infos that belong to this model
  sql_query <- paste("DELETE FROM model_run_info WHERE model_id == ", 
                     model_id,
                     sep = "")
  db_response <- dbExecute(appDB, sql_query)
  
  ## delete the model itself
  sql_query <- paste("DELETE FROM models WHERE id == ", 
                     model_id,
                     sep = "")
  db_response <- dbExecute(appDB, sql_query)
  
  return(NULL)
}



#' Function that deletes all models and all of their run infos for a specific project. 
#' This is useful when a user wants to reset the project (for example when uploading new data into a project).
#'  
#' @param project_id
#'  
#' @return NULL
#'   
deleteAllModels <- function(project_id){
  ## first, find all the model_ids that belong to this project
  model_ids <- getAllModelsRunInfos(project_id)$model_id
  
  ## delete model run-infos that belong to the models that are part of the project
  sql_query <- paste("DELETE FROM model_run_info WHERE model_id IN ('", 
                     paste(model_ids, collapse = "','"),
                     "')",
                     sep = "")
  db_response <- dbExecute(appDB, sql_query)
  
  ## delete the models that are part of the project
  sql_query <- paste("DELETE FROM models WHERE project_id == ", 
                     project_id,
                     sep = "")
  db_response <- dbExecute(appDB, sql_query)
  
  return(NULL)
}



#' Function that stores the actual model (not the model run info) and the model type.
#' 
#' @param user_id the user that is associated to this model
#' @param project_id the project that is associated to this model
#' @param model_type text string that describes the model type
#' @param model the actual model object
#' 
#' @return the id index of the newly inserted model
#' 
storeModel <- function(user_id, project_id, model_type, model){
  
  model_info <- list(
    user_id,
    project_id,
    model_type,
    model = I(list(serialize(model, NULL)))
  )
  
  sql_query <- paste("INSERT INTO models (user_id, project_id, model_type, serialized_model) 
                     VALUES (?, ?, ?, ?)",
                     sep = "")
  db_response <- dbExecute(appDB, sql_query, model_info)
  inserted_row <- dbGetQuery(appDB, "SELECT * FROM models WHERE id==last_insert_rowid()")
  return(inserted_row$id)
}



#' Function that updates the stored model (not the model run info).
#' 
#' @param model_id the id of the model to update
#' @param model the new model object to update to
#' 
#' @return the id index of the newly inserted model
#' 
updateModel <- function(model_id, model){
  new_model_info <- data.frame(
    in_model = I(list(serialize(model, NULL))),
    in_model_id = model_id
  )
  sql_query <- paste("UPDATE models SET serialized_model = ? WHERE id = ?",
                     sep = "")
  db_response <- dbExecute(appDB, sql_query, new_model_info)
  inserted_row <- dbGetQuery(appDB, "SELECT * FROM models WHERE id==last_insert_rowid()")
  return(inserted_row$id)
}



#' Function that returns the actual model (not the model run info) that is stored in the database. If no such model exist, returns NULL.
#' 
#' @param model_id the model id of the model to return. 
#' 
#' @return model object, or NULL if model not found or table doesn't exist
#' 
getModel <- function(model_id){
  if (!dbExistsTable(appDB, "models")){
    return(NULL)
  }
  sql_query <- paste("SELECT *
                      FROM models
                      WHERE id == ", model_id, sep = "")
  model_record_from_db <- dbGetQuery(appDB, sql_query)
  unserialized_model <- unserializeObject(model_record_from_db$serialized_model)
  return(unserialized_model)
}



getModelType <- function(model_id){
  if (!dbExistsTable(appDB, "models")){
    return(NULL)
  }
  sql_query <- paste("SELECT model_type
                      FROM models
                      WHERE id == ", model_id, sep = "")
  model_record_from_db <- dbGetQuery(appDB, sql_query)
  return(model_record_from_db$model_type)
}



#' Function that retrieves the information about the model runs for a given model (e.g., auc, 
#' confusion matrix, iteration number, etc...).
#' 
#' @param model_id ID of model. Only model run information for this specific model will be 
#' returned.
#' 
#' @return dataframe if model information exists, or NULL otherwise. 
#' 
getModelRunInfo <- function(model_id){
    if (!dbExistsTable(appDB, "models") || !dbExistsTable(appDB, "model_run_info")){
      return(NULL)
    }
    sql_query <- paste("SELECT model_id, iteration_number, auc, leave_out_auc, serialized_confusion_matrix, serialized_assessment_set, serialized_word_scores, lexicon_id, datetime(created_at,'localtime') as created_at, model_type
    FROM model_run_info
    INNER JOIN models ON model_run_info.model_id = models.id
    WHERE model_id == ", model_id, sep = "")
    model_run_info <- dbGetQuery(appDB, sql_query)
    
    return(model_run_info)
}



#' Function that retrieves the information about the model-runs for a given Project (note: 
#' that the function above is for a specific model, while this one is for all models in project).
#' 
#' @param project_id ID of project. All model-runs' information for this specific project will be 
#' returned.
#' 
#' @return dataframe if project and model information exists, or NULL otherwise. 
#' 
getAllModelsRunInfos <- function(project_id){
  if (!dbExistsTable(appDB, "models") || !dbExistsTable(appDB, "model_run_info")){
    return(NULL)
  }
  sql_query <- paste("SELECT model_id, iteration_number, auc, leave_out_auc, serialized_confusion_matrix, serialized_assessment_set, serialized_word_scores, lexicon_id, datetime(created_at,'localtime') as created_at, model_type
  FROM model_run_info
  INNER JOIN models ON model_run_info.model_id = models.id
  WHERE project_id == ", project_id, sep = "")
  model_run_info <- dbGetQuery(appDB, sql_query)
  
  return(model_run_info)
}



#' This is a helper function provided for other modules to help them easily unserialize the given object 
#' (e.g., a serialiezed confusion matrix or a serialized model)
#' 
#' @param serialized_object
#' 
#' @return NULL
#' 
unserializeObject <- function(serialized_object){
  ## since the serialization function works differently based on object type, 
  ## check if the object is a serialized string (thus representing some basic object) or a list (representing a complex object like a model)
  if (typeof(serialized_object)=="character"){
    return(unserialize(charToRaw(as.character(serialized_object))))
  }
  if (typeof(serialized_object)=="list"){
    return(lapply(serialized_object, 'unserialize')[[1]])
  }
  return(NULL)
}



#' Returns the concepts stored in the database based on given project ID.
#' 
#' @param project_id
#' 
#' @return a dataframe with the concepts saved for the given project. 
#' Returns NULL if 'concepts' table doesn't exist, or if this project doesn't have any concepts associated to it.
#' 
getProjectConcepts <- function(project_id){
  if (!dbExistsTable(appDB, "concepts")){
    return(NULL)
  }

  concepts <- dbGetQuery(appDB, paste("SELECT id, name, description, rule, project_id, datetime(created_at,'localtime') as created_at 
                                        FROM concepts WHERE project_id == ", project_id, sep = ""))
  return(concepts)
}



#' Adds a new concept to a given project and stores it in the database.
#' 
#' @param project_id
#' @param concept_name
#' @param concept_description
#' @param concept_rule
#' 
#' @return NULL
#' 
addConceptToProject <- function(project_id, concept_name, concept_description, concept_rule){
  
  concept <- list(
    name = concept_name,
    description = concept_description,
    rule = concept_rule,
    project_id
  )
  
  sql_query <- paste("INSERT INTO concepts (name, description, rule, project_id) 
                     VALUES (?, ?, ?, ?)",
                     sep = "")
  
  db_response <- dbExecute(appDB, sql_query, concept)
  return(NULL)
}