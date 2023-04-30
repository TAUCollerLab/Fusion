#' The main server file of the system. It will load the different server modules as needed.
options(shiny.maxRequestSize=30*1024^2)

library(shiny)



#' Server Main Logic
#' 
shinyServer(function(input, output, session) {
    
    #This makes the sidebar menu begin with projects
    #updateTabItems(session, "sidebar_tabs", "project")
    
    ## Server-side logic for loading data into the work environment
    work_env <- callModule(loadWorkEnv, "loadData")
    ## initialize the lower boundary on recall value and the concept list
    work_env$hard_recall_value <- reactiveVal(0.5)  ## in the past hard_recall_value by default was 0.4, but in some data there were issues with this value, and 0.8 worked well.
    work_env$current_concept_list <- reactiveVal(NULL)
    
    ## Server-side logic for creating a list of models in the system
    callModule(module = listAllModels, id = "listModels", work_env)
    
    ## Server-side logic for creating a list of models in the system
    values_passed_to_other_modules <- callModule(module = simpleModel, id = "createSimpleModels", work_env)
    
    ## Server-side logic for lexicon building tab
    callModule(module = buildLexicon, id = "lexiconTab", work_env, values_passed_to_other_modules)
    
    ## Server-side logic for listing existing projects of the user
    callModule(listProjectsPage, "loadProjects")
    
    ## Server-side logic for capturing and documenting concepts
    # callModule(manageConcepts, "manageConcepts", work_env)
    
    ## Server-side logic for model-assisted content analysis of text for building a better lexicon
    callModule(reflectionEngine, "reflectionUI", work_env, values_passed_to_other_modules)
    
    ## Server-side logic for model-assisted content analysis of text for building a better lexicon
    callModule(insightsEngine, "insightsUI", work_env, values_passed_to_other_modules)
    
    
    ## Adds user info in the sidebar (should be moved to the users module later)
    output$userInfo <- renderUI({
        user_info <- getUserInfo(current_user_id())
        return(list(
            br(),
            fluidRow(align="center",
                     div(id="avatar"),
                     user_info$full_name,
                     br(),
                     ifelse(user_info$role==1, "Domain Expert", "Data Scientist")
            )
        ))
    })
    
    
    output$projectName <- renderMenu({
        proj_df <- getProjects(current_user_id())
        proj_name <- proj_df$name[which(proj_df$id == current_project_id())]
        return(
            menuItem(paste("Project:", proj_name, sep = " "), tabName = "project", icon = icon("tasks")
            )
        )
    })
    
    
    ## Server-side logic for useful tools fusion provides
    callModule(module = usefulTools, id = "usefulTools")
    
    
    ## About-screen server module
    callModule(module = aboutScreen, id = "aboutScreen")
    
    
    #' This function runs when this session ends. It should be used to close 
    #' various connections and open files. Note: with ODBC there is no need to use this function.
    #' 
    #onStop(function(){})
    # session$onSessionEnded(function() {
    #     if(dbIsValid(appDB)){
    #         RSQLite::dbDisconnect(appDB)
    #     }
    # })
})