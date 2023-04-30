#' A module for inputing data into the application

library(shiny)
library(caTools)
library(rsample)
library(tidyverse)
library(DT)


#------------------------------------ UI SIDE --------------------------------------


#' This function creates the data loading menu on the ui-side, to load data into 
#' the working environment. It shows file-input components, an 'upload files' button, 
#' and a preview components.
#'
#' @param id File loading menu component ID, each ID must be unique across the entire app
#' @param label component label
#'
#' @return High level Shiny component for composing the page for the data acquisition screen.
#' 
loadWorkEnvMenu <- function(id, label = "DataLoadingMenu") {
  ns <- NS(id)
  fluidPage(
    uiOutput(ns("highlevelDataScreen"))
  )
}



#' Screen 1: first-time empty state screen that welcomes the user to load a new data set.
#' 
#' @param ns Namespace for the component
#' 
#' @return a list of UI components to show on this screen
#' 
firstUseScreen <- function(ns){
  return(
    list(
      column(12, style='padding:120px;',
             h3("Creating Models Using Labeled Data"),
             br(),
             p("Fusion will assist you in producing high-accuracy machine learning models through a guided and iterative process.
        There are several machine learning model types to choose from, but the first step is to provide 
        labeled data from which to train a model. This is an important step required for all the model types.
        Some model types may additionally require a lexicon or an additional enhancement coprus of text. If you're unsure, you can always come back here to re-upload them."),
             br(),
             actionButton(inputId = ns("get_data_started_btn"),
                          label = "Get started"
             )
      )
    )
  )
}



#' Screen 2: Load-data screen that allows the user to upload new data or corpus.
#' This screen provides some guidance to the user. It is also used when a user wants to upload
#' new external corpus or data later on.
#' 
#' @param ns Namespace for the component
#' 
#' @return a list of UI components to show on this screen
#' 
loadDataScreen <- function(ns, datafile_name){
  return(
    list(
      fluidRow(
        column(6,
               # the first file-input widget to load the data
               fileInput(inputId = ns("datafile"), 
                         label = "Labeled Data for Training and Validation",
                         multiple = FALSE,
                         accept = c(
                           "text/csv",
                           "text/comma-separated-values",
                           ".csv",
                           #"application/vnd.ms-excel",
                           ".xlsx"),
                         placeholder = ifelse(is.null(datafile_name), "No file selected", datafile_name)
               ),
               # the second input box for the leave out subset
               fileInput(inputId = ns("leaveoutfile"), 
                         label = "Leave-Out Validation",
                         multiple = FALSE,
                         accept = c(
                           "text/csv",
                           "text/comma-separated-values",
                           ".csv",
                           #"application/vnd.ms-excel",
                           ".xlsx"),
                         placeholder = ifelse(is.null(datafile_name), "No file selected", datafile_name)
               ),
               div(class="separator", "For advanced users"),
               # the third file-input widget for the expert corpus file
               fileInput(inputId = ns("corpusfile"), 
                         label = "External Enhancement Corpus",
                         multiple = FALSE,
                         accept = c(
                           "text/plain",
                           ".txt")
               )
        ),
        ## this is the right hand of the screen, that asks the user for details about the inputted data 
        ## and in some cases provides some warnings to the user
        column(6,
               uiOutput(ns("userGuidanceScreen"))
        )
      ),
      fluidRow(
        ## a button to move to the next screen
        column(12, align = 'right',
               uiOutput(ns("nextScreenButton"))
        )
      )
    )
  )
}



#' Screen 3: Main screen that summarizes to the user the data uploaded for this project,
#' and provides an overview of how this data is going to be used.
#' 
#' @param ns
#' 
#' @return a list of UI components to show on this screen
#' 
ongoingDataScreen <- function(ns){
  return(
    list(
      fluidRow(
        column(12,
               ## lists the data points stored in the system, and allows user to switch between them 
               ## (note: data should not be changed mid project, otherwise it is like starting from scratch)
               uiOutput(ns("listStoredData")),
               ## A row of buttons for uploading new data, for viewing the currently loaded data
               actionButton(inputId = ns("get_data_started_btn"),
                            label = "Upload new data",
                            icon = icon("upload")
               ),
               actionButton(inputId = ns("toggle_data_preview_btn"),
                            label = "Summary/Data overview",
                            icon = icon("th")
               )
        )
      ),
      ## a short summary of the data loaded into the system and how it may be used. This provides a brief user guidance
      fluidRow(
        column(12, style='padding-top:50px;',
               uiOutput(ns("chosenDataDescription"))
        )
      )
    )
  )
}





#------------------------------------ SERVER SIDE --------------------------------------


#' Adds server-side functionality for the data acquisition screen in order to load the
#' work environment from the chosen files or from database if possible. Upon loading from DB 
#' or interaction with the "upload" button it loads the files and provides a preview.
#'
#' @param input standard Shiny param
#' @param output standard Shiny param
#' @param session standard Shiny param
#'
#' @return list of reactive values that define the work environment: data and corpus
#' 
loadWorkEnv <- function(input, output, session){
  
  UI_elements_on_screen <- reactiveVal(NULL)        # holds the UI elements that should be shown on screen
  list_of_data_column_names <- reactiveVal(NULL)    # holds a list of column names from uploaded data file
  temporary_data_upload <- reactiveVal(NULL)        # holds newly uploaded data files, until it is preprocessed in the system
  temporary_leaveout_upload <- reactiveVal(NULL)
  
  
  # initialize the work_env object we will return at the end of this function
  if (!exists("work_env")){
    work_env <- reactiveValues(
      data = reactiveVal(NULL),
      lexicon = reactiveVal(NULL),
      corpus = reactiveVal(NULL),
      classification_labels = tibble(
        positive = "",
        negative = ""
      ),
      created_at = reactiveVal(NULL)
    )
  }
  
  
  
  # checks and updates the work environment in case the project changes
  observeEvent(current_project_id(), {
    if (doesProjectDataExist(current_project_id())){
      from_db_data <- getProjectData(current_project_id())
      from_db_lexicon <- getProjectLexicon(current_project_id())
      
      work_env$data(from_db_data$data)
      work_env$lexicon(from_db_lexicon$lexicon)
      work_env$corpus(from_db_data$corpus)
      work_env$classification_labels <- from_db_data$classification_labels
      work_env$created_at(from_db_data$created_at)
    } else {
      ## if changing to a new project that doesn't yet have data loaded, clear work_env
      work_env$data(NULL)
      work_env$lexicon(NULL)
      work_env$corpus(NULL)
      work_env$classification_labels = tibble(
        positive = "",
        negative = ""
      )
      work_env$created_at(NULL)
    }
    # work_env$data(addDanielsConcept(work_env$data()))
    # setProjectData(current_project_id(), work_env)
  })
  
  
  
  # Access the selected files if given
  in_data_file <- reactive({
    # If no file is selected, don't do anything
    shiny::validate(need(input$datafile, message = FALSE))
    input$datafile
  })
  
  in_corpus_file <- reactive({
    input$corpusfile
  })
  
  in_leaveout_file <- reactive({
    # If no file is selected, don't do anything
    shiny::validate(need(input$leaveoutfile, message = FALSE))
    input$leaveoutfile
  })
  
  # Observer for the datafile upload to respond when it is clicked
  observeEvent(input$datafile,{
    # checks to see if data was uploaded by user (only when no other data was previously uploaded 
    # to this project). If no for both, it will stop and show a notification
    proj_data <- getProjectData(current_project_id())
    if (is.null(in_data_file()) && is.null(proj_data)){
      sendSweetAlert(session = session,
                     title = "Labeled data is required",
                     text = "Please upload a labeled-data file.", 
                     type = "error"
      )
      return(NULL)
    }
    
    ## DATA FILE
    ## If there are files given, then read them into the work_env variable. Since we accept two different file formats,
    ## check first which file type it is (XLSX or CSV).
    if (!is.null(in_data_file())){
      newly_uploaded_data <- NULL
      if (in_data_file()$type=="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"){
        newly_uploaded_data <- read.xlsx(in_data_file()$datapath)
      }
      if (in_data_file()$type=="text/csv" | in_data_file()$type=="application/vnd.ms-excel"){
        newly_uploaded_data <- read.csv(in_data_file()$datapath, header = TRUE, stringsAsFactors = FALSE)
      }
      ## store the file contents in a reactive value for verification later
      temporary_data_upload(newly_uploaded_data)
    }
    
    ## pull out the column names from the data file, and make them available for other functions
    list_of_data_column_names(colnames(temporary_data_upload()))
  })
  
  # Observer for the corpus upload to respond when it is clicked
  observeEvent(input$corpusfile,{
    ## if an external enhancement corpus was given, load it into the work environment 
    ## (we do not process or check its content, as opposed to a data file or lexicon)
    newly_uploaded_enhancement_corpus <- NULL
    if (!is.null(in_corpus_file())){
      newly_uploaded_enhancement_corpus <- readLines(in_corpus_file()$datapath, warn = FALSE)
      work_env$corpus(newly_uploaded_enhancement_corpus)
    }
  })
  
  # Observer for the leaveout upload to respond when it is clicked
  observeEvent(input$leaveoutfile,{
    if (is.null(in_leaveout_file())){
      sendSweetAlert(session = session,
                     title = "Leaveout data is required",
                     text = "Please upload a leave-out-data file.", 
                     type = "error"
      )
      return(NULL)
    }
    
    ## Leave Out FILE
    ## If there are files given, then read them into the work_env variable. Since we accept two different file formats,
    ## check first which file type it is (XLSX or CSV).
    if (!is.null(in_leaveout_file())){
      newly_uploaded_leaveout <- NULL
      if (in_leaveout_file()$type=="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"){
        newly_uploaded_leaveout <- read.xlsx(in_leaveout_file()$datapath)
      }
      if (in_leaveout_file()$type=="text/csv" | in_leaveout_file()$type=="application/vnd.ms-excel"){
        newly_uploaded_leaveout <- read.csv(in_leaveout_file()$datapath, header = TRUE, stringsAsFactors = FALSE)
      }
      temporary_leaveout_upload(newly_uploaded_leaveout)
    }
    
    
  })
  
  # Changes the data preview widget to show a preview of the currently loaded data
  output$trainingpreview <- DT::renderDataTable({
    work_env$data() %>%
      filter(data_designation == "training") %>%
      select(-"data_designation", -"fusion.label", -"fusion.text")
  },
  server = TRUE,
  extensions="Responsive"
  )
  
  # Changes the data preview widget to show a preview of the currently loaded data
  output$validationpreview <- DT::renderDataTable({
    work_env$data() %>%
      filter(data_designation == "validation") %>%
      select(-"data_designation", -"fusion.label", -"fusion.text")
  },
  server = TRUE,
  extensions="Responsive"
  )
  
  # Changes the data preview widget to show a preview of the currently loaded data
  output$leaveoutpreview <- DT::renderDataTable({
    work_env$data() %>%
      filter(data_designation == "leave-out validation") %>%
      select(-"data_designation", -"fusion.label", -"fusion.text")
  },
  server = TRUE,
  extensions="Responsive"
  )
  
  
  
  # Returns the UI components for listing stored data from the database
  output$listStoredData <- renderUI({
    ns <- session$ns
    proj_data <- getProjectData(current_project_id())
    if (is.null(proj_data)){
      return(NULL)
    }

    return(
      awesomeRadio(12,
                   inputId = ns("data_select_radio"),
                   label = "Select data to use:", 
                   choices = c(as.character(proj_data$created_at)),
                   selected = as.character(proj_data$created_at)
      )
    )
  })
  
  
  
  # Creates the input boxes part of the screen in order to ask the user for more details about the data (e.g., which column has the labels, etc...)
  # Returns NULL if no data has been loaded.
  output$userGuidanceScreen <- renderUI({
    ns <- session$ns
    
    if (is.null(temporary_data_upload())){
      return(NULL)
    }
    
    return(
      list(
        selectInput(ns("classification_label_select"), label = "Which column holds the classification labels?", 
                    choices = c("unselected" = "", list_of_data_column_names()), 
                    selected = c("label", "Label", "labels", "Labels", "Labeling")),
        selectInput(inputId = ns("classification_pos_label"), 
                    label = "Which of the following is the positive label?",
                    choices = c("re-select label column" = "")),
        selectizeInput(
          ns('classification_text_select'), 
          'Select below the column(s) that hold the text for classification', 
          choices = c("unselected" = "", list_of_data_column_names()), 
          multiple = TRUE
        ),
        numericInput(ns("qualitatively_examined_num"), 
                     label = "Beginning from the first row, how many data observations have been qualitatively examined to generate a lexicon?", 
                     value = ifelse (is.null(work_env$data()), 
                                     300, 
                                     nrow(work_env$data() %>% filter(data_designation=="training")))
        )
      )
    )
  })
  
  
  
  #' This observer waits for the user to select the column that holds the labels for classification,
  #' and then it creates a list of all possible values for the label -- for the user to choose which
  #' one is the positive value, and the others will be assumed to be negative value (for binary classification).
  observeEvent({
    input$classification_label_select
  },{
    if (input$classification_label_select!=""){
      values = levels(as.factor(temporary_data_upload()[,input$classification_label_select]))
      updateSelectInput(session, "classification_pos_label",
                        label = "Which label do you want to identify?",
                        choices = c("unselected" = "", values)
      )
    }
  }, ignoreNULL = T)
  
  
  
  #' This observer makes the "next" button appear as long as there was some change in the data uploaded
  output$nextScreenButton <- renderUI({
    ns <- session$ns
    proj_data <- getProjectData(current_project_id())
    if (is.null(temporary_data_upload()) & is.null(proj_data)){
      return(NULL)
    }
    if (is.null(temporary_leaveout_upload())){
      return(NULL)
    }
    return(
      actionButton(inputId = ns("next_screen_btn"), 
                   label = "Next"
      )
    )
  })
  
  
  
  # observes a case when user changes projects, if so, nullify the list of column names
  observeEvent({
    current_project_id()
  },{
    list_of_data_column_names(NULL)
  }, ignoreInit = TRUE)
  
  
  
  # Observes to see if user has loaded any data previously, if yes, show main screen, otherwise
  # show welcome screen.
  observe({
    ns <- session$ns
    proj_data <- getProjectData(current_project_id())
    
    ## if no projects exist at all, user has to create at least one project
    if (current_project_id()==0){
      sendSweetAlert(session = session, 
                     title = "Welcome", 
                     text = 'Please begin by creating a new project in the "projects" tab.',
                     type = "info")
      return(NULL)
    }
    
    if (is.null(proj_data)){
      ## this is first time, no data loaded, it will also call for the screen to load data
      UI_elements_on_screen(firstUseScreen(ns))
    }
    else {
      ## this is the main screen, showing which data is loaded, and provide guidance
      UI_elements_on_screen(ongoingDataScreen(ns))
    }
  })
  
  
  
  # This is the high level UI screen renderer. It will switch to the specific screens upon need.
  output$highlevelDataScreen <- renderUI({
    return(UI_elements_on_screen())
  })
  
  
  
  output$chosenDataDescription <- renderUI({
    ns <- session$ns
    proj_data <- getProjectData(current_project_id())
    if (is.null(proj_data)){
      return(NULL)
    }
    
    # toggle to check if user has chosen to see the overview or the data preview 
    if((input$toggle_data_preview_btn%%2)==0){
      
      training_count <- nrow(proj_data$data %>% filter(data_designation == "training"))
      validation_count <- nrow(proj_data$data %>% filter(data_designation == "validation" | data_designation == "leave-out validation"))
      iterations_left_count <- nrow(proj_data$data %>% filter(data_designation == "unassigned"))%/%300
      
      return(list(
        p(paste0("The labeled data you have uploaded has ", prettyNum(nrow(proj_data$data),big.mark=",", preserve.width="none"), " rows. This data is going to be used 
                 to develop a Machine Learning (ML) model through an ongoing iterative process. Fusion will
                 use ", prettyNum(training_count,big.mark=",", preserve.width="none"), " rows of the labeled data for training and ", prettyNum(validation_count,big.mark=",", preserve.width="none"), " rows for validation purposes. The rest of the data
                 will be reserved for followup iterations---in each iteration Fusion will increase the training set, and assemble a new validation set.
                 Below is an illustraion of how the current data set is assigned. This data set is enough for ", iterations_left_count, " more iterations, but you can load additional data later.")
        ),
        br(),
        plotOutput(ns("dataInfo"), height = 120)
      )
      )
    } else {
      df <- proj_data$data %>% filter(data_designation == "training")
      temp_count <- df %>% count(fusion.label)
      df_size <- nrow(df)
      positive_count_in_training <- temp_count[2,2]/df_size
      
      df <- proj_data$data %>% filter(data_designation == "validation")
      temp_count <- df %>% count(fusion.label)
      df_size <- nrow(df)
      positive_count_in_validation = temp_count[2,2]/df_size
      
      df <- proj_data$data %>% filter(data_designation == "leave-out validation")
      temp_count <- df %>% count(fusion.label)
      df_size <- nrow(df)
      positive_count_in_leaveout = temp_count[2,2]/df_size

      return(
        fluidRow(
          tabBox(
            # The id lets us use 'input$file_preview_tabs' on the server to find the current tab
            id = ns("file_preview_tabs"), width = 12,
            tabPanel(paste0("Training Data (", round(positive_count_in_training,3)*100, "% ", work_env$classification_labels$positive, ")"),
                     DT::dataTableOutput(ns("trainingpreview"))
            ),
            tabPanel(paste0("Validation Data (", round(positive_count_in_validation,3)*100, "% ", work_env$classification_labels$positive, ")"),
                     DT::dataTableOutput(ns("validationpreview"))
            ),
            tabPanel(paste0("Leave-Out Data (", round(positive_count_in_leaveout,3)*100, "% ", work_env$classification_labels$positive, ")"),
                     DT::dataTableOutput(ns("leaveoutpreview"))
            )
          )
        )
      )
    }
  })
  
  
  
  # Observes for clicks on the "get started" button on the welcome screen or the "upload new data" from the overview screen (screen 3), 
  # and if so, load screen 2 (mentioned above)
  observeEvent(input$get_data_started_btn,{
    UI_elements_on_screen(
      ## switch to screen 2: the load-data screen that allows the user to upload new data or corpus.
      loadDataScreen(session$ns, input$datafile$name)
    )
  })
  
  
  
  # Observer for the "next" button
  observeEvent(input$next_screen_btn,{
    ## check if user forgot to input one of the fields, if so warn them
    if (
      !is.null(temporary_data_upload()) & (
        is.null(input$classification_label_select) |
        is.null(input$classification_pos_label) |
        ifelse(is.null(input$classification_pos_label), "",input$classification_pos_label) =="" |
        is.null(input$classification_text_select)
      )
    ){
      sendSweetAlert(session = session,
                     title = "Input is incomplete",
                     text = "Please fill in all the input boxes.", 
                     type = "error"
      )
      return(NULL)
    }
    
    ## make sure with user that the correct text columns were selected
    if(!is.null(input$classification_text_select)){
      shinyWidgets::confirmSweetAlert(session = session,
                                      inputId = "confirmTextColumns",
                                      text = paste("Based on your input, only the following data columns will be used for text classification:", paste(input$classification_text_select, collapse = ", ")),
                                      btn_labels = c("Wait, let me adjust it", "Confirm"))
    }
  })
  
  
  # Observer for the "next" button - after use confirms column selection
  observeEvent(input$confirmTextColumns,{
    if(input$confirmTextColumns){
      # if new data file has been uploaded, initialize it, and store in the work environment
      if (!is.null(temporary_data_upload()) & !is.null(temporary_leaveout_upload())){
        full_data_set <- temporary_data_upload()
        
        ## A heuristic to find the negative label value (e.g., NS). It will look for a value in the selected
        ## label column that appears most frequently but does not equal to the positive value (e.g., SU)
        non_pos_labels <- full_data_set %>%
          filter(.[,!!input$classification_label_select] != input$classification_pos_label) %>%
          count(eval(as.name(!!input$classification_label_select))) %>%
          arrange(desc(n))
        negative_label = non_pos_labels[[1,1]]

        prev_work_data <- work_env$data()

        work_env$data(
          initializeData(full_data_set = steralize_data(full_data_set, 
                                                        input$classification_label_select, 
                                                        positive_label = input$classification_pos_label, 
                                                        negative_label = negative_label), 
                         classification_label_column = input$classification_label_select,
                         classification_text_column_names = input$classification_text_select,
                         input$qualitatively_examined_num,
                         iteration_validation_size = 300)
        )
        if (!is.null(prev_work_data)){
          prev_work_data <- prev_work_data %>% 
            filter(data_designation!='leave-out validation') %>%
            select("id", "data_designation", "fusion_iteration")
          
          cur_data <- work_env$data() %>%
            select(-"data_designation", -"fusion_iteration") %>%
            left_join(prev_work_data, by = "id") %>%
            mutate(data_designation = ifelse(data_designation=='', 'unassigned', data_designation))
          work_env$data(cur_data)
        }
        
        leaveout <- temporary_leaveout_upload()
        work_env$data(
          initializeLeaveOutData(
            work_data = work_env$data(),
            leaveout_data_set = steralize_data(leaveout, 
                                               input$classification_label_select,
                                               positive_label = input$classification_pos_label, 
                                               negative_label = negative_label),
            classification_label_column = input$classification_label_select,
            classification_text_column_names = input$classification_text_select
          )
        )
        
        work_env$classification_labels$positive = input$classification_pos_label
        work_env$classification_labels$negative = negative_label
      }
      # if loaded data from file, store it in the database, and retrieve the id it was stored with
      if(!is.null(temporary_data_upload())){
        data_id <- setProjectData(current_project_id(), work_env)
        work_env$created_at(getDataByID(data_id)$created_at)
      }
      
      ## this is needed to help know if the user has inputted new data or not
      temporary_data_upload(NULL)

      ## This forces the screen elements to refresh again when new data was uploaded
      temp_cur_proj_id <- current_project_id()
      current_project_id(1)
      current_project_id(temp_cur_proj_id)
      
      ## switch to screen 3: the overview screen
      UI_elements_on_screen(ongoingDataScreen(session$ns))
    }
  })
  
  
  
  #' Returns data about the current data sets used for training, enhancement, and validation.
  #' 
  #' @return plot about the current state of the data sets, specifically data set sizes
  #' 
  output[["dataInfo"]] <- renderPlot({
    if(is.null(work_env$data())){
      return(NULL)
    }
    size_scale_for_waffle <- nrow(work_env$data())/50
    count_table <- round(table(work_env$data()$data_designation)/size_scale_for_waffle)
    count_table <- count_table[order(factor(names(count_table), levels = c('training', 'validation', 'leave-out validation', 'unassigned')))]
    counts <- count(work_env$data(), data_designation) %>%
      tibble::deframe()
    
    counts <- counts[order(factor(names(counts), levels = c('training', 'validation', 'leave-out validation', 'unassigned')))]
    
    pretty_counts <- prettyNum(unname(counts),big.mark=",", preserve.width="none")
    count_table_names <- paste0(names(counts), " (", pretty_counts, ")")
    names(count_table) <- count_table_names
    
    plot <- waffle(parts = count_table, 
                   rows = 1,
                   size = 0.1,
                   legend_pos = "bottom",
                   colors = rev(wes_palette(n=5, name="Darjeeling1"))
    )
    
    output_plot <- plot + theme(plot.background = element_rect(color = NA, fill = "#ecf0f5"))
    return(output_plot)
  },
  bg="#ecf0f5", 
  execOnResize=T
  )
  
  
  ## this is the return for the main server-side function of this module
  return(work_env)
}



#' This initializes the inputted data and splits it into 4 different data sets: training, 
#' validation, leave-out validation, and unassigned for later use. This function should be
#' called each time a new data set is being loaded into the work environment.
#' 
#' @param full_data_set this is the full data table that was inputted by the user
#' @param classification_label_column this is the name of the column that holds the labels
#' @param classification_text_column_names this is the column names (multiple) that hold the text to be classified. 
#' For example, this `title` and `content` names.
#' @param qual_examined_size the amount of results that the qualitative analyst has already
#' examined, thus contaminated them from being in validation sets. Begins from the first row.
#' @param iteration_validation_size the size of the validation set. Default is 500.
#' 
#' @return an updated work_env object that has a new column 'data_designation'
#' 
initializeData <- function(full_data_set, classification_label_column, classification_text_column_names, qual_examined_size, iteration_validation_size = 500){
  full_data_set <- full_data_set %>%
    mutate(fusion.label = full_data_set[,classification_label_column]) %>%
    mutate(!!classification_text_column_names[1] := paste("<b>", .[,!!classification_text_column_names[1]], "</b>", sep = " ")) %>%
    unite("fusion.text", all_of(classification_text_column_names), remove = FALSE, sep = " ")
  
  
  df.training <- full_data_set %>%
    slice(1:qual_examined_size) %>%
    mutate(data_designation = "training")
  
  non_training_data <- full_data_set %>%
    slice((1+qual_examined_size):(nrow(full_data_set)))
  
  ## check to see if the non-training data is too small, if so, try to divide what little data there is into halves
  iteration_validation_size <- ifelse(nrow(non_training_data)<(iteration_validation_size*2), 
                                      floor(nrow(non_training_data)/2) - 1, 
                                      iteration_validation_size)
  
  
  ### Method below will randomly split the validation and unassigned data sets
  non_training_data_split <- non_training_data %>%
    initial_split(prop = iteration_validation_size / nrow(non_training_data),
                  strata = all_of(classification_label_column))
  df.validation <- training(non_training_data_split) %>%
    mutate(data_designation = "validation") %>%
    mutate(fusion_iteration = 1)
  
  df.unassigned <- testing(non_training_data_split) %>%
    mutate(data_designation = "unassigned")
  
  
  ### Method below will not randomly split the validation and unassigned data sets
  # df.validation <- non_training_data %>%
  #   slice(1:iteration_validation_size) %>%
  #   mutate(data_designation = "validation")
  # 
  # df.unassigned <- non_training_data %>%
  #   slice((1+iteration_validation_size):(nrow(non_training_data))) %>%
  #   mutate(data_designation = "unassigned")
  
  work_data <- bind_rows(df.training, 
                         df.validation, 
                         #df.leave_out_validation, 
                         df.unassigned)
  return(work_data)
}


initializeLeaveOutData <- function(work_data, leaveout_data_set, classification_label_column, classification_text_column_names){
  leaveout_data_set <- leaveout_data_set %>%
    mutate(fusion.label = leaveout_data_set[,classification_label_column]) %>%
    mutate(!!classification_text_column_names[1] := paste("<b>", .[,!!classification_text_column_names[1]], "</b>", sep = " ")) %>%
    unite("fusion.text", all_of(classification_text_column_names), remove = FALSE, sep = " ") %>%
    mutate(data_designation = "leave-out validation")
  
  if (is.null(work_data)){
    return(leaveout_data_set)
  }
  work_data <- bind_rows(work_data, leaveout_data_set)
  return(work_data)
}



#' Prepares the data sets for another iteration of the model generation process by joining sets and allocating new ones. 
#' The old validation set joins the training set, and a new validation set is created from the unassigned set
#' at the same size as the previous one.
#' 
#' @param work_data
#' 
#' @return an updated version of work_env ready for another iteration
#' 
advanceValidationSet <- function(work_data, iteration_number){
  prev_iteration_validation_size = nrow(work_data %>% filter(data_designation=="validation"))
  unassigned_size = nrow(work_data %>% filter(data_designation=="unassigned"))
  iteration_validation_size = prev_iteration_validation_size
  if (unassigned_size<prev_iteration_validation_size*2){
    iteration_validation_size = unassigned_size-1
  }
  
  df.training_and_leave_out <- work_data %>%
    mutate(data_designation = ifelse(data_designation=="validation", 
                                     "training", 
                                     data_designation
    )) %>%
    filter(data_designation != "unassigned")
  
  prev_unassigned <- work_data %>% 
    filter(data_designation == "unassigned")
  
  ### Method below will randomly split the validation and unassigned data sets
  split <- sample.split(prev_unassigned$fusion.label, SplitRatio = iteration_validation_size)
  
  # df.validation <- subset(prev_unassigned, split == TRUE) %>%
  #   mutate(data_designation = "validation") %>%
  #   mutate(fusion_iteration = iteration_number)
  # 
  # df.unassigned <- subset(prev_unassigned, split == FALSE) %>%
  #   mutate(data_designation = "unassigned")
  
  df.validation <- prev_unassigned %>%
    slice((1):(iteration_validation_size)) %>%
    mutate(data_designation = "validation") %>%
    mutate(fusion_iteration = iteration_number)
  
  df.unassigned <- prev_unassigned %>%
    slice((1+iteration_validation_size):(nrow(prev_unassigned))) %>%
    mutate(data_designation = "unassigned")
  
  ### Method below will not randomly split the validation and unassigned data sets
  # df.validation <- prev_unassigned %>%
  #   slice(1:iteration_validation_size) %>%
  #   mutate(data_designation = "validation")
  # 
  # df.unassigned <- prev_unassigned %>%
  #   slice((1+iteration_validation_size):(nrow(prev_unassigned))) %>%
  #   mutate(data_designation = "unassigned")
  
  iterated_work_data <- bind_rows(df.training_and_leave_out, df.validation, df.unassigned)
  
  return(iterated_work_data)
}


#' A function that cleans-up the data set and makes sure it has a Label that is binary 
#' (with only 2 label types). This is currently hardcoded and should be fixed to accommodate any
#' given data set.
#' 
#' @param full_data_set
#' @param classification_label_column_name
#' @param positive_label
#' @param negative_label
#' 
#' @return steralized data set
#' 
steralize_data <- function(full_data_set, classification_label_column_name, positive_label, negative_label){
  classification_label <- enquo(classification_label_column_name)

  ## steralize data set by removing N\A values, and making sure it is a binary label
  output <- full_data_set %>%
    filter(.[,!!classification_label] != "N/A") %>%
    filter(.[,!!classification_label] != "") %>%
    mutate(!!classification_label_column_name := as.factor(ifelse(.[,!!classification_label] == positive_label, 
                                                                  positive_label, 
                                                                  negative_label)
    )) %>%
    mutate(!!classification_label_column_name := droplevels(.[,!!classification_label]))
  return(output)
}





addDanielsConcept <- function(work_data){
  containsPhoneNumber <- function(text){
    ## captures the following phone number examples: +919367788755, 8989829304, +16308520397, 786-307-3615
    phone_regex = '([\\+]?\\d{1,3}[-\\s\\.]?[(]?\\d{3}[)]?)?[-\\s\\.]?\\d{3}[-\\s\\.]?\\d{4,6}'
    #phone_regex_more_strict = '[\\+]?[0-9]?[-\\s\\.]?([(]?[0-9]{3}[)])?[-\\s\\.]?[0-9]{3}[-\\s\\.]?[0-9]{4,6}'
    
    output = grepl(pattern = phone_regex, x = text)
    #output = lapply(text, function(message) return(grepl(pattern = phone_regex, x = message)))
    return(output)
  }
  
  containsURL <- function(text){
    ## captures URLs that begin with http or https
    url_regex = 'https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]+\\.[a-zA-Z0-9\\(\\)]{1,6}\\b([-a-zA-Z0-9\\(\\)@:%_\\+.~#?&//=]*)'
    
    output = grepl(pattern = url_regex, x = text)
    #output = lapply(text, function(message) return(grepl(pattern = url_regex, x = message)))
    return(output)
  }
  
  
  add_operational_security_vector <- function(data){
    
    data %>%
      mutate(concept =
               ## inclusion of email and instant messaging
               (str_detect(tolower(pds.email_address), "protonmail.com|mail.ru|bk.ru|jabber.ru|xmpp.jp") |
                  (str_detect(tolower(content), "icq|telegram|wickr") | !is.na(pds.telegram_address))
               ) &
               ## exclusion
               (!str_detect(tolower(pds.email_address), "gmail.com|hotmail.com|hotmail.co.uk|yahoo.com|brauchen.info") &
                  !str_detect(tolower(content), "whatsapp|skype")
               ) &
               ## phone number
               !containsPhoneNumber(content) &
               ## URLs
               !containsURL(content)
      ) %>%
      mutate(concept = replace_na(concept, FALSE)) %>%
      #filter(concept==TRUE) %>%
      mutate(concept = ifelse(concept, 1, 0))
  }
  
  return(add_operational_security_vector(work_data))
}