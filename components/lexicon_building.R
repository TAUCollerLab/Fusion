library(compareDF)
library(htmlTable)


#------------------------------------ UI SIDE --------------------------------------


#' This function generates the UI components for the explanation screen.
#'
#' @param id
#' @param label
#' 
#' @return list of UI components
buildLexiconWidget <- function(id, label = "manageLexicon") {
  ns <- NS(id)
  
  
  return(
    fluidPage(
      column(12,
             fluidRow(
               column(3,
                      uiOutput(ns("lexicon_selector"))
               ),
               column(5,
                      fileInput(inputId = ns("lexiconfile"), 
                                label = "Select lexicon to import",
                                multiple = FALSE,
                                accept = c(".xlsx", ".xls", ".csv")
                      )
               ),
               column(4,
                      downloadButton(outputId = ns("exportLexicon"), 
                                     label = "Export", 
                                     style = "margin: 1px; margin-top: 24px;"
                      )
               ))
      ),
      column(12,
             uiOutput(ns("quality_metrics"))
      ),
      column(12,
             fluidRow(
               uiOutput(ns("lexiconManagementWindow"))
             )
      )
    )
  )
}



#------------------------------------ SERVER SIDE --------------------------------------


#' Adds server-side functionality for the explanation screen.
#' 
#' @param input
#' @param output
#' @param session
#' @param work_env
#' 
#' @return NULL
buildLexicon <- function(input, output, session, work_env, reactive_values_from_other_modules){
  
  currently_selected_lexicon <- reactiveVal(NULL)
  temporary_lexicon_upload <- reactiveVal(NULL)     # holds newly uploaded lexicon, for checking it before saving to the work environment
  all_lexicons_in_project <- reactiveVal(NULL)
  all_model_run_infos_in_project <- reactiveVal(NULL)
  
  
  
  in_lexicon_file <- reactive({
    input$lexiconfile
  })
  
  #' This function renders the selector for choosing which lexicon is being examined at the moment. 
  #' This will allow other components of the page to switch the working environment to use the selected lexicon.
  output$lexicon_selector <- renderUI({
    ns <- session$ns
    
    choices <- c("")
    current_lexicon_id <- c("")
    
    if(!nrow(all_lexicons_in_project())==0){
      
      choices <- all_lexicons_in_project()$id
      
      current_lexicon_id <- getProjectLexiconID(current_project_id())
    }
    
    return(
      list(
        selectInput(inputId = ns("lexicon_select_for_inspection"),
                    label = "Lexicon ver.", 
                    choices = choices,
                    width = "350px",
                    selected = current_lexicon_id
        ),
        actionButton(inputId = ns("switch_btn"), 
                     label = "Use this version",
                     style = "margin-bottom: 23px;"
        )
      )
    )
  })
  
  
  
  observeEvent(input$lexicon_select_for_inspection,{
    selected_lexicon_id <- input$lexicon_select_for_inspection
    ## sometimes the UI component for the selected lexicon hasn't been created yet at this point, and it will be NULL.
    ## in such cases return NULL, and wait for it to be created (in which case it will not be NULL anymore). 
    if (is.null(selected_lexicon_id) | selected_lexicon_id==""){
      currently_selected_lexicon(NULL)
      return(NULL)
    }
    
    currently_selected_lexicon(getLexiconByID(selected_lexicon_id)$lexicon)
  })
  
  
  
  #' This is the tabbed window that allows to view/edit the currently selected lexicon, to see history of changes, 
  #' and to compare various lexicon versions.
  output$lexiconManagementWindow <- renderUI({
    ns <- session$ns
    
    return(
      list(
        tabBox(
          # The id lets us use 'input$lexicon_management_tabs' on the server to find the current tab
          id = ns("lexicon_management_tabs"), width = 12,
          tabPanel(title = "View/Edit",
                   DT::dataTableOutput(ns("lexiconViewEdit"))
          ),
          tabPanel(title = "History", 
                   uiOutput(ns("lexicon_history"))
          ),
          tabPanel(title = "Compare Lexicons", 
                   tableOutput(ns("lexicon_comparison")),
                   uiOutput(ns("table_footnote"))
          )
        )
      )
    )
  })
  
  
  
  #' Provides a widget to view and edit the currently loaded lexicon
  output$lexiconViewEdit <- DT::renderDT({
    return(currently_selected_lexicon())
  },
  server = TRUE,
  selection = 'none',
  editable = 'all'
  )
  
  
  
  #' This observer allows to edit all cells of the lexicon
  #' The main problem at the moment is that it only allows to edit existing cells (but not to add or remove them).
  observeEvent(input$lexiconViewEdit_cell_edit, {
    lexicon_being_edited <- work_env$lexicon()
    lexicon_being_edited <- editData(lexicon_being_edited, input$lexiconViewEdit_cell_edit, 'lexiconViewEdit')
    addLexiconToProject(current_project_id(), lexicon_being_edited)
    work_env$lexicon(lexicon_being_edited)
  })
  
  
  
  #' Provides the history of changes over the different lexicon versions. 
  #' Each time there was a new lexicon added, it shows the time/date and the diff compared to the previous version.
  output$lexicon_history <- renderUI({
    ns <- session$ns
    
    ## this is a helper function that does a better 'seq' function
    sequenceWrapper <- function(lb, ub, by=-1) {
      s <- c()
      if(!ub > lb) s <- seq(lb,ub, by=by)
      return(s)
    }
    
    lexicons <- all_lexicons_in_project()
    if(nrow(lexicons)==0){
      return(HTML("No lexicons exist in the system"))
    }
    
    ## initialize the output list, and our counting variable j
    output_list <- list()
    j = 1
    
    ## Since we print the history in reverse order, from the most recent version of the lexicon to the earlier versions
    ## begin from the last items
    for (i in sequenceWrapper(nrow(lexicons)-1,1)){
      lex_i <- unserializeObject(lexicons$serialized_lexicon[i])
      lex_i_plus_1 <- unserializeObject(lexicons$serialized_lexicon[i+1])
      output_list[[(j)]] <- HTML(paste("Lexicon", lexicons$id[i+1], "was added at", lexicons$created_at[i+1], sep = " "))
      j <- j + 1
      diff_df <- compare_df(df_new = lex_i_plus_1, df_old = lex_i, group_col = c("terms"), stop_on_error = F)
      out <- create_output_table(comparison_output = diff_df, output_type = "html")
      output_list[[(j)]] <- out
      j <- j + 1
      output_list[[(j)]] <- br()
      j <- j + 1
    }
    
    ## Now, do the first lexicon outside the loop since we have no other lexicon version to compare it with
    lex_1 <- lexicons[1]
    output_list[[(j)]] <- HTML(paste("Lexicon", lexicons$id[1], "was added at", lexicons$created_at[1], sep = " "), "<br>")
    j <- j + 1
    
    return(output_list)
  })
  
  
  
  #' This creates value boxes that show the core metrics of a single lexicon version. The metrics shown are the average
  #' metrics of this lexicon version across all model runs where it has been used.
  output$quality_metrics <- renderUI({
    selected_lexicon_id <- input$lexicon_select_for_inspection
    ## sometimes the UI component for the selected lexicon hasn't been created yet at this point, and it will be NULL.
    ## in such cases return NULL, and wait for it to be created (in which case it will not be NULL anymore). 
    if (is.null(selected_lexicon_id)){
      return(NULL)
    }
    
    lexicon_metrics <- all_model_run_infos_in_project() %>%
      filter(lexicon_id == selected_lexicon_id)
    
    ## in case the lexicon selected hasn't been used in any model runs, return NULL (thus creating no value boxes)
    if (nrow(lexicon_metrics)==0){
      return(NULL)
    }
    
    ## calculate the average metrics values for the selected lexicon
    lexicon_metrics <- lexicon_metrics %>%
      rowwise() %>%
      mutate(precision = unserializeObject(serialized_confusion_matrix)$byClass['Precision'][[1]]) %>%
      mutate(recall = unserializeObject(serialized_confusion_matrix)$byClass['Recall'][[1]]) %>%
      ungroup() %>%
      summarise(avg.auc = mean(auc), avg.precision = mean(precision), avg.recall = mean(recall))
    
    return(
      fluidRow(
        valueBox(round(lexicon_metrics$avg.auc, digits = 3), "Avg. AUC", color = "light-blue"),
        valueBox(round(lexicon_metrics$avg.precision, digits = 3), "Avg. Precision", color = "light-blue"),
        valueBox(round(lexicon_metrics$avg.recall, digits = 3), "Avg. Recall", color = "light-blue")
      )
    )
  })
  
  
  
  #' Observer to switch the lexicon versions
  observeEvent(input$switch_btn,{
    selected_lexicon_id <- input$lexicon_select_for_inspection
    ## sometimes the UI component for the selected lexicon hasn't been created yet at this point, and it will be NULL.
    ## in such cases return NULL, and wait for it to be created (in which case it will not be NULL anymore). 
    if (is.null(selected_lexicon_id) | selected_lexicon_id==""){
      return(NULL)
    }
    switchProjectLexicon(current_project_id(), selected_lexicon_id)
    work_env$lexicon(getLexiconByID(selected_lexicon_id)$lexicon)
  })
  
  
  
  #' Functionality for the download button
  output$exportLexicon <- downloadHandler(
    filename = function() {
      paste("lexicon_", input$lexicon_select_for_inspection, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(currently_selected_lexicon(), file, row.names = FALSE)
    }
  )
  
  
  
  # Observer for the upload button to respond when it is clicked
  observeEvent(input$lexiconfile,{
    ## LEXICON
    ## Same as data file above, since we accept two different file formats,
    ## check first which file type it is (XLSX or CSV).
    newly_uploaded_lexicon <- NULL
    if (!is.null(in_lexicon_file())){
      if (in_lexicon_file()$type=="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"){
        newly_uploaded_lexicon <- read.xlsx(in_lexicon_file()$datapath)
        ## convert the lexicon file from a non structured format to a tidy flat format
        newly_uploaded_lexicon <- tidy_lexicon(newly_uploaded_lexicon)
      }
      if (in_lexicon_file()$type=="text/csv"){
        newly_uploaded_lexicon <- read.csv(in_lexicon_file()$datapath, header = TRUE, stringsAsFactors = FALSE)
      }
      ## store the file contents in a reactive value for verification later
      temporary_lexicon_upload(newly_uploaded_lexicon)
      
      # check for lexicon warnings
      lexicon_log <- checkLexicon(temporary_lexicon_upload())
      lexicon_warnings <- lexicon_log$warnings
      lexicon_errors <- lexicon_log$errors
      if(!is.null(lexicon_warnings) | !is.null(lexicon_errors)){
        sendSweetAlert(session = session, 
                       title = ifelse(is.null(lexicon_errors), "Warning", "Lexicon upload failed"), 
                       text = ifelse(is.null(lexicon_errors), lexicon_warnings, lexicon_errors),
                       type = ifelse(is.null(lexicon_errors), "warning", "error")
        )
      }
      ## if no errors found in the newly uploaded lexicon file, put it into the work environment
      if(is.null(lexicon_errors)){
        work_env$lexicon(temporary_lexicon_upload())
        addLexiconToProject(current_project_id(), work_env$lexicon())
      }
    }
  })
  
  
  
  #' Observer for identifying when user adds new lexicons to the system
  observeEvent({
    input$lexiconfile                       ## user has imported a new lexicon
    input$lexiconViewEdit_cell_edit         ## user has edited one of the lexicons, thus is has been added as a new version
    current_project_id()                    ## user switched projects
    1
  },
  {
    all_lexicons_in_project(getAllLexiconsInProject(current_project_id()))
    currently_selected_lexicon(NULL)
  })
  
  
  
  #' Observer for identifying when user has created a new model or has run another iteration of a model
  observeEvent({
    reactive_values_from_other_modules$num_of_models_created_during_this_session()    ## when a new model is created by user
    reactive_values_from_other_modules$num_of_iterations_run_during_this_session()    ## when a new iteration is run by user
    current_project_id()                                                              ## when user changes project
    1
  },{
    all_model_runs <- getAllModelsRunInfos(current_project_id())
    all_model_run_infos_in_project(all_model_runs)
  })
  
  
  
  #' Function that compares the different versions of the lexicon (metric-wise)
  output$lexicon_comparison <- renderTable({
    if (nrow(all_model_run_infos_in_project())==0){
      return(NULL)
    }
    
    avg.table <- all_model_run_infos_in_project() %>%
      rowwise() %>%
      mutate(precision = unserializeObject(serialized_confusion_matrix)$byClass['Precision'][[1]]) %>%
      mutate(recall = unserializeObject(serialized_confusion_matrix)$byClass['Recall'][[1]]) %>%
      mutate(`Lexicon ver.` = lexicon_id) %>%
      ungroup() %>%
      group_by(`Lexicon ver.`) %>%
      summarise(`Average AUC` = as.character(round(mean(auc),3)), `Average Precision` = as.character(round(mean(precision),3)), `Average Recall` = as.character(round(mean(recall),3)))
    
    return(avg.table)
  })
  
  
  
  #' This adds a footnote with a comment about values under the table of lexicon metrics
  output$table_footnote <- renderUI({
    if (nrow(all_model_run_infos_in_project())==0){
      return(NULL)
    }
    return(
      HTML("* higher values are better")
    )
  })
}



#------------------------------------ HELPER FUNCTIONS --------------------------------------



#' A function that does some verification checks on the lexicon and returns warnings or errors in case there are issues.
#' 
#' @param lexicon a lexicon table
#' 
#' @return a list with two string, the first one is 'fusion_warnings' and the second one is 'fusion_errors'. These can be
#' displayed to the user as error messages. Returns NULL if no lexicon file was given. 
#' 
checkLexicon <- function(lexicon){
  fusion_warnings = NULL
  fusion_errors = NULL
  
  # if no lexicon file was given, return NULL
  if (is.null(lexicon)){
    return(NULL)
  }
  
  # check if lexicon has a column named "terms"
  if (length(which(names(lexicon) == "terms"))==0){
    fusion_errors <- paste0(fusion_errors,
                            'The lexicon file must include a column named "terms", holding a term of up to 3 words per each row. 
                              A second lexicon column needs to indicate the label for each of these terms.\n'
    )
    return(
      list(
        errors = fusion_errors,
        warnings = fusion_warnings
      )
    )
  }
  
  # check for duplicated terms
  dups <- duplicated(lexicon, fromLast = F)
  if (length(which(dups))!=0){
    fusion_warnings <- paste0(fusion_warnings,
                              paste("Duplicated lexicon terms in row(s)", paste(which(dups), collapse = ', ')),
                              ".\n"
    )
  }
  
  # check for terms that are in both group labels
  dups_term_only <- duplicated(lexicon %>% select(terms), fromLast = F)
  dups_label_n_term <- duplicated(lexicon %>% select(Label, terms), fromLast = F)
  # subtract dups of label-term pairs from dupes among terms only, this way we get dupes between label groups
  duplicated_terms <- which(dups_term_only - dups_label_n_term!=0)
  if (length(duplicated_terms)!=0){
    fusion_warnings <- paste0(fusion_warnings,
                              paste("Some lexicon terms appear in both label groups, see row(s)", paste(duplicated_terms, collapse = ', ')),
                              ".\n"
    )
  }
  
  return(
    list(
      errors = fusion_errors,
      warnings = fusion_warnings
    )
  )
}



#' A helper function that converts a lexicon from Ghal's unstructured format, to a tidy flat format.
#' 
#' @param lexicon
#' 
#' @return a dataframe that holds the newly formatted lexicon, or NULL if given lexicon was NULL
#' 
tidy_lexicon <- function(lexicon){
  if (is.null(lexicon)){
    return(NULL)
  }
  
  extract_domains_from_urls <- function(x){
    domains <- sapply(x, function(y) ifelse(y=="", 
                                            "", 
                                            strsplit(gsub("http://|https://|www\\.", "", y), "/")[[c(1, 1)]]
                                            )
    )
    return(domains)
  }
  
  unwrap_patterns <- function(in_text){
    pattern <- str_trim(strsplit(in_text, "x=")[[1]][1])
    sub_words <- strsplit(strsplit(in_text, "x=")[[1]][2], ",")[[1]]
    out <- paste(sapply(sub_words, function(y) gsub("x", str_trim(y), pattern)), collapse = ",")
    return(out)
  }
  
  unwrap_skipgrams <- function(in_text){
    left_side_of_skipgram <- str_trim(strsplit(strsplit(in_text, "AND")[[1]][1], ";")[[1]])
    right_side_of_skipgram <- str_trim(strsplit(strsplit(in_text, "AND")[[1]][2], ";")[[1]])
    if (is.na(strsplit(in_text, "AND")[[1]][2])){
      return(in_text)
    }
    output_string = ""
    for (word in left_side_of_skipgram){
      output_string <- paste(output_string, paste(word, right_side_of_skipgram, sep = " AND ", collapse = ","), sep = ",")
    }
    return(output_string)
  }
  
  ## convert NAs in lexicon to empty string
  lexicon <- lexicon %>%
    mutate_all(list(~replace_na(as.character(.),"")))
  
  ## helper function to wrap a sequence of numbers (so it doesn't go over last number and back, which it does by default)
  sequenceWrapper <- function(lb, ub, by=1) {
    s <- c()
    if(!ub < lb) s <- seq(lb,ub, by=by)
    return(s)
  }
  
  ## merge custom columns (merge only columns that don't require any special treatment)
  custom_column_names <- tibble(col_names = colnames(lexicon)[sequenceWrapper(7,length(lexicon))]) %>%
    filter(!col_names %in% "Website.links")
  if (nrow(custom_column_names)>0){
    if (nrow(custom_column_names)==1){
      ## single custom field
      lexicon$custom_fields <- lexicon[, custom_column_names[[1]]]
    } else{
      ## multiple custom fields that need to be pasted together
      lexicon$custom_fields <- apply(lexicon[, custom_column_names[[1]]], 1, paste, collapse = ";")
    }
  } else {
    ## no custom fields
    lexicon$custom_fields <- ""
  }
  
  
  ## if "Website.links" is missing, put it as an empty column (otherwise the code below crashes as it expects to see this column)
  if(!"Website.links" %in% colnames(lexicon)){
    lexicon[,"Website.links"] <- ""
  }

  
  output <- lexicon %>%
    ## Create a column for labels
    mutate(Label = Labeling) %>%
    ## flatten all the columns that have lists of items
    separate_rows(Words, sep = ";|,") %>%
    separate_rows(Phrases, sep = ";") %>%
    separate_rows(custom_fields, sep = ";") %>%
    separate_rows(Website.links, sep = ";") %>%
    separate_rows(Patterns, sep = "\\];\\s*\\[") %>% mutate(Patterns = gsub("\\[|\\]|\\];", "", Patterns)) %>%
    mutate(Patterns = sapply(Patterns, function(x) unwrap_patterns(x))) %>%
    separate_rows(Patterns, sep = ",") %>%
    mutate(Patterns = sapply(Patterns, function(x) unwrap_skipgrams(x))) %>%
    separate_rows(Patterns, sep = ",") %>%
    ## extract domain names from website links
    mutate(Website.links = str_trim(gsub("<|>", "", Website.links))) %>%
    mutate(Website.links = extract_domains_from_urls(Website.links)) %>%
    ## combine all the columns and trim white spaces
    mutate(terms = paste(str_trim(Words), str_trim(Phrases), str_trim(custom_fields), str_trim(Website.links), str_trim(Patterns), sep = ";")) %>%
    separate_rows(terms, sep = ";") %>%
    ## properly mark NA terms, select only the main columns we need, and filter out any duplicates or empty terms (that may have been added during the flattening process)
    mutate(terms = ifelse(terms=="NA", NA, terms)) %>%
    select(Label, terms, Category, `Sub-Category`) %>%
    distinct() %>%
    filter(terms!="")
  
  return(output)
}