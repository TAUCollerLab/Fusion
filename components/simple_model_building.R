#' A module for generating a statistical model, and listing existing models.

library(shiny)
library(waffle)
library(anytime)
library(wesanderson)


#------------------------------------ UI SIDE --------------------------------------


#' List of existing models in the current project. Also allows to create new models.
#' Each model in the list is going to be presented as a Shiny Dashboard Box.
#'
#' @param id Widget ID, each ID must be unique across the entire app
#' @param label component label
#'
#' @return None
#' 
simpleModelWidget <- function(id, label = "createSimpleModels") {
  ns <- NS(id)
  fluidPage(
    uiOutput(ns("modelCreate"))
  )
}



createSimpleModelBar <- function(ns, work_env){
  fluidPage(
    htmlOutput(ns("model_descriptor")),
    plotOutput(ns("dataInfo"), height = 80),
    br(),
    fluidRow(column(12,
                    div(style="display:inline-block", actionButton(ns("create_simplified_model_btn"), label = "Add ML model"))
    )),
    br()
  )
}

#' A function that provides the needed UI box for listing model information within 
#' a box component. This should be used to list all iterations of a single model. 
#' For other models use this function multiple times.
#' 
#' @param ns namespace associated to the higher level component this Box is going to be in.
#' @param model_id id of the model this box is showing information for
#' @param model_run_info dataframe that holds the model information to be displayed (with
#' the info for all the iteration of this specific given model_id).
#' 
#' @return None
#'
simplifiedModelInfoBox <- function(ns, iteration_id, model_run_info){
  if(is.null(model_run_info)){
    return()
  }
  
  model_name <- function(model_type){
    if (model_type=='Unsupervised - Lexicon'){ 
      return('Reflection')
    }
    else {
      return('New Insights')
    }
  }
  
  iterator <- unique(model_run_info$iteration_number)
  model_output_list <- lapply(iterator, function(i) {
    list(
      # HTML(paste0("<p>", model_name(model_run_info$model_type[i])," model is ready!</p>")),
      # HTML(paste0("Iteration ", i, ifelse(is.na(model_run_info$lexicon_id[i]),":",paste0(" (lex. ", model_run_info$lexicon_id[i], "):")))),
      # uiOutput(ns(paste("modelStats", model_id, "i", model_run_info$iteration_number[i], sep = ""))),
      htmlOutput(ns(paste("F1Score", i, "i", sep = "")), placeholder = TRUE)
    )
  })
  size <- length(iterator)
  model_output_list[[(size+1)]] <- actionButton(ns(paste("iteration", iteration_id, "-iterate_btn", sep = "")), label = "Iterate")
  model_output_list[[(size+2)]] <- actionButton(ns(paste("iteration", iteration_id, "-delete_btn", sep = "")), label = "Remove")

  b <- box(title = strong(paste("Iteration ", iteration_id, sep = "")), 
           width = 12, 
           status = "primary", 
           model_output_list
  )
  return (b)
}


#------------------------------------ SERVER SIDE --------------------------------------


#' Adds server-side functionality for model list widget.
#' It defines the functionality of all the needed buttons and server-side 
#' logic for the various widgets being used.
#'
#' @param input standard Shiny param
#' @param output standard Shiny param
#' @param session standard Shiny param
#' @param work_env the work environment variable which actually is a list of other variables:
#' data, lexicon, model, etc. 
#'
#' @return None
#' 
simpleModel <- function(input, output, session, work_env){
  
  storedDB <- reactiveVal(NULL)
  iteration_done_flag <- reactiveVal(FALSE)   ## this flag is a hack to force the UI to refresh after an iteration is done
  num_of_models_created_during_this_session <- reactiveVal(0)
  num_of_iterations_run_during_this_session <- reactiveVal(0)
  
  
  
  #' Function that generates the list of models (in the form of boxes) to be presented.
  #' It should list all existing models in the system/project. Also, should allow to create new models.
  output$modelCreate <- renderUI({
    # takes the namespace from the current session
    ns <- session$ns
    
    ui_component_list <- list()
    i = 1
    
    ui_component_list[[i]] <- createSimpleModelBar(ns, work_env)
    i <- i + 1
    
    for (iteration_id in unique(storedDB()$iteration_number)){
      ui_component_list[[i]] <- simplifiedModelInfoBox(ns, iteration_id, storedDB() %>% filter(iteration_number==iteration_id))
      i <- i + 1
    }
    
    return(fluidRow(ui_component_list))
  })
  
  
  
  output$model_descriptor <- renderUI({
    output_text <- HTML(
        paste0("Here you will generate a model for your data. Feedback will be given in two steps:",
        "<br><br>", 
        "The first will serve as a <b>reflection of your input and knowledge mapping</b> (e.g., lexicon). It will classify text based on words and phrases that appear in the lexicon 'as-is', hence a great opportunity to reflect on how your knowledge mapping fits the problem domain.",
        "<br><br>",
        "This second will <b>provide you with new insights</b> by enriching the lexicon with additional semantically-similar words and leveraging Machine-Learning techniques. This is a good way to gain insights beyond the raw data and may provide new perspectives."
        # "<br>",
        # draw_model_ranks(0, 2)
               )
        )
    return(output_text)
  })
  
  
  ## this creates a simple visualization for users to see how each model is different from the others.
  draw_model_ranks <- function(speed_rank=2, control_rank=1){
    sequenceWrapper <- function(lb, ub, by=1) {
      s <- c()
      if(!ub < lb) s <- seq(lb,ub, by=by)
      return(s)
    }
    
    output_html <- "<div><span class='modelRank'>"
    output_html <- paste0(output_html, "Speed:&nbsp;")
    for (i in sequenceWrapper(1,speed_rank)){
      output_html <- paste0(output_html, "<span class='fas fa-paper-plane'></span>")
    }
    for (i in sequenceWrapper(1,(3 - speed_rank))){
      output_html <- paste0(output_html, "<span class='far fa-paper-plane modelRankGrayed'></span>")
    } 
    output_html <- paste0(output_html, "&emsp;")
    output_html <- paste0(output_html, "User Control:&nbsp;")
    for (i in sequenceWrapper(1,control_rank)){
      output_html <- paste0(output_html, "<span class='fas fa-user-circle'></span>")
    }
    for (i in sequenceWrapper(1,(3 - control_rank))){
      output_html <- paste0(output_html, "<span class='far fa-user-circle modelRankGrayed'></span>")
    } 
    output_html <- paste0(output_html, "</span></div>")
    
    return(output_html)
  }
  
  
  
  #' Observer for the generate new model button. Based on the chosen model, it will 
  #' select the correct algorithm function to call from the 'models' file(s).
  #' 
  observeEvent(input$create_simplified_model_btn,{
    
    ## pre-run checks
    if (is.null(work_env$data())){
      sendSweetAlert(session = session, 
                     title = "Labeled data missing", 
                     text = "A data file is required for generating a model. Please upload it using the data tab.",
                     type = "error")
      return(NULL)
    }
    if (is.null(work_env$lexicon())){
      sendSweetAlert(session = session, 
                     title = "Lexicon missing", 
                     text = "The model type you have chosen requires a lexicon file. Please upload it using the data tab",
                     type = "error")
      return(NULL)
    }
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    # Create a callback function to update progress.
    # Each time this is called:
    # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
    #   distance. If non-NULL, it will set the progress to that value.
    # - It also accepts optional detail text.
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }
    
    use_external_corpus <- FALSE
    
    
    sendModeltoDB <- function(res, model_type){
      newly_created_model_id <- storeModel(
        current_user_id(), 
        current_project_id(), 
        model_type,
        res$model
      )
      
      storeModelRunInfo(
        model_id = newly_created_model_id,
        iteration_number = 1,
        auc = res$auc,
        leave_out_auc = res$leave_out_auc,
        res$confusion_matrix,
        res$assessment_set,
        res$word_scores,
        lexicon_id = getProjectLexiconID(current_project_id())
      )
      return(newly_created_model_id)
    }
    
    
    
    
    is_there_concept_column = "concept" %in% colnames(work_env$data())
    
    ## Unsupervised Lexicon model is being run first - reflection on domain expert's knowledge mapping
    progress$set(message = "Gathering Reflection Feedback", value = 0)
    res = callModule(unsupervised_lexicon_model, "unsupervised_lexicon", work_env, updateProgress, use_concepts = is_there_concept_column)
    unsupervised_model_id <- sendModeltoDB(res, model_type = 'Unsupervised - Lexicon')
    
    ## Supervised Smart model is being run second - providing new insights to the domain expert
    progress$set(message = "Gathering New Insigiths", value = 0)
    res = callModule(supervised_smart_lexicon_model, "supervised_smart_lexicon_wikipedia", work_env, updateProgress, use_external_corpus, use_concepts = is_there_concept_column)
    supervised_model_id <- sendModeltoDB(res, model_type = 'Supervised - Smart Lexicon')
    
    # if (input$model_type_select=="Supervised" & input$classification_type_select=="Lexicon"){
    #   res = callModule(supervised_lexicon_model, "supervised_lexicon_wikipedia", work_env, updateProgress, use_external_corpus, use_concepts = is_there_concept_column)
    # }
    # if (input$model_type_select=="Unsupervised" & input$classification_type_select=="Smart Lexicon"){
    #   res = callModule(unsupervised_smart_lexicon_model, "unsupervised_smart_lexicon_wikipedia", work_env, updateProgress, use_external_corpus, use_concepts = is_there_concept_column)
    # }
    # if (input$model_type_select=="Supervised" & input$classification_type_select=="Baseline"){
    #   res = callModule(supervised_baseline_model, "supervised_baseline_wikipedia", work_env, updateProgress, use_external_corpus)
    # }
    
    assignIteratorBehavior(1, unsupervised_model_id = unsupervised_model_id, supervised_model_id = supervised_model_id)
    assignDeleteModelBehavior(1, unsupervised_model_id = unsupervised_model_id, supervised_model_id = supervised_model_id)
    num_of_models_created_during_this_session(num_of_models_created_during_this_session() + 2)
    num_of_iterations_run_during_this_session(num_of_iterations_run_during_this_session()+1)
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
                   colors = rev(wes_palette(n=5, name="Darjeeling1")))
    output_plot <- plot + theme(plot.background = element_rect(color = NA, fill = "#ecf0f5"))
    return(output_plot)
  },  
  bg="#ecf0f5", 
  execOnResize=T
  )
  
  
    
  #' Observer for when the user switches between projects, thus the list of models in the UI
  #' needs to be updated to the models within the newly chosen project. This observer is
  #' also used to trigger for whenever a user finishes running an iteration to update UI. 
  observeEvent({
    current_project_id()
    iteration_done_flag()
  },{
    model_info <- getAllModelsRunInfos(current_project_id())
    storedDB(model_info)
  }, ignoreInit = TRUE)
  
  
  
  #' Observer for updating the information needed for the UI components (AUC values, etc). 
  #' It invokes on the following events:
  #' 1. on first opening of the 'model-generation' screen
  #' 2. on a click of 'create new model' button
  #' 3. on a click of 'iterate model' button
  observe({
    number_of_iterate_buttons <- length(unique(storedDB()$iteration_number))
    iterate_btn_names <- paste0("iteration", seq_len(number_of_iterate_buttons), "-iterate_btn")
    delete_btn_names <- paste0("iteration", seq_len(number_of_iterate_buttons), "-delete_btn")
    lapply(cbind(iterate_btn_names, delete_btn_names), function(x){
      observeEvent({
        input$create_simplified_model_btn
        input[[x]]
        1
      },{
        model_info <- getAllModelsRunInfos(current_project_id())
        storedDB(model_info)
        total_number_of_iterations_in_project <- unique(model_info$iteration_number)
        
        for(cur_iteration in total_number_of_iterations_in_project){
          # cur_model_runs <- model_info %>% filter(model_id == cur_model_id)
          # for (i in 1:nrow(cur_model_runs)) {
          
            # Need local so that each item gets its own number. Without it, the value
            # of i in the renderPlot() will be the same across all instances, because
            # of when the expression is evaluated.
            local({
              my_iteration <- cur_iteration
              cur_unsupervised <- model_info %>% filter(model_type == 'Unsupervised - Lexicon' & iteration_number==my_iteration)
              cur_supervised <- model_info %>% filter(model_type == 'Supervised - Smart Lexicon' & iteration_number==my_iteration)
              
              unsupervised_conf_mat <- unserializeObject(cur_unsupervised$serialized_confusion_matrix)
              supervised_conf_mat <- unserializeObject(cur_supervised$serialized_confusion_matrix)
              
              # modelStatsName <- paste("modelStats", my_model_id, "i", my_i, sep="")
              # output[[modelStatsName]] <- renderUI({
              #   tagList(
              #     column(
              #       width = 2,
              #       tags$div(title="Area Under the ROC Curve (AUC) measures the overall performance of a binary classifier system and demonstrates its diagnostic ability while its discrimination threshold is varied. The AUC value is within the range of 0.5-1.0, where 0.5 represents the performance of a random classifier and 1.0 would correspond to a perfect classifier.",
              #                `data-toggle`="tooltip",
              #                `data-placement`="bottom",
              #       descriptionBlock(
              #         number = round(cur_model$auc[my_i], digits = 3),
              #         numberColor = ifelse(cur_model$auc[my_i]>0.85, "green", ifelse(cur_model$auc[my_i]>0.65, "yellow", "red")),
              #         # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black. 
              #         header = "AUC", 
              #         rightBorder = FALSE,
              #         marginBottom = FALSE
              #       )
              #       )
              #     ),
              #     column(
              #       width = 3,
              #       descriptionBlock(
              #         number = round(cur_model$leave_out_auc[my_i], digits = 3),
              #         numberColor = ifelse(cur_model$leave_out_auc[my_i]>0.85, "green", ifelse(cur_model$leave_out_auc[my_i]>0.65, "yellow", "red")),
              #         # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black. 
              #         header = "Leave-out AUC", 
              #         rightBorder = FALSE,
              #         marginBottom = FALSE
              #       )
              #     ),
              #     column(
              #       width = 2,
              #       tags$div(title="Precision: proportion of positive predictions that were true (classification exactness).",
              #                `data-toggle`="tooltip",
              #                `data-placement`="bottom",
              #       descriptionBlock(
              #         number = round(conf_mat$byClass['Precision'][[1]], digits = 3),
              #         # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black. 
              #         header = "Precision", 
              #         rightBorder = FALSE,
              #         marginBottom = FALSE
              #       )
              #       )
              #     ),
              #     column(
              #       width = 2,
              #       tags$div(title="Recall: proportion of true-positive cases that were predicted as positive (classification completeness).",
              #                `data-toggle`="tooltip",
              #                `data-placement`="bottom",
              #       descriptionBlock(
              #         number = round(conf_mat$byClass['Recall'][[1]], digits = 3),
              #         # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black. 
              #         header = "Recall", 
              #         rightBorder = FALSE,
              #         marginBottom = FALSE
              #       )
              #       )
              #     ),
              #     column(
              #       width = 3,
              #       descriptionBlock(
              #         number = round(conf_mat$byClass['Balanced Accuracy'][[1]], digits = 3),
              #         # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black. 
              #         header = "Balanced Accuracy", 
              #         rightBorder = FALSE,
              #         marginBottom = FALSE
              #       )
              #     ),
              #     br()
              #   )
              # })
              # 
              # confusionMatrixName <- paste("confusionMatrix", my_model_id, "i", my_i, sep="")
              # output[[confusionMatrixName]] <- renderPrint({
              #   conf_mat$table
              # })
              ModelF1ScoreName <- paste("F1Score", my_iteration, "i", sep="")
              output[[ModelF1ScoreName]] <- renderUI({
                #model_type_string <- cur_model$model_type
                output_html <- NULL
                f1_explain = 'F1 score is a way of combining precision and recall to measure performance. It is an effective measure for heavily imbalanced datasets.'
                diff_f1_between_models <- supervised_conf_mat$byClass[7] - unsupervised_conf_mat$byClass[7]
                diff_auc_between_models <- ((cur_supervised$auc - cur_unsupervised$auc) + (cur_supervised$leave_out_auc - cur_unsupervised$leave_out_auc))/2
                if (diff_f1_between_models<0){
                  if (diff_auc_between_models>0){
                    ## this is when AUC for supervised model is better, but F1 score (recall+precision) is not
                    output_html <- HTML(paste0("<p>For the selected recall value (", work_env$hard_recall_value(), "), your knowledge mapping's <span data-toggle='tooltip' data-placement='bottom' title='",f1_explain,"'>F1 score</span> is <b>", round(unsupervised_conf_mat$byClass[7], 4), "</b>, which is 
                     higher than machine-learning score of <b>", round(supervised_conf_mat$byClass[7], 4), "</b>. However, the machine-enhanced model had, on average, a higher AUC value (higher by <b>", round(diff_auc_between_models,3) ,"</b>), which means that across all scenarios it can perform better. Please use the new-insights feedback to improve your knowledge mapping.</p>"))
                  } else {
                    ## this is both AUC and F1 scores have not improved by using supervised model
                    output_html <- HTML(paste0("<p>Awesome. Your knowledge mapping performed well on the given validation dataset. The <span data-toggle='tooltip' data-placement='bottom' title='",f1_explain,"'>F1 score</span> is <b>", round(unsupervised_conf_mat$byClass[7], 4),"</b> and AUC is <b>", round(cur_unsupervised$auc,3),"</b>.
                                               Use the comparison with the machine's model to <b>see which knowledge maping terms you should preserve and not change</b>. In this iteration, the changes made by the supervised model performed worse.</p>"))
                  }
                } else {
                  if (diff_auc_between_models>0){
                  ## this is when both AUC and F1 score can be improved using supervised model suggestions
                  output_html <- HTML(paste0("<p>Your knowledge mapping's <span data-toggle='tooltip' data-placement='bottom' title='",f1_explain,"'>F1 score</span> for this iteration is <b>", round(unsupervised_conf_mat$byClass[7], 4), "</b>. 
                                           Using the provided machine learning feedback, this can be further improved to reach <b>", round(supervised_conf_mat$byClass[7], 4), "</b>, and improve AUC by <b>", round(diff_auc_between_models,3) ,
                                             "</b>. Please see the detailed feedback.</p>"))
                  } else {
                  ## this is when F1 can be improved, but AUC may be a bit lower (though in some case leave out auc is still higher).
                    output_html <- HTML(paste0("<p>Your knowledge mapping's <span data-toggle='tooltip' data-placement='bottom' title='",f1_explain,"'>F1 score</span> for this iteration is <b>", round(unsupervised_conf_mat$byClass[7], 4), "</b>. 
                                           Using the provided machine learning feedback, this can be further improved to reach <b>", round(supervised_conf_mat$byClass[7], 4), "</b>. Please see the detailed feedback.</p>"))
                  }
                }
                return(output_html)
              })
            })
          #}
        }
      })
    })
  })
  
  
  
  #' Observer for granting the 'iterate' buttons their functionality. It is invoked only
  #' when the screen is first opened.
  #' 
  observeEvent({
    1
  },
  {
    model_info <- getAllModelsRunInfos(current_project_id())
    total_number_of_iterations_in_project <- unique(model_info$iteration_number)
    for(iteration_id in total_number_of_iterations_in_project){
      model_runs_for_iteration <- model_info %>% filter(iteration_number==iteration_id)
      index_of_model_id_column = 1
      index_of_unsupervised_row = 1
      index_of_supervised_row = 2
      assignIteratorBehavior(iteration_id, unsupervised_model_id = model_runs_for_iteration[index_of_unsupervised_row,index_of_model_id_column], supervised_model_id = model_runs_for_iteration[index_of_supervised_row, index_of_model_id_column])
      assignDeleteModelBehavior(iteration_id, unsupervised_model_id = model_runs_for_iteration[index_of_unsupervised_row,index_of_model_id_column], supervised_model_id = model_runs_for_iteration[index_of_supervised_row, index_of_model_id_column])
    }
  }, ignoreInit = FALSE)
  
 
  
  #' Dynamic menu for model type selection 
  output$dynamic_model_select_box <- renderUI({
    ns <- session$ns
    
    elem1 <- radioGroupButtons(
      inputId = ns("model_type_select"),
      label = "ML Model Type", 
      choices = c("Supervised", "Unsupervised"),
      status = "primary",
      selected = input$model_type_select
    )
    
    return(
      list(
      tags$div(style="display: inline-block;vertical-align:top;",
               elem1),
      tags$div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
      tags$div(style="display: inline-block;vertical-align:top;",
               elem2),
      tags$div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
      tags$div(style="display: inline-block;vertical-align:top;",
               elem3)
    )
    )
  })
  
  elem2 <- renderUI({
    ns <- session$ns
    if (input$model_type_select=="Unsupervised"){
      out <- radioGroupButtons(
        inputId = ns("classification_type_select"),
        label = "Lexicon Selection", 
        choices = c("Lexicon", "Smart Lexicon"),
        status = "primary",
        selected = input$classification_type_select
      )
    } else {
      out <- radioGroupButtons(
        inputId = ns("classification_type_select"),
        label = "Lexicon Selection", 
        choices = c("Lexicon", "Smart Lexicon", "Baseline"),
        status = "primary",
        selected = input$classification_type_select
      )
    }
    return(out)
  })
  
  elem3 <- renderUI({
    ns <- session$ns
    if (!is.null(input$model_type_select) & !is.null(input$classification_type_select)){
      if(input$model_type_select=="Unsupervised" & input$classification_type_select=="Lexicon"){
        out <- NULL
      } else {
        out <- radioGroupButtons(
          inputId = ns("model_corpus_select"),
          label = "Enhancement Corpus",
          choices = c("Wikipedia", "External corpus"),
          status = "primary",
          selected = "Wikipedia"
        )
      }
    } else {
      return(NULL)
    }
    return(out)
  })
  
  
  
  output$additional_model_config_selectors <- renderUI({
    ns <- session$ns
    
    ## refresh the list of concepts in the project
    work_env$current_concept_list(getProjectConcepts(current_project_id()))
    
    return(
      list(
        div(style="display:inline-block", pickerInput(
          inputId = ns("concept_selector"),
          label = "Concepts", 
          choices = work_env$current_concept_list()$name,
          multiple = TRUE
        )),
        div(style="display:inline-block", dropdownButton(
          sliderInput(ns("hard_recall_value"), "Min. Recall:",
                      min = 0, 
                      max = 1,
                      value = work_env$hard_recall_value(),
                      step = 0.1),
          circle = FALSE, status = "primary", icon = icon("gear"), width = "100px",
          tooltip = tooltipOptions(title = "Advanced Settings")
        ))
      )
    )
  })
  
  
  
  assignIteratorBehavior <- function(iteration_id_for_iterator_behavior, unsupervised_model_id, supervised_model_id){
    local({
      iteration_id <- iteration_id_for_iterator_behavior
      iterate_button_name <- paste0("iteration", iteration_id, "-iterate_btn")
      
      observeEvent(input[[iterate_button_name]],{
        # Recalculate the new data sets as preparation for the next iteration of the model
        new_work_env <- advanceValidationSet(work_env$data(), iteration_id+1)
        work_env$data(new_work_env)
        setProjectData(current_project_id(), work_env)
        
        # Create a Progress object
        progress <- shiny::Progress$new()
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())
        
        # Create a callback function to update progress.
        # Each time this is called:
        # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
        #   distance. If non-NULL, it will set the progress to that value.
        # - It also accepts optional detail text.
        updateProgress <- function(value = NULL, detail = NULL) {
          if (is.null(value)) {
            value <- progress$getValue()
            value <- value + (progress$getMax() - value) / 5
          }
          progress$set(value = value, detail = detail)
        }
        
        # model_run_info <- getModelRunInfo(model_id)
        is_there_concept_column = "concept" %in% colnames(work_env$data())
        
        # Run the next iteration of the model and save results
        ## Unsupervised Lexicon
        progress$set(message = "Gathering Reflection Feedback", value = 0)
        res_unsupervised = callModule(unsupervised_lexicon_model, "unsupervised_lexicon", work_env, updateProgress, use_concepts = is_there_concept_column)
        ## need to update the actual model in the DB as well
        updateModel(unsupervised_model_id, res_unsupervised$model)
        
        ## Supervised Smart Lexicon
        progress$set(message = "Gathering New Insights", value = 0)
        res_supervised = callModule(supervised_smart_lexicon_model, "supervised_smart_lexicon_wikipedia", work_env, updateProgress, use_concepts = is_there_concept_column)
        ## need to update the actual model in the DB as well
        updateModel(supervised_model_id, res_supervised$model)
        
        # if(model_run_info$model_type[1]=="Supervised - Lexicon - Wikipedia"){
        #   res = callModule(supervised_lexicon_model, "supervised_lexicon_wikipedia", work_env, updateProgress, use_concepts = is_there_concept_column)
        # }
        # if(model_run_info$model_type[1]=="Supervised - Baseline - Wikipedia"){
        #   res = callModule(supervised_baseline_model, "supervised_baseline_wikipedia", work_env, updateProgress)
        # }
        # if(model_run_info$model_type[1]=="Unsupervised - Smart Lexicon - Wikipedia"){
        #   res = callModule(unsupervised_smart_lexicon_model, "unsupervised_smart_lexicon_wikipedia", work_env, updateProgress, use_concepts = is_there_concept_column)
        # }
        # if(model_run_info$model_type[1]=="Supervised - Lexicon - External corpus"){
        #   res = callModule(supervised_lexicon_model, "supervised_lexicon_wikipedia", work_env, updateProgress, use_external_corpus = TRUE, use_concepts = is_there_concept_column)
        # }
        # if(model_run_info$model_type[1]=="Supervised - Smart Lexicon - External corpus"){
        #   res = callModule(supervised_smart_lexicon_model, "supervised_smart_lexicon_wikipedia", work_env, updateProgress, use_external_corpus = TRUE, use_concepts = is_there_concept_column)
        # }
        # if(model_run_info$model_type[1]=="Supervised - Baseline - External corpus"){
        #   res = callModule(supervised_baseline_model, "supervised_baseline_wikipedia", work_env, updateProgress, use_external_corpus = TRUE)
        # }
        # if(model_run_info$model_type[1]=="Unsupervised - Smart Lexicon - External corpus"){
        #   res = callModule(unsupervised_smart_lexicon_model, "unsupervised_smart_lexicon_wikipedia", work_env, updateProgress, use_external_corpus = TRUE, use_concepts = is_there_concept_column)
        # }
        
        

        res = res_unsupervised
        storeModelRunInfo(
          unsupervised_model_id,
          iteration_number = 1+iteration_id,
          auc = res$auc,
          leave_out_auc = res$leave_out_auc,
          res$confusion_matrix,
          res$assessment_set,
          res$word_scores,
          lexicon_id = getProjectLexiconID(current_project_id())
        )
        
        res = res_supervised
        storeModelRunInfo(
          supervised_model_id,
          iteration_number = 1+iteration_id,
          auc = res$auc,
          leave_out_auc = res$leave_out_auc,
          res$confusion_matrix,
          res$assessment_set,
          res$word_scores,
          lexicon_id = getProjectLexiconID(current_project_id())
        )
        
        
        iteration_done_flag(!iteration_done_flag())
        num_of_iterations_run_during_this_session(num_of_iterations_run_during_this_session()+1)
      })
    })
  }
  
  
  
  assignDeleteModelBehavior <- function(iteration_id_for_delete_model_behavior, unsupervised_model_id, supervised_model_id){
    local({
      iteration_id <- iteration_id_for_delete_model_behavior
      delete_model_button_name <- paste0("iteration", iteration_id, "-delete_btn")
      
      observeEvent(input[[delete_model_button_name]],{
        deleteModel(unsupervised_model_id)
        deleteModel(supervised_model_id)
        
        ## signal that the UI needs to update since we deleted a model, we'll just use the same signal flag as the iterator one
        iteration_done_flag(!iteration_done_flag())
        ## signal to external modules that the number of models has changed in this session
        num_of_models_created_during_this_session(num_of_models_created_during_this_session() - 2)
      })
    })
  }
  
  
  #' Observer for updating the stored value of the hard recall based on the value in the slider, 
  #' to be used for model generation.
  #' 
  observeEvent({
    input$hard_recall_value
  },{
    work_env$hard_recall_value(input$hard_recall_value)
  })

  
  
  ## return reactive values needed for other modules in order to know when some items were updated/changed.
  return(
    reactiveValues(
      num_of_models_created_during_this_session = num_of_models_created_during_this_session,
      num_of_iterations_run_during_this_session = num_of_iterations_run_during_this_session
    )
  )
}