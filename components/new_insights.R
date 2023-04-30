library(ggthemes)
library(vip)
library(highcharter)
library(tidytext)
library(shinyPagerUI)
library(d3Tree)


#' Function that colors text based on the word score values. Blue for words with a negative score (high probability for negative classification), 
#' red for words with a positive score (higher probability for positive classification), and black otherwise.
#' 
#' @param text
#' @param word_scores
#' 
#' @return text that has HTML tags to indicate color
#' 
colorize_text <- function(text, word_scores){
  word_score_table <- word_scores %>%
    mutate(processed_word = word)
  
  # individuate words
  unique_words <- function(x) {
    purrr::map(.x = x,
               .f = ~ unique(base::strsplit(x = ., split = " |[\\]n")[[1]],
                             collapse = " "))
  }
  
  ## Find all skip-ngrams of length up to 3, that appear in text and used by model. 
  ## Calculate score for each word based on these skip-ngrams
  ngrams_df <- data.frame(processed_text = preprocess(text)) %>%
    unnest_tokens(ngrams, processed_text, token = "skip_ngrams", n = 3L) %>%
    mutate(processed_word = str_replace_all(ngrams, " ", "_")) %>%
    inner_join(word_score_table, by = "processed_word") %>%
    separate_rows(processed_word, sep = "_") %>%
    group_by(processed_word) %>% 
    mutate(avg_score = mean(score), original_ngrams = ngrams) %>% 
    ungroup()
  
  ## if the above skip-grams dataframe is not empty, and found skip-grams in the text, then calculate average score in case a word appears in multiple ngrams.
  if (nrow(ngrams_df)>0){
    ngrams_df <- ngrams_df %>%
      group_by(processed_word) %>%
      distinct() %>%
      mutate(original_ngrams = paste(original_ngrams, collapse = ",")) %>% 
      ungroup() %>%
      select(processed_word, avg_score, original_ngrams) %>%
      distinct()
  }
  
  # creating a dataframe with colorized text
  df <- 
    tibble::enframe(unique_words(x = text)) %>%
    unnest(cols = c(value)) %>%
    mutate(processed_word = preprocess(value)) %>%
    left_join(ngrams_df, by = "processed_word") %>%
    # here you can specify the color/word combinations you need 
    mutate(value2 = dplyr::case_when(
      avg_score < 0 ~ paste0('<span style = "color:blue" data-toggle="tooltip" data-placement="bottom" title="', paste0("Propensity: ", round(avg_score,3)), " (", original_ngrams, ")",'">', value, '</span>'),
      avg_score > 0 ~ paste0('<span style = "color:red" data-toggle="tooltip" data-placement="bottom" title="', paste0("Propensity: ", round(avg_score,3)), " (", original_ngrams, ")",'">', value, '</span>'),
      avg_score == 0 ~ paste0('<span style = "color:black" data-toggle="tooltip" data-placement="bottom" title="', paste0("Propensity: ", round(avg_score,3)), " (", original_ngrams, ")",'">', value, '</span>'),
      TRUE ~ value)) %>%
    select(., -value)
  
  out <- paste(df$value2, collapse = " ")
  return(out)
}



colorize_text_accurate_and_slow <- function(text, word_scores){
  get_word_coloring_table <- function(text, word_scores){
    word_score_table <- word_scores %>%
      mutate(processed_word = word)
    
    # individuate words
    unique_words <- function(x) {
      purrr::map(.x = x,
                 .f = ~ unique(base::strsplit(x = ., split = " |[\\]n")[[1]],
                               collapse = " "))
    }
    
    text_df <- 
      tibble::enframe(unique_words(x = text)) %>%
      unnest(cols = c(value)) %>%
      mutate(processed_word = preprocess(value))
    
    skip_gram_size = 2L
    
    df <- text_df %>%
      filter(processed_word!="") %>%
      mutate(row_id = as.integer(rownames(.))) %>%
      rowwise() %>%
      mutate(word_context = .$processed_word[c(max((row_id-skip_gram_size),1):min((row_id+skip_gram_size), nrow(.)))] %>% 
               paste(collapse = " ")) %>%
      select(-row_id, -name)
    
    for (i in 1:nrow(df)){
      word_res = single_text_skip_gram_score(df$processed_word[i], df$word_context[i], word_score_table)
      if (!is.null(word_res)){
        df[i,'avg_score'] = word_res$avg_score
        df[i,'original_ngrams'] = word_res$original_ngrams
      } else {
        df[i,'avg_score'] = as.double(NA)
        df[i,'original_ngrams'] = ""
      }
    }
    return(df)
  }
  
  single_text_skip_gram_score <- function(word, processed_text, word_score_table){
    ngrams_df <- data.frame(processed_text) %>%
      unnest_tokens(ngrams, processed_text, token = "skip_ngrams", n = 3L) %>%
      mutate(processed_word = str_replace_all(ngrams, " ", "_")) %>%
      inner_join(word_score_table, by = "processed_word") %>%
      separate_rows(processed_word, sep = "_") %>%
      group_by(processed_word) %>% 
      transmute(avg_score = mean(score), original_ngrams = ngrams) %>% 
      ungroup() %>%
      filter(processed_word == word)
    
    ## if the above skip-grams dataframe is not empty, and found skip-grams in the text, then calculate average score in case a word appears in multiple ngrams.
    if (nrow(ngrams_df)>0){
      ngrams_df <- ngrams_df %>%
        group_by(processed_word) %>%
        distinct() %>%
        mutate(original_ngrams = paste(original_ngrams, collapse = ",")) %>% 
        ungroup() %>%
        distinct()
    }
    
    if(nrow(ngrams_df)==0){
      return(NULL)
    }
    return(ngrams_df)
  }
  
  # creating a dataframe with colorized text
  df <- 
    get_word_coloring_table(text, word_scores) %>%
    # here you can specify the color/word combinations you need 
    mutate(value2 = dplyr::case_when(
      abs(avg_score) < 0.01 ~ paste0('<span style = "color:black" data-toggle="tooltip" data-placement="bottom" title="', paste0("Propensity: ", round(avg_score,3)), " (", original_ngrams, ")",'">', value, '</span>'),
      avg_score < 0 ~ paste0('<span style = "color:blue" data-toggle="tooltip" data-placement="bottom" title="', paste0("Propensity: ", round(avg_score,3)), " (", original_ngrams, ")",'">', value, '</span>'),
      avg_score > 0 ~ paste0('<span style = "color:red" data-toggle="tooltip" data-placement="bottom" title="', paste0("Propensity: ", round(avg_score,3)), " (", original_ngrams, ")",'">', value, '</span>'),
      TRUE ~ value)) %>%
    select(., -value)
  
  out <- paste(df$value2, collapse = " ")
  return(out)
}

#------------------------------------ UI SIDE --------------------------------------


#' This function generates the UI components for the explanation screen.
#'
#' @param id
#' @param label
#' 
#' @return list of UI components
insightsEngineWidget <- function(id, label = "insightsUI") {
  ns <- NS(id)
  
  
  return(
    fluidPage(
      
      column(12,
             div(style="display:inline-block;",
                 uiOutput(ns("iteration_selector"))
             ),
             div(style="display:inline-block;", 
                 uiOutput(ns("exportFiles"))
             )
      ),
      fluidRow(
        column(12,
               #box(width = 12, title = strong('Knowledge Mapping Performance'),
                   uiOutput(ns("modelStats")),
               #)
        ),
        # column(12,
        #        box(width = 12, title = strong('Lexicon Tree'),
        #         d3treeOutput(ns("categorization_lexicon_context"), width = "100%")
        #        )
        # ),
        column(12,
               box(width = 12, title = strong("New Insights by Examples from The Validation Set"),
                   # column(7,
                   uiOutput(ns("classificationErrorAnalysisRadioButtons")),
                   uiOutput(ns("classificationErrorAnalisys")),
                   pageruiInput(ns("pager"), page_current = 1, pages_total = 1),
                   # ),
                   # column(5,
                   #        d3treeOutput(ns("categorization_lexicon_context"), width = "100%", height = "600px")
                   #       ),
                   footer = list(
                     htmlOutput(ns("footnote_for_classification_error_analysis"))
                   )
               )
        ),
        column(12,
               box(width = 12, title = strong('Term Frequency Across All Documents in Validation'),
                   DT::dataTableOutput(ns("termSummaryTable"))
               )
        ),
        column(12,
               box(width = 6, title = strong('New Terms (not in Lexicon)'),
                   tableOutput(ns("topNewTerms"))
               ),
               box(width = 6, title = strong('Terms to Re-Examine in Lexicon'),
                   tableOutput(ns("topTermsToExamine"))
               ),

        ),
        
        
        column(12,
               box(width = 12, title = strong("Lexicon Terms vs. Model Terms"),
                   plotOutput(ns("lexiconTermsPlot"),
                              height = 1500,
                              brush = brushOpts(id = ns("plot_brush"),
                                                fill = "#ccc",
                                                direction = "y")
                   ),
                   footer = htmlOutput(ns("footnote_for_lexicon_terms_plot"))
               )
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
#' @param reactive_values_from_other_modules
#' 
#' @return NULL
insightsEngine <- function(input, output, session, work_env, reactive_values_from_other_modules){
  
  filtered_assessment_set <- reactiveVal(NULL)
  # search_box_filtered_assessment_set <- reactiveVal(NULL)
  full_assessment_set <- reactiveVal(NULL)
  selected_words <- reactiveVal(NULL)
  list_of_models_in_project <- reactiveVal(NULL)
  export_new_terms_table <- reactiveVal(NULL)
  export_existing_terms_table <- reactiveVal(NULL)
  
  
  #' Classification Error Analysis
  output$classificationErrorAnalisys <- renderUI({
    if (is.null(selected_words())){
      return(NULL)
    }
    
    ## figure out the lexicon that was used for the model we selected to examine
    if (is.null(input$iteration_select_for_inspection)){
      return(NULL)
    }
    all_model_infos <- getAllModelsRunInfos(current_project_id())
    model_run_info <- all_model_infos %>% filter(iteration_number == input$iteration_select_for_inspection & model_type == 'Supervised - Smart Lexicon')
    
    lex <- NULL
    if (!is.na(model_run_info$lexicon_id)){
      lex <- getLexiconByID(model_run_info$lexicon_id)$lexicon
    }
    
    
    assessment_set <- lexiconBrushFiltering(selected_words(), 
                                            full_assessment_set(),#search_box_filtered_assessment_set(),
                                            lex)
    
    ## in case the lexicon brush filter was empty, bring the whole set
    if (is.null(assessment_set)){
      assessment_set <- full_assessment_set() # search_box_filtered_assessment_set()
    }
    filtered_assessment_set(assessment_set)
    
    ## in case some part of the screen was loaded before the input$results_explore_select
    ## had a value
    if (is.null(input$results_explore_select)){
      return(NULL)
    }
    
    # false negatives
    if (input$results_explore_select==1){
      results <- filtered_assessment_set() %>%
        filter(fusion.label != Prediction) %>%
        filter(Prediction=="NS")
    }
    # false positives
    if (input$results_explore_select==2){
      results <- filtered_assessment_set() %>%
        filter(fusion.label != Prediction) %>%
        filter(Prediction=="SU")
    }
    # true positives
    if (input$results_explore_select==3){
      results <- filtered_assessment_set() %>%
        filter(fusion.label == Prediction) %>%
        filter(Prediction=="SU")
    }
    # true negatives
    if (input$results_explore_select==4){
      results <- filtered_assessment_set() %>%
        filter(fusion.label == Prediction) %>%
        filter(Prediction=="NS")
    }
    current_page <- min(input$pager$page_current, ceiling(nrow(results)/10))
    updatePageruiInput(session = session, inputId = "pager", pages_total = ceiling(nrow(results)/10), page_current = current_page)
    
    results <- results %>%
      slice(((current_page-1)*10 + 1):(current_page*10))
    colorized_text <- lapply(results$fusion.text, function(x){
      list(
        HTML("<p>", "&#8227;", colorize_text_accurate_and_slow(x, selected_words()), "</p>")
      )
    })
    
    return(
      colorized_text
    )
  })
  
  
  
  #' Helper function to generate bar plot based on word scores
  #' 
  #' @param datatable
  #' @param x
  #' 
  generateBarPlot <- function(datatable, x, is_lexicon_color){
    cond = NULL
    if (is_lexicon_color){
      cond = datatable$Label  ## BUG: the name of the column here is hard coded to be 'Label'. When this is fixed in the lexicon building file, fix here too.
      color_values = c("lightblue", "firebrick")
    } else {
      cond = datatable$score > 0
      color_values = c("gray80", "gray50")
    }
    plot <- ggplot(data = datatable, aes(x = x, y = score, fill = cond, label = round(score, 3))) +
      geom_bar(stat="identity", alpha=.6, width=.8, show.legend = FALSE) +
      scale_fill_manual(values = color_values) +
      geom_text(size = 3, position = position_stack(vjust = 0.5)) +
      coord_flip() +
      xlab("") +
      ylab("") +
      theme_hc(base_size = 14, base_family = "sans", style = "default") +
      theme(
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x = element_line(color='gray', size = .3)
      )
    return(plot)
  }
  
  
  
  #' Lexicon Terms Plot
  output$lexiconTermsPlot <- renderPlot({
    ## figure out the lexicon that was used for the model we selected to examine
    if (is.null(input$iteration_select_for_inspection)){
      return(NULL)
    }
    all_model_infos <- getAllModelsRunInfos(current_project_id())
    model_run_info <- all_model_infos %>% filter(iteration_number == input$iteration_select_for_inspection & model_type == 'Supervised - Smart Lexicon')
    
    ## if baseline model, it won't have a lexicon
    if(is.na(last(model_run_info$lexicon_id)) | is.null(selected_words())){
      return(NULL)
    }
    
    lex <- getLexiconByID(last(model_run_info$lexicon_id))$lexicon %>%
      mutate(word = trimws(tolower(terms))) %>%
      mutate(word = preprocess(word)) %>%
      select(-Category, -`Sub-Category`) %>%
      distinct()
    
    # check for terms that are in both group labels
    dups_term_only <- duplicated(lex %>% select(terms), fromLast = F)
    dups_label_n_term <- duplicated(lex %>% select(Label, terms), fromLast = F)
    # subtract dups of label-term pairs from dupes among terms only, this way we get dupes between label groups
    duplicated_terms <- which(dups_term_only - dups_label_n_term!=0)
    if (length(duplicated_terms!=0)) {
      lex <- lex[-duplicated_terms,]
    }
    
    word_score_table <- selected_words() %>%
      mutate_if(is.character, stringr::str_replace_all, pattern = "_", replacement = " ") %>%
      filter(score > -0.9) %>%
      inner_join(lex, by = "word") %>%
      mutate(terms = factor(terms)) %>%
      mutate(terms = fct_reorder(terms, score)) %>%
      arrange(score)
    
    plot <- generateBarPlot(word_score_table, word_score_table$terms, TRUE)
    ## if lexicon provides Category column, use it to facet_wrap
    if ("Category" %in% names(word_score_table)){
      plot <- plot + facet_wrap( ~ word_score_table$Category, ncol=4)
    }    
    return(plot)
  })
  
  
  
  #' A helper function that generate a dataframe of the results selected by the user on the 
  #' lexicon plot (brush filter). It will return NULL if nothing is selected.
  #' 
  #' @param word_scores
  #' @param assessment_set
  #' @param lexicon
  #' 
  #' @return dataframe with the filtered results
  #' 
  lexiconBrushFiltering <- function(word_scores, assessment_set, lexicon){
    # if baseline model, it won't have a lexicon
    if(is.null(lexicon)){
      return(NULL)
    }
    
    lex <- lexicon %>%
      mutate(word = trimws(tolower(terms))) %>%
      mutate(word = preprocess(word)) %>%
      distinct()
    
    word_score_table <- word_scores %>%
      mutate_if(is.character, stringr::str_replace_all, pattern = "_", replacement = " ") %>%
      inner_join(lex, by = "word") %>%
      mutate(terms = factor(terms)) %>%
      mutate(terms = fct_reorder(terms, score)) %>%
      arrange(score)
    
    # With base graphics, need to tell it what the x and y variables are.
    selected_words <- brushedPoints(word_score_table, input$plot_brush, xvar = "score", yvar = "terms")
    
    output <- data.frame()
    if(nrow(selected_words)==0){
      return(NULL)
    }
    for (i in c(1:nrow(selected_words))){
      cur_term = as.character(selected_words$word[i])
      
      out <- assessment_set %>%
        filter(
          str_detect(
            preprocessed_text_for_classification, paste0("\\b",cur_term,"\\b")
          )
        )
      output <- rbind(output, out)
    }
    
    return(
      output %>%
        distinct()
    )
  }
  
  
  
  #' Radio buttons to choose which results to show
  output$classificationErrorAnalysisRadioButtons <- renderUI({
    ns <- session$ns
    
    cm <- c(0,0,0,0)
    if (!is.null(filtered_assessment_set())){
      cm[3] <- filtered_assessment_set() %>%
        filter(fusion.label != Prediction) %>%
        filter(Prediction=="NS") %>% 
        nrow()
      
      cm[2] <- filtered_assessment_set() %>%
        filter(fusion.label != Prediction) %>%
        filter(Prediction=="SU") %>% 
        nrow()
      
      cm[4] <- filtered_assessment_set() %>%
        filter(fusion.label == Prediction) %>%
        filter(Prediction=="SU") %>%
        nrow()
      
      cm[1] <- filtered_assessment_set() %>%
        filter(fusion.label == Prediction) %>%
        filter(Prediction=="NS") %>%
        nrow()
    }
    choices <- c(1,2,3,4)
    names(choices) <- c(paste0("False Negatives (", cm[3], ")"), 
                        paste0("False Positives (", cm[2], ")"), 
                        paste0("True Positives (", cm[4], ")"),
                        paste0("True Negatives (", cm[1], ")"))
    
    return(
      radioGroupButtons(
        inputId = ns("results_explore_select"),
        choices = choices,
        status = "primary",
        selected = 1
      )
    )
  })
  
  output$exportFiles <- renderUI({
    ns <- session$ns
    return(
      list(
        downloadButton(outputId = ns("exportValidation"), label = "Data For Next Iteration", style = "margin: 1px; margin-bottom: 28px;"),
        downloadButton(outputId = ns("exportFeedback"), label = "Export Feedback", style = "margin: 1px; margin-bottom: 28px;")
      )
    )
  })
  
  output$exportValidation <- downloadHandler(
    filename = function() {
      #paste("validation_set_", Sys.Date(), ".csv", sep = "")
      paste("validation_set_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      #write.csv(filtered_assessment_set() %>% select(-"preprocessed_text_for_classification", -"data_designation", -"fusion.text", -"fusion.label"), file, row.names = FALSE)
      write.xlsx(full_assessment_set() %>% select(-"preprocessed_text_for_classification", -"data_designation", -"fusion.text", -"fusion.label"), file, row.names = FALSE)
    }
  )
  
  
  feedbackToHTML <- function(){
    if (is.null(selected_words())){
      return(NULL)
    }
    
    ## figure out the lexicon that was used for the model we selected to examine
    if (is.null(input$iteration_select_for_inspection)){
      return(NULL)
    }
    all_model_infos <- getAllModelsRunInfos(current_project_id())
    model_run_info <- all_model_infos %>% filter(iteration_number == input$iteration_select_for_inspection & model_type == 'Supervised - Smart Lexicon')
    
    lex <- NULL
    if (!is.na(model_run_info$lexicon_id)){
      lex <- getLexiconByID(model_run_info$lexicon_id)$lexicon
    }
    
    
    assessment_set <- lexiconBrushFiltering(selected_words(), 
                                            full_assessment_set(),#search_box_filtered_assessment_set(),
                                            lex)
    
    ## in case the lexicon brush filter was empty, bring the whole set
    if (is.null(assessment_set)){
      assessment_set <- full_assessment_set() # search_box_filtered_assessment_set()
    }
    filtered_assessment_set(assessment_set)
    
    ## in case some part of the screen was loaded before the input$results_explore_select
    ## had a value
    if (is.null(input$results_explore_select)){
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
    
    progress$set(message = "Exporting Feedback", value = 0)
    updateProgress(value = 0.1, detail = "Analyzing False Negatives")
    # false negatives
    results_FN <- filtered_assessment_set() %>%
      filter(fusion.label != Prediction) %>%
      filter(Prediction=="NS")
    colorized_text_FN <- paste(unlist(
      lapply(results_FN$fusion.text, function(x){paste0("<p>", "&#8227;", colorize_text_accurate_and_slow(x, selected_words()), "</p>")})),
      collapse='')

    updateProgress(value = 0.3, detail = "Analyzing False Positives")
    # false positives
    results_FP <- filtered_assessment_set() %>%
      filter(fusion.label != Prediction) %>%
      filter(Prediction=="SU")
    colorized_text_FP <- paste(unlist(
      lapply(results_FP$fusion.text, function(x){paste0("<p>", "&#8227;", colorize_text_accurate_and_slow(x, selected_words()), "</p>")})),
      collapse='')

    updateProgress(value = 0.5, detail = "Analyzing True Positives")
    # true positives
    results_TP <- filtered_assessment_set() %>%
      filter(fusion.label == Prediction) %>%
      filter(Prediction=="SU")
    colorized_text_TP <- paste(unlist(
      lapply(results_TP$fusion.text, function(x){paste0("<p>", "&#8227;", colorize_text_accurate_and_slow(x, selected_words()), "</p>")})),
      collapse='')

    updateProgress(value = 0.7, detail = "Analyzing True Negatives")
    # true negatives
    results_TN <- filtered_assessment_set() %>%
      filter(fusion.label == Prediction) %>%
      filter(Prediction=="NS")
    colorized_text_TN <- paste(unlist(
      lapply(results_TN$fusion.text, function(x){paste0("<p>", "&#8227;", colorize_text_accurate_and_slow(x, selected_words()), "</p>")})),
      collapse='')

    
    updateProgress(value = 0.9, detail = "Generating File")
    
    code <- paste0("<!DOCTYPE html>
              <html>
                <body>
                  <h1>Iteration ", input$iteration_select_for_inspection, " Feedback - New Insights [", Sys.Date(), "]</h1>",
                   "<h2>New Terms (not in Lexicon)</h2>",
                   htmlTable(export_new_terms_table()),
                   "<h2>Terms to Re-Examine in Lexicon</h2>",
                   htmlTable(export_existing_terms_table()),
                  paste0("<p>&bull; Terms colored in <span style='color:red;'>red</span> increase the model's propensity to classify that text as '", work_env$classification_labels$positive, "', while terms in <span style='color:blue;'>blue</span> increase the model's propensity to classify that text as '", work_env$classification_labels$negative, "'. Terms without a color (i.e., in black font) were not considered by the model. Hovering on the colored words will show the model's propensity of that word.</p>"),
                  "<br>",
                  "<h2>Flse Negatives</h2>",
                  colorized_text_FN,
                  "<h2>False Positives</h2>",
                  colorized_text_FP,
                  "<h2>True Positives</h2>",
                  colorized_text_TP,
                  "<h2>True Negatives</h2>",
                  colorized_text_TN,
                "</body>
              </html>")
    code <- paste(as.character(code), collapse = "\n")

    return (code)
  }
  

  
  output$exportFeedback <- downloadHandler(
    filename = function() {
      #paste("validation_set_", Sys.Date(), ".csv", sep = "")
      paste("new_insights_feedback_", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      #write.csv(filtered_assessment_set() %>% select(-"preprocessed_text_for_classification", -"data_designation", -"fusion.text", -"fusion.label"), file, row.names = FALSE)
      # write.xlsx(full_assessment_set() %>% select(-"preprocessed_text_for_classification", -"data_designation", -"fusion.text", -"fusion.label"), file, row.names = FALSE)
      write.table(feedbackToHTML(), 
                  file, 
                  quote = FALSE,
                  col.names = FALSE,
                  row.names = FALSE
      )
    }
  )
  
  
  
  
  
  output$variableImportance <- renderHighchart({
    if (is.null(input$iteration_select_for_inspection)){
      return(NULL)
    }
    importance_values <- vi(getModel(input$iteration_select_for_inspection)) %>% filter(Importance>0)
    
    plot <- highchart() %>% 
      hc_chart(type = "bar") %>% 
      hc_xAxis(categories = importance_values$Variable) %>% 
      hc_add_series(name = "Model Vectors", data = importance_values$Importance)
  })
  
  
  output$iteration_selector <- renderUI({
    ns <- session$ns
    # if (is.null(list_of_models_in_project())){
    #   return(NULL)
    # }
    selected_item = ""
    choices <- unique(list_of_models_in_project()$iteration_number)
    if (is.null(choices)){
      choices <- c("")
    } else {
      names(choices) <- lapply(choices, function(i){return(paste0("Iteration ", i))})
      selected_item = max(choices)
    }
    return(
      selectInput(ns("iteration_select_for_inspection"), 
                  label = "New Insights", 
                  choices = choices, 
                  selected = selected_item,
                  width = "400px")
    )
  })
  
  
  observeEvent({
    input$iteration_select_for_inspection
  },{
    if(input$iteration_select_for_inspection==""){
      return()
    }
    
    ## Get the assessment set from the database based on the selected model
    all_model_infos <- getAllModelsRunInfos(current_project_id())
    model_info <- all_model_infos %>% filter(iteration_number == input$iteration_select_for_inspection & model_type == 'Supervised - Smart Lexicon')
    full_set <- unserializeObject(model_info$serialized_assessment_set)
    word_scores <- unserializeObject(model_info$serialized_word_scores)
    
    word_scores <- word_scores %>% mutate(model_label = ifelse(score>0, work_env$classification_labels$positive, work_env$classification_labels$negative))
    selected_words(word_scores)
    ## For now, store the full assessment set in the variable. After brush selection, it will be filtered and stored in this variable
    # search_box_filtered_assessment_set(full_set)
    full_assessment_set(full_set)
  })
  
  # observeEvent({
  #   input$search
  # },{
  #   if (input$search==""){
  #     assessment_set <- full_assessment_set()
  #     search_box_filtered_assessment_set(assessment_set)
  #     return(NULL)
  #   }
  #   if (str_length(input$search)<3){
  #     return(NULL)
  #   }
  #   out <- full_assessment_set() %>%
  #     filter(
  #       str_detect(
  #         preprocessed_text_for_classification, paste0("\\b",input$search,"\\b")
  #       )
  #     )
  #   search_box_filtered_assessment_set(out)
  # })
  
  
  # Observer to update the list of existing models in the project
  observeEvent({
    1
    current_project_id()    ## when user switches projects
    reactive_values_from_other_modules$num_of_models_created_during_this_session()    ## when a new model is created by user
    reactive_values_from_other_modules$num_of_iterations_run_during_this_session()    ## when an iteration was run
  },{
    model_infos_from_db <- getAllModelsRunInfos(current_project_id())
    if (is.null(model_infos_from_db)){
      return()
    }
    if(nrow(model_infos_from_db)==0){
      return()
    }
    model_infos_with_names <- model_infos_from_db %>%
      group_by(model_id) %>% 
      mutate(name = paste0(model_id, ". ", model_type, " [", anydate(created_at), "]", 
                           ifelse(is.na(lexicon_id),"",paste0(" / Lexicon ", last(lexicon_id)))
                           ))
    
    list_of_models_in_project(model_infos_with_names)
  },ignoreNULL = T)
  
  
  
  output$termSummaryTable <- DT::renderDataTable({
    ns <- session$ns
    
    ## figure out the lexicon that was used for the model we selected to examine
    if (is.null(input$iteration_select_for_inspection)){
      return(NULL)
    }
    all_model_infos <- getAllModelsRunInfos(current_project_id())
    model_run_info <- all_model_infos %>% filter(iteration_number == input$iteration_select_for_inspection & model_type == 'Supervised - Smart Lexicon')
    
    ## if baseline model, it won't have a lexicon
    if(is.na(last(model_run_info$lexicon_id)) | is.null(selected_words())){
      return(NULL)
    }
    
    if (is.null(full_assessment_set())){
      return(NULL)
    }
    
    ## helper function to count occurrences in True Positive, True Negative, False Positive, and False Negative groups
    count_word_occurrences_per_group <- function(word){
      full_assessment_set() %>%
        mutate(validation.group = ifelse(fusion.label==Prediction,
                                         ifelse(fusion.label=="SU", "TP", "TN"),
                                         ifelse(fusion.label=="SU", "FN", "FP"))) %>% 
        filter(str_detect(preprocessed_text_for_classification, paste0("\\b", word, "\\b"))) %>% 
        group_by(validation.group) %>% 
        tally() %>%
        spread(validation.group, n) %>%
        mutate(word)
    }
    
    ## Get lexicon
    lex <- getLexiconByID(last(model_run_info$lexicon_id))$lexicon %>%
      mutate(word = trimws(tolower(terms))) %>%
      mutate(word = preprocess(word)) %>%
      select(-Category, -`Sub-Category`) %>%
      distinct()
    
    word_score_table <- selected_words() %>%
      mutate_if(is.character, stringr::str_replace_all, pattern = "_", replacement = " ") %>%
      #inner_join(lex, by = "word") %>%
      mutate(terms = factor(word)) %>%
      mutate(terms = fct_reorder(terms, score))
    
    
    output_table <- data.frame(word = character(), TP = integer(), TN = integer(), FP = integer(), FN = integer())
    
    for (i in 1:length(word_score_table$word)){
      output_table <- bind_rows(output_table, count_word_occurrences_per_group(word_score_table$word[i]))
    }
    
    output_table <- output_table %>%
      distinct() %>%  ## this filter is for cases like "kill myself" and "kill yourself", who both are turned to "kill" after stemming preprocessing, thus we need to remove one of them
      inner_join(word_score_table, by = "word") %>%
      replace_na(list(TP = 0, TN = 0, FP = 0, FN = 0)) %>%
      transmute(Terms = terms, 
                `Propensity` = round(score,3),
                `False Negative` = `FN`,
                `False Positive` = `FP`,
                `True Positive` = `TP`, 
                `True Negative` = `TN`)
    
    return(output_table)
    
  },
  server = TRUE,
  # extensions="Responsive",
  callback=JS(
    "table.on('search.dt', function () {",
    "setTimeout(function(){",
    paste0("Shiny.setInputValue('",session$ns('search'),"', table.search() );"),
    "},1000)",
    "} );"
  )
  )
  
  

  output$footnote_for_lexicon_terms_plot <- renderUI({
    ## figure out the lexicon that was used for the model we selected to examine
    if (is.null(input$iteration_select_for_inspection)){
      return(NULL)
    }
    if (input$iteration_select_for_inspection==""){
      return(NULL)
    }
    model_run_info <- getModelRunInfo(input$iteration_select_for_inspection)
    
    ## if baseline model, it won't have a lexicon
    if(is.na(last(model_run_info$lexicon_id)) | is.null(selected_words())){
      return(NULL)
    }
    
    return(
      HTML(paste0("<p>&bull; The values shown in the chart represent the model's propensity to classify as '", work_env$classification_labels$positive, "' or '", work_env$classification_labels$negative, "'. Red color indicates the terms that were labeled as '", work_env$classification_labels$positive, "' in the lexicon, and blue color indicates the terms that were labeled as '", work_env$classification_labels$negative, "' in the lexicon. The plot above allows for selection of terms for inspection.</p>"))
    )
  })
  
  
  
  output$footnote_for_classification_error_analysis <- renderUI({
    ## figure out the lexicon that was used for the model we selected to examine
    if (is.null(input$iteration_select_for_inspection)){
      return(NULL)
    }
    
    return(
      HTML(paste0("<p>&bull; Terms colored in <span style='color:red;'>red</span> increase the model's propensity to classify that text as '", work_env$classification_labels$positive, "', while terms in <span style='color:blue;'>blue</span> increase the model's propensity to classify that text as '", work_env$classification_labels$negative, "'. Terms without a color (i.e., in black font) were not considered by the model. Hovering on the colored words will show the model's propensity of that word.</p>"))
    )
  })
  
  
  
  output$categorization_lexicon_context <- renderD3tree({
    ## figure out the lexicon that was used for the model we selected to examine
    if (is.null(input$iteration_select_for_inspection)){
      return(NULL)
    }
    model_run_info <- getModelRunInfo(input$iteration_select_for_inspection)
    lex <- NULL
    if (!is.na(last(model_run_info$lexicon_id))){
      lex <- getLexiconByID(last(model_run_info$lexicon_id))$lexicon %>%
        mutate(Label = factor(Label), Category = factor(Category), `Sub-Category` = factor(`Sub-Category`), terms = factor(terms), temp = 2) %>% 
        select(Label, Category, terms, temp)
    }
    if(is.null(lex)){
      return(NULL)
    }
    return(
      d3tree(data = list(root = df2tree(struct=as.data.frame(lex),
                                        rootname='Categorization'
                                        #toolTip = lex$terms
      ),
      layout = 'collapse'),             # collapse, radial, cartesian 
      direction = 'h'
      )
    )
  })
  
  
  output$modelStats <- renderUI({
    if (is.null(input$iteration_select_for_inspection)){
      return(NULL)
    }
    if (input$iteration_select_for_inspection==""){
      return(NULL)
    }
    all_model_infos <- getAllModelsRunInfos(current_project_id())
    cur_model <- all_model_infos %>% filter(iteration_number == input$iteration_select_for_inspection & model_type == 'Supervised - Smart Lexicon')
    conf_mat <- unserializeObject(cur_model$serialized_confusion_matrix)
    
    return(
    tagList(
        # column(
        #   width = 2,
        #   tags$div(title="Area Under the ROC Curve (AUC) measures the overall performance of a binary classifier system and demonstrates its diagnostic ability while its discrimination threshold is varied. The AUC value is within the range of 0.5-1.0, where 0.5 represents the performance of a random classifier and 1.0 would correspond to a perfect classifier.",
        #            `data-toggle`="tooltip",
        #            `data-placement`="bottom",
        #            descriptionBlock(
        #              number = round(cur_model$auc, digits = 3),
        #              numberColor = ifelse(cur_model$auc>0.85, "green", ifelse(cur_model$auc>0.65, "yellow", "red")),
        #              # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black. 
        #              header = "AUC", 
        #              rightBorder = FALSE,
        #              marginBottom = FALSE
        #            )
        #   )
        # ),
        # column(
        #   width = 3,
        #   descriptionBlock(
        #     number = round(cur_model$leave_out_auc, digits = 3),
        #     numberColor = ifelse(cur_model$leave_out_auc>0.85, "green", ifelse(cur_model$leave_out_auc>0.65, "yellow", "red")),
        #     # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black. 
        #     header = "Leave-out AUC", 
        #     rightBorder = FALSE,
        #     marginBottom = FALSE
        #   )
        # ),
        column(
          width = 4,
          tags$div(title="Precision: proportion of positive predictions that were true (classification exactness).",
                   `data-toggle`="tooltip",
                   `data-placement`="bottom",
                   descriptionBlock(
                     number = round(conf_mat$byClass['Precision'][[1]], digits = 3),
                     # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black. 
                     header = "Precision", 
                     rightBorder = FALSE,
                     marginBottom = FALSE
                   )
          )
        ),
        column(
          width = 4,
          tags$div(title="Recall: proportion of true-positive cases that were predicted as positive (classification completeness).",
                   `data-toggle`="tooltip",
                   `data-placement`="bottom",
                   descriptionBlock(
                     number = round(conf_mat$byClass['Recall'][[1]], digits = 3),
                     # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black. 
                     header = "Recall", 
                     rightBorder = FALSE,
                     marginBottom = FALSE
                   )
          )
        ),
        column(
          width = 4,
          descriptionBlock(
            number = round(conf_mat$byClass['Balanced Accuracy'][[1]], digits = 3),
            # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black. 
            header = "Accuracy", 
            rightBorder = FALSE,
            marginBottom = FALSE
          )
        )
    )
    )
  })
  
  
  output$topNewTerms <- renderTable({
    ns <- session$ns
    
    ## figure out the lexicon that was used for the model we selected to examine
    if (is.null(input$iteration_select_for_inspection)){
      return(NULL)
    }
    all_model_infos <- getAllModelsRunInfos(current_project_id())
    model_run_info <- all_model_infos %>% filter(iteration_number == input$iteration_select_for_inspection & model_type == 'Supervised - Smart Lexicon')
    
    ## if baseline model, it won't have a lexicon
    if(is.na(last(model_run_info$lexicon_id)) | is.null(selected_words())){
      return(NULL)
    }
    
    if (is.null(full_assessment_set())){
      return(NULL)
    }
    
    ## helper function to count occurrences in True Positive, True Negative, False Positive, and False Negative groups
    count_word_freq_per_group <- function(word){
      output <- full_assessment_set() %>%
        mutate(validation.group = ifelse(fusion.label==Prediction,
                                         ifelse(fusion.label=="SU", "TP", "TN"),
                                         ifelse(fusion.label=="SU", "FN", "FP"))) %>% 
        filter(str_detect(preprocessed_text_for_classification, paste0("\\b", word, "\\b"))) %>% 
        group_by(validation.group) %>% 
        tally() %>%
        spread(validation.group, n) %>%
        mutate(word)
      
      return(output)
    }

    ## Get lexicon
    lex <- getLexiconByID(last(model_run_info$lexicon_id))$lexicon %>%
      mutate(word = trimws(tolower(terms))) %>%
      mutate(word = preprocess(word)) %>%
      select(-Category, -`Sub-Category`) %>%
      distinct()
    
    word_score_table <- selected_words() %>%
      mutate_if(is.character, stringr::str_replace_all, pattern = "_", replacement = " ") %>%
      left_join(lex, by = "word") %>%
      mutate(Remove = ifelse(is.na(Label), "KEEP", "REMOVE")) %>%
      filter(Remove == "KEEP") %>%
      mutate(terms = factor(word)) %>%
      mutate(terms = fct_reorder(terms, score))
    
    
    output_table <- data.frame(word = character(), TP = integer(), TN = integer(), FP = integer(), FN = integer(), F1 = integer())
    
    for (i in 1:length(word_score_table$word)){
      output_table <- bind_rows(output_table, count_word_freq_per_group(word_score_table$word[i]))
    }
    
    output_table <- output_table %>%
      distinct() %>%  ## this filter is for cases like "kill myself" and "kill yourself", who both are turned to "kill" after stemming preprocessing, thus we need to remove one of them
      inner_join(word_score_table, by = "word") %>%
      replace_na(list(TP = 0, TN = 0, FP = 0, FN = 0, F1 = 0)) %>%
      mutate(Recall = TP / (TP + FN)) %>%
      mutate(Precision = TP / (TP + FP)) %>%
      mutate(F1 = 2 * (Precision * Recall)/(Precision+Recall)) %>%
      filter(!is.nan(F1)) %>%
      arrange(desc(F1)) %>%
      transmute(Terms = terms,
                `Model Label` = model_label,
                `Effectiveness (identifying SU)` = round(F1,3),
                #`Recall` = round(Recall,3),
                #`Suggested Label` = ifelse(F1>0.5, model_label, ifelse(model_label=='SU', 'NS', 'SU'))
      )
    export_new_terms_table(output_table)
   return(output_table)
  })
  
  
  
  output$topTermsToExamine <- renderTable({
    ns <- session$ns
    
    ## figure out the lexicon that was used for the model we selected to examine
    if (is.null(input$iteration_select_for_inspection)){
      return(NULL)
    }
    all_model_infos <- getAllModelsRunInfos(current_project_id())
    model_run_info <- all_model_infos %>% filter(iteration_number == input$iteration_select_for_inspection & model_type == 'Supervised - Smart Lexicon')
    
    ## if baseline model, it won't have a lexicon
    if(is.na(last(model_run_info$lexicon_id)) | is.null(selected_words())){
      return(NULL)
    }
    
    if (is.null(full_assessment_set())){
      return(NULL)
    }
    
    ## helper function to count occurrences in True Positive, True Negative, False Positive, and False Negative groups
    count_word_freq_per_group <- function(word){
      output <- full_assessment_set() %>%
        mutate(validation.group = ifelse(fusion.label==Prediction,
                                         ifelse(fusion.label=="SU", "TP", "TN"),
                                         ifelse(fusion.label=="SU", "FN", "FP"))) %>% 
        filter(str_detect(preprocessed_text_for_classification, paste0("\\b", word, "\\b"))) %>% 
        group_by(validation.group) %>% 
        tally() %>%
        spread(validation.group, n) %>%
        mutate(word)
      
      return(output)
    }
    
    ## Get lexicon
    lex <- getLexiconByID(last(model_run_info$lexicon_id))$lexicon %>%
      mutate(word = trimws(tolower(terms))) %>%
      mutate(word = preprocess(word)) %>%
      select(-Category, -`Sub-Category`) %>%
      distinct()
    
    word_score_table <- selected_words() %>%
      mutate_if(is.character, stringr::str_replace_all, pattern = "_", replacement = " ") %>%
      inner_join(lex, by = "word") %>%
      filter(Label==work_env$classification_labels$positive) %>%
      mutate(terms = factor(word)) %>%
      mutate(terms = fct_reorder(terms, score))
    
    
    output_table <- data.frame(word = character(), TP = integer(), TN = integer(), FP = integer(), FN = integer(), F1 = integer())
    
    for (i in 1:length(word_score_table$word)){
      output_table <- bind_rows(output_table, count_word_freq_per_group(word_score_table$word[i]))
    }
    
    output_table <- output_table %>%
      distinct() %>%  ## this filter is for cases like "kill myself" and "kill yourself", who both are turned to "kill" after stemming preprocessing, thus we need to remove one of them
      inner_join(word_score_table, by = "word") %>%
      replace_na(list(TP = 0, TN = 0, FP = 0, FN = 0, F1 = 0)) %>%
      mutate(Recall = TP / (TP + FN)) %>%
      mutate(Precision = TP / (TP + FP)) %>%
      mutate(F1 = 2 * (Precision * Recall)/(Precision+Recall)) %>%
      filter(!is.nan(F1)) %>%
      filter(F1<=0.5) %>%
      arrange(desc(F1)) %>%
      transmute(Terms = terms,
                `Model Label` = model_label,
                `Lexicon Label` = Label,
                `Effectiveness (identifying SU)` = round(F1,3),
                #`Recall` = round(Recall,3),
                #`Suggested Label` = ifelse(F1>0.5, model_label, ifelse(model_label=='SU', 'NS', 'SU'))
      ) %>%
      distinct()
    
    export_existing_terms_table(output_table)
    return(output_table)
  })
}