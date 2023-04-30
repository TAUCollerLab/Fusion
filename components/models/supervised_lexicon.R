#######################  Lexicon_Supervised.R


#' This is the main function that trains the model on the server-side. It uses the given 
#' input to generate a model. It also sends progress updates back to the UI as it goes. 
#' The function returns a list of objects that provide information about the generated model.
#'
#' @param input standard Shiny param
#' @param output standard Shiny param
#' @param session standard Shiny param
#' @param work_env list of reactive values that define the work environment (data, lexicon)
#' @param updateProgress a function that sends progress updates to the UI for 
#'                       the progress bar. Values should be between 0 and 1.
#' @param concept_dataframe a dataframe of out-of-lexicon concept vectors (e.g., Bidul)
#'
#' @return a list of: model, AUC, leave-out-AUC, confusion matrix, explainer, 
#'         and dataframe of results for the explainer
#' 
supervised_lexicon_model <- function(input, output, session, work_env, updateProgress = NULL, 
                                     use_external_corpus = FALSE, use_concepts = FALSE){
  
  
  enhancement_corpus <- enhanced_wikipedia_corpus
  if (use_external_corpus){
    enhancement_corpus <- work_env$corpus()
  }
  
  set.seed(GlobalModelSeed)
  
  
  
  updateProgress(value = 0.1, detail = "preprocessing data")
  df <- extractTextForClassification(work_env$data())
  updateProgress(value = 0.2, detail = "creating data term matrix")
  dtm <- text2dtm(df$text)
  
  ## initial.lexicon
  updateProgress(value = 0.3, detail = "initial lexicon")
  lex <- getLexicon(work_env$lexicon(),
                    work_env$classification_labels,
                    smart = F)
  
  ## read enhancement corpus
  #enhance <- readLines("www/wikipedia.dat", n = 1, warn = FALSE)
  #enhance <- preprocess(enhance)
  
  
  ## create word vectors
  updateProgress(value = 0.4, detail = "creating word vectors")
  word_vectors <- corpusWord2Vec(enhancement_corpus, c(lex$term, df$text[df$data_designation == "training"]))
  
  ## if smart lexicon
  #lex <- getLexicon(work_env$lexicon(), 
  #                  smart = T, word_vectors, correl_threshold = 0.8)
  
  
  ## run supervised model
  updateProgress(value = 0.8, detail = "training model")
  dvm <- dtm2dvm(dtm, word_vectors, lex$term)
  
  ## adding columns for each concept (each added column should represent an out-of-lexicon concept)
  if (use_concepts){
    dvm <- cbind(dvm, work_env$data()$concept)
  }
  
  model <- tryCatch({
    ## this is in the case the model doesn't converge and returns an error
    supervisedModel(dvm[df$data_designation == "training",], df[df$data_designation == "training",]$label)
  },
  error = function(e) {
    print(e)
    return(NULL)
  }
  )

  if (is.null(model)){
    return(
      data.frame(
        "Model" = "Supervised Lexicon",
        "auc" = NaN,
        "leave_out_auc" = NaN,
        "precision" = NaN,
        "recall" = NaN
      )
    )
  }
  acc <- supervisedModelAccuracy(model, dvm[df$data_designation == "validation",], df[df$data_designation == "validation",]$label, work_env$classification_labels, work_env$hard_recall_value())
  leave_out <- supervisedModelAccuracy(model, dvm[df$data_designation == "leave-out validation",], df[df$data_designation == "leave-out validation",]$label, work_env$classification_labels, work_env$hard_recall_value())
  
  ## run the generated model to predict word propensity scores
  updateProgress(value = 0.9, detail = "word propensity score")
  
  word.vectors.df <- data.frame(word_vectors[rownames(word_vectors) %in% lex$term,])
  #word.vectors.df = cbind(word.vectors.df, concept = NA)
  #word.vectors.df <- data.frame(cbind(word_vectors[rownames(word_vectors) %in% lex$term,], NA))
  # add an additional column(s) that represents the concept being used, we only need it to make the matrix at the right size
  if (use_concepts){
    #word.vectors.df <- data.frame(cbind(word_vectors[rownames(word_vectors) %in% lex$term,], NA))
    word.vectors.df = cbind(word.vectors.df, `X101` = 0)
  }
  predict_word_probabilities <- predict(model, newdata = word.vectors.df, type = "prob", na.action = na.pass)
  word.prob <- cbind(term = row.names(word.vectors.df), predict_word_probabilities)
  word_scores <- data.frame(word.prob) %>%
    mutate(NS = -1 * NS) %>%
    mutate(word = as.character(term), score = ifelse(abs(SU)>abs(NS), SU, NS)) %>%
    select(word, score)
  
  updateProgress(value = 1, detail = "finializing")
  return(
    list(
      model = model,
      auc = acc["auc"][[1]],
      leave_out_auc = leave_out["auc"][[1]],
      confusion_matrix = acc$confusion_matrix,
      assessment_set = work_env$data() %>%
        mutate(preprocessed_text_for_classification = df$text) %>%
        filter(data_designation == "validation") %>%
        mutate(Prediction = acc$pred),
      word_scores = word_scores
    )
  )
}