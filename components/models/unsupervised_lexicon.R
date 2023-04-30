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
#'
#' @return a list of: model, AUC, leave-out-AUC, confusion matrix, explainer, 
#'         and dataframe of results for the explainer
#' 
unsupervised_lexicon_model <- function(input, output, session, work_env, updateProgress = NULL, use_concepts = FALSE){
  
  set.seed(GlobalModelSeed)
  
  updateProgress(value = 0.1/2, detail = "preprocessing data")
  df <- extractTextForClassification(work_env$data())
  updateProgress(value = 0.2/2, detail = "creating data term matrix")
  dtm <- text2dtm(df$text)
  
  ## initial.lexicon 
  updateProgress(value = 0.3/2, detail = "initial lexicon")
  lex <- getLexicon(work_env$lexicon(),
                    work_env$classification_labels,
                    smart = F)
  
  ## if smart lexicon
  #enhance <- readLines("C:\\Users\\user\\Dropbox\\Useful Codes\\Text analysis\\wikipedia.dat", n = 1, warn = FALSE)
  #enhance <-   as.data.frame(preprocess(enhance))[,2]
#  word_vectors <- corpusWord2Vec(enhanced_wikipedia_corpus, c(lex$term, df$text[df$data_designation == "training"]))
#  lex <- getLexicon(work_env$lexicon(), 
#                    smart = T, word_vectors, correl_threshold = 0.8)
  
  concept_indicator = 0
  if (use_concepts){
    df_filtered <- work_env$data() %>% filter(data_designation == "validation")
    concept_indicator = df_filtered$concept
  }
  

  ## run unsupervised model
  updateProgress(value = 0.8/2, detail = "making predictions")
  pred <- unsupervisedModel(
    dtm = dtm[df$data_designation == "validation",], 
    lexicon = lex, 
    classification_labels = work_env$classification_labels, 
    validation_labels = df$label[df$data_designation == "validation"],
    normalize_method = "lex.size", 
    SU.frac.value = 0.05, 
    recall = work_env$hard_recall_value(), 
    concept_indicator = concept_indicator
  )
  acc <- unsupervisedModelAccuracy(pred, df[df$data_designation == "validation",]$label, work_env$classification_labels)
  

  
  if (use_concepts){
    df_filtered <- work_env$data() %>% filter(data_designation == "leave-out validation")
    concept_indicator = df_filtered$concept
  }
  leave_out_pred <- unsupervisedModel(
    dtm[df$data_designation == "leave-out validation",],
    lexicon = lex,
    classification_labels = work_env$classification_labels,
    df$label[df$data_designation == "leave-out validation"],
    normalize_method = "lex.size",
    SU.frac.value = 0.05,
    recall = work_env$hard_recall_value(),
    concept_indicator = concept_indicator
  )
  leave_out <- unsupervisedModelAccuracy(leave_out_pred, df[df$data_designation == "leave-out validation",]$label, work_env$classification_labels)

  updateProgress(value = 1/2, detail = "finializing")
  return(
    list(
      model = NULL,
      auc = acc["auc"][[1]],
      leave_out_auc = leave_out["auc"][[1]],
      confusion_matrix = acc$confusion_matrix,
      assessment_set = work_env$data() %>%
        mutate(preprocessed_text_for_classification = df$text) %>%
        filter(data_designation == "validation") %>%
        mutate(Prediction = pred$label),
      word_scores = data.frame(word = lex$term, score = ifelse(lex$label==work_env$classification_labels$positive, 1, -1))
    )
  )
}

