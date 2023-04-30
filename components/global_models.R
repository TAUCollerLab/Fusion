#' Global functions that are needed for running statistical models

library(qdap)
library(tm)
library(SnowballC)
library(text2vec)
library(lsa)
library(glmnet)
library(caret)
library(AUC)
library(DMwR)
library(e1071)






#' Pre-process function that used to clean up and stem words before analysis
#' 
#' @param text
#' 
#' @return myCorpus object
#' 
preprocess <- function(text){
  myCorpus <- VCorpus(VectorSource(text)) %>%
    #tm_map(removeURLs) %>%
    tm_map(removeNewLineSymbols) %>%
    tm_map(stripWhitespace) %>%
    tm_map(removePunctuation) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(stemDocument)
  return(as.data.frame(myCorpus, stringsAsFactors = FALSE)[,2])
}


removeURLs <- content_transformer(
  function(x) 
    gsub("(f|ht)tp(s?)://(.*)[.][a-z]+(\\S*)","",x)
  )

removeNewLineSymbols <- content_transformer(
  function(x) 
    gsub("\\\\n","",x)
)



#' Extract text and label columns for classification from data file
extractTextForClassification <- function(data){
  df <- data.frame(label = data$fusion.label, 
                   text = preprocess(data$fusion.text),
                   data_designation = data$data_designation,
# not used for now  subcategory = data$Subcategory,
                   stringsAsFactors=FALSE)
  return(df)
}



#' Text to document-term matrix transformation function
text2dtm <- function(text, prune = FALSE){
  it <- itoken(text,
               tokenizer = word_tokenizer)
  vocab <- create_vocabulary(it,  
                             ngram = c(ngram_min = 1L, ngram_max = 3L))
  if(prune){
    vocab <- prune_vocabulary(vocab, term_count_min = 5L)
  }
  vectorizer <- vocab_vectorizer(vocab)
  
  dtm <- create_dtm(it, vectorizer, skip_grams_window = 3)
  return(dtm)
}

#' Document-text matrix to Document-vector matrix
dtm2dvm <- function(dtm, word_vectors, vocab_subset = NULL){
  vocab <- intersect(colnames(dtm), rownames(word_vectors))
  if (!is.null(vocab_subset)) vocab <- intersect(vocab, vocab_subset)
  subdtm <- dtm[, vocab]
  subvec <- word_vectors[vocab,]
  
  dvm <- tcrossprod(subdtm/Matrix::rowSums(subdtm), t(subvec))
  dvm[is.na(dvm)] <- 0
  return(dvm)
}



########################## ProcessEnhanceCorpus.R

corpusWord2Vec <- function(enhancetext, datatext){
  
  it1 <- itoken(enhancetext, 
                tokenizer = word_tokenizer)
  vocab1 <- create_vocabulary(it1)
  vocab1 <- prune_vocabulary(vocab1, term_count_min = 5L)
  
  it2 <- itoken(datatext, 
                tokenizer = word_tokenizer)
  vocab2 <- create_vocabulary(it2,
                              ngram = c(ngram_min = 1L, ngram_max = 3L))
  
  vocab <- create_vocabulary(c(vocab1$term, vocab2$term))
  it <- itoken(c(enhancetext, datatext), 
               tokenizer = word_tokenizer)
  
  vectorizer <- vocab_vectorizer(vocab)
  tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)
  glove <- GlobalVectors$new(rank = 100, 
                             x_max = 10)
  word_transform <- glove$fit_transform(tcm, n_iter = 20)
  word_vectors <- word_transform + t(glove$components)
  return(word_vectors)
}



#######################  Models.R

supervisedModel <- function(train.dvm, train.label){
  train.df <- data.frame(label = train.label, 
                         as.matrix(train.dvm))
  smote.train.df <- SMOTE(label ~ ., train.df, 
                          k = 5, 
                          perc.over = 1000)
  train_control <- trainControl(method = "cv", number = 10, 
                                classProbs =  TRUE, 
                                allowParallel = TRUE)
  
  model <- train(label ~ ., data = smote.train.df, 
                 trControl = train_control, 
                 method = "glmnet", 
                 family = "binomial", 
                 na.action = na.pass)#, 
                 #nlambda = 100)
  return(model)
}

#' Function that runs the unsupervised model and generates classification predictions.
#'
#' @param dtm
#' @param lexicon
#' @param classification_labels
#' @param validation_labels
#' @param normalize_method the method to normalize -- none = do not normalize; 
#' lex.size = the more terms you have in a lexicon, the lower the probability; 
#' SU.frac = normalize by SU.frac.value
#' @param SU.frac.value
#' 
unsupervisedModel <- function(dtm, lexicon, classification_labels, 
                              validation_labels, normalize_method = "lex.size", 
                              SU.frac.value = 0.05, recall = 0.5, concept_indicator = 0){
  SU.dtm <- dtm[,colnames(dtm) %in% lexicon$term[lexicon$label == classification_labels$positive]]
  NS.dtm <- dtm[,colnames(dtm) %in% lexicon$term[lexicon$label == classification_labels$negative]]
  
  ## Normalize_method = none, do not normalize
  SU <- rowSums(SU.dtm) #+ concept_indicator
  NS <- rowSums(NS.dtm) + concept_indicator
  ## Normalize_method = lex.size, the more terms you have in a lexicon, the lower the probability (This is the default method)
  if (normalize_method == "lex.size") {
    SU <- SU/dim(SU.dtm)[2]  
    NS <- NS/dim(NS.dtm)[2]
  }
  ## Normalize_method = SU.frac, normalize by SU.frac.value (default value is 0.05)
  if (normalize_method == "SU.frac") {
    SU <- SU/SU.frac.value  
    NS <- NS/(1-SU.frac.value)
  }
  
  label <- validation_labels #ifelse(SU > NS, classification_labels$positive, classification_labels$negative)
  prob <- SU/(SU+NS)
  prob <- ifelse(is.nan(prob), 0, prob)
  pred <- data.frame(label, prob)
  
  ## Hard recall setting
  n.suspect <- ceiling(length(label[label == classification_labels$positive])*recall)
  SU <- label[order(pred$prob, decreasing = T)]==classification_labels$positive
  index <- order(pred$prob, decreasing = T)[which(cumsum(SU)==n.suspect)[1]]
  cutoff <- pred$prob[index]
  pred$label <- ifelse(pred$prob >= cutoff, classification_labels$positive, classification_labels$negative)
  
  return(pred)
}



supervisedModelAccuracy <- function(model, valid.m, valid.label, classification_labels, recall = 0.5){
  valid.df <- data.frame(label = valid.label, 
                         as.matrix(valid.m))
  ## not setting a hard recall value
  # pred <- predict(model, newdata = valid.df, type = "raw", na.action = na.pass)
  # pred[is.na(pred)] <- classification_labels$negative
  # cf <- confusionMatrix(as.factor(pred), 
  #                       as.factor(valid.df$label), 
  #                       positive = classification_labels$positive)
  
  ## Hard recall setting
  pred <- predict(model, newdata = valid.df, type = "prob", na.action = na.pass)
  pred[is.na(pred)] <- 0
  if(length(unique(pred[,2]))==1) pred[1,2] <- abs(1-pred[1,2])
  #quant <- ceiling(length(valid.df$label[valid.df$label == classification_labels$positive])*recall)
  #cutoff <- sort(pred$SU[valid.df$label == classification_labels$positive], decreasing = T)[quant]
  n.suspect <- ceiling(length(valid.df$label[valid.df$label == classification_labels$positive])*recall)
  SU <- valid.df$label[order(pred$SU, decreasing = T)]==classification_labels$positive
  index <- order(pred$SU, decreasing = T)[which(cumsum(SU)==n.suspect)[1]]
  cutoff <- pred$SU[index]
  
  pred$SU[pred$SU == cutoff] = ifelse(runif(sum(pred$SU == cutoff), 0, 1)>0.5, cutoff + 0.001, cutoff - 0.001)
  pred.bin <- ifelse(pred$SU >= cutoff, classification_labels$positive, classification_labels$negative)
  cf <- confusionMatrix(as.factor(pred.bin), 
                        as.factor(valid.df$label),
                        mode = "everything",
                        positive = classification_labels$positive)

  r <- roc(pred[[classification_labels$positive]], 
           factor(valid.df$label, levels = c(classification_labels$negative, classification_labels$positive)))
  au <- auc(r)
  
  return(
    list(
      auc = au, 
      accuracy = cf$byClass[11], 
      precision = cf$byClass[5], 
      recall = cf$byClass[6], 
      confusion_matrix = cf,
      pred = pred.bin
    )
  )
}



unsupervisedModelAccuracy <- function(pred, label, classification_labels){
  cf <- confusionMatrix(as.factor(pred$label), 
                        as.factor(label),
                        mode = "everything",
                        positive = classification_labels$positive)
  r <- roc(pred$prob, 
           as.factor(label))
  au <- auc(r)
  
  return(
    list(
      auc = au, 
      accuracy = cf$byClass[11], 
      precision = cf$byClass[5], 
      recall = cf$byClass[6],
      confusion_matrix = cf
    )
  )
}


###################### Lexicons.R

getLexicon <- function(lexicons, classification_labels,  smart = F, word_vectors = NULL, correl_threshold = 0.9){
  
  lexicon_SU <- preprocess(lexicons[lexicons$Label == classification_labels$positive,]$terms)
  lexicon_NS <- preprocess(lexicons[lexicons$Label == classification_labels$negative,]$terms)
  
  # lexicon terms
  ## for smart lexicon in the first round it generates a lexicon -> add all ngram types
  if (smart){

    it.lex.SU <- itoken(lexicon_SU, tokenizer = word_tokenizer)
    vocab.lex.SU <- create_vocabulary(it.lex.SU,
                                      ngram = c(ngram_min = 1L, ngram_max = 3L))

    it.lex.NS <- itoken(lexicon_NS, tokenizer = word_tokenizer)
    vocab.lex.NS <- create_vocabulary(it.lex.NS,
                                      ngram = c(ngram_min = 1L, ngram_max = 3L))

    ## for normal lexicons, use only the specific words and phrases used (e.g., don't add ngrams),
  } else {
    lexicon_SU_ <- gsub(pattern = " ", 
                        replacement = "_", 
                        x = lexicon_SU)
    
    lexicon_NS_ <- gsub(pattern = " ", 
                        replacement = "_", 
                        x = lexicon_NS)
    
    lexicon_NS_ <- lexicon_NS_[lexicon_NS_ != ""]
    lexicon_SU_ <- lexicon_SU_[lexicon_SU_ != ""]
    
    it.lex.SU <- itoken(lexicon_SU_, tokenizer = word_tokenizer)
    vocab.lex.SU <- create_vocabulary(it.lex.SU)
    
    
    it.lex.NS <- itoken(lexicon_NS_, tokenizer = word_tokenizer)
    vocab.lex.NS <- create_vocabulary(it.lex.NS)
  }

  
  if (!smart | is.null(word_vectors)){
    SU.terms <- vocab.lex.SU$term
    NS.terms <- vocab.lex.NS$term
  } else {
    SU.terms <- apply(t(word_vectors), 2, 
                      cosine, 
                      t(word_vectors[vocab.lex.SU$term,]))
    SU.terms[SU.terms < correl_threshold] <- 0
    SU.terms <- SU.terms[,colSums(SU.terms)>0]
    SU.terms <- unique(c(rownames(SU.terms), colnames(SU.terms)))
    
    NS.terms <- apply(t(word_vectors), 2, 
                      cosine, 
                      t(word_vectors[vocab.lex.NS$term,]))
    NS.terms[NS.terms < correl_threshold] <- 0
    NS.terms <- NS.terms[,colSums(NS.terms)>0]
    NS.terms <- unique(c(rownames(NS.terms), colnames(NS.terms)))
  }
  
  # new vocab 
  vocab.final <- rbind(data.frame(label = classification_labels$positive, term = SU.terms, stringsAsFactors = FALSE), 
                       data.frame(label = classification_labels$negative, term = NS.terms, stringsAsFactors = FALSE))
  
  return(vocab.final)
}
