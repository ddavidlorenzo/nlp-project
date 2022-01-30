library(textmineR)
library(quanteda)
library(rlist)
library("spacyr")
library(plotly)

reduce <- function(x) { Reduce(paste, x)}

tokenize <- function(corpus, kind="twitter"){
  if(kind=="twitter") {
    tokens(corpus, 
           remove_punct = TRUE, 
           remove_url = TRUE, 
           remove_symbols = TRUE,
           remove_numbers = TRUE
           ) %>% tokens_tolower(
           ) %>% tokens_select(
             pattern = stopwords("en"), selection = "remove"
           ) %>% tokens_select(
             pattern = "@\\S+", selection="remove", valuetype ="regex"
           ) %>% tokens_select(
             pattern = "www\\.\\S+", selection="remove", valuetype ="regex"
           ) %>% tokens_select(
             pattern = "amp", selection="remove"
           ) %>% tokens_select(
             pattern = "quot", selection="remove"
           )
  } else {
    tokens(corpus, 
           remove_punct = TRUE, 
           remove_url = TRUE, 
           remove_symbols = TRUE
           ) %>% tokens_tolower(
           ) %>% tokens_select(
            pattern = stopwords("en"), selection = "remove"
    )
  }
  
}

preprocess <- function(df, kind="twitter"){
  
  df.corpus <- iconv(corpus(df), from = "UTF-8", to = "ASCII", sub = "")
  df.tokens <- tokenize(df.corpus, kind=kind)
  
  spacy_initialize(model = "en_core_web_sm")
  
  df.spacy <- lapply(df.tokens, reduce) %>% unlist(use.names = F) %>% spacy_parse(pos = F, tag = F, entity = F)
  
  head(lapply(df.tokens, reduce), 3)
  
  if (is.null(options()$spacy_initialized)) 
        spacy_initialize()

  df.processed <- aggregate(df.spacy$lemma, list(df.spacy$doc_id), FUN=reduce)$x
  
  spacy_finalize()
  
  df.processed
} 
  
ldaSummary <- function(embeddings){
  data.frame(topic = rownames(embeddings$phi),
             prevalence = round(colSums(embeddings$theta), 3),
             coherence = round(embeddings$coherence, 3),
             top_terms = apply(embeddings$top_terms, 2, paste, collapse=", "),
             stringsAsFactors = FALSE)
  
}

printSummaryTable <- function(order, decreasing=TRUE){
  if(order!="prevalence" && order!="coherence") 
    stop("Illegal value for order. Supported values are 'prevalence' and 'coherence")
  
  if (order=="prevalence")
    embeddings$summary[ order(embeddings$summary$prevalence, decreasing = decreasing),]
  else
    embeddings$summary[ order(embeddings$summary$coherence, decreasing = decreasing),]
  
}
  
getDtm <- function(docs, names=NULL, ngram_window= c(1,1), cpus=4, verbose=FALSE, ...){
  if(is.null(names)) names<-docs
  dtm_embed <- CreateDtm(doc_vec = docs,
                         doc_names = names,
                         ngram_window = ngram_window,
                         verbose = verbose,
                         cpus = cpus,
                         ...
  )
  # Filter out tokens that appears <= 2 times (it means those words are "isolated")
  dtm_embed[, colSums(dtm_embed) > 2]
}


predict_word <- function(embeddings, tcm, out=NULL, method = "gibbs",
                         iterations = 200, burnin = 175, ...){
  
  if(!is.null(out) && file.exists(out))
    predictions <- list.unserialize(out)
  else{
    predictions <- predict(embeddings, tcm, method = method,
                           iterations = iterations, burnin = burnin, ...)
    
    if(!is.null(out)) list.serialize(predictions, out)
  }
  
  predictions
}


predict_doc <- function(embeddings, dtm = NULL, out=NULL, docs = NULL, 
                           method = "gibbs", iterations = 200, 
                           burnin = 175, ...){
  # Check for missing document term matrix to estimate it in the context of
  # the function from a collection of documents.
  if(is.null(dtm)){
    if(is.null(docs)) stop("Illegal parametrization: either the document term matrix or a collection of documents must be provided")
    # Get the document term matrix from the documents.
    dtm <- getDtm(docs)
  }
  
  if(!is.null(out) && file.exists(out))
    predictions <- list.unserialize(out)
  else{
    # Compute the predictions
    predictions <- predict(embeddings, dtm, method = method,
                           iterations = iterations, burnin = burnin, ...)
    
    if(!is.null(out)) list.serialize(predictions, out)
  }
  
  predictions
}


trimLabel <- function(label, nchars=50) ifelse(nchar(label)>nchars, paste0(substring(label,1,nchars-3), "..."), label)


getHoverTemplate <- function(label){
  paste(
    "\"", label,"\"<br>",
    "<b>likelihood</b>: %{y}<br>",
    "<b>topic</b>: %{x}<br>",
    "<extra></extra>"
  )
}


plot_words2topic <- function(words, predict_matrix) {
  vocab <- rownames(predict_matrix)
  indices <- (lapply(words, function(word){which(vocab==word)}) %>% unlist(use.names=F))
  
  if(length(indices)==1) return(plot_ly(x=colnames(predict_matrix),
                                        y=predict_matrix[indices[1],],
                                        type="bar")
  )
  
  if(length(indices) > 1){
    data <-predict_matrix[indices,]
    label <- rownames(data)
    
    fig <- plot_ly(x=factor(colnames(data),levels=colnames(data)), 
                   y=data[1,],
                   name=label[1],
                   type="bar")
    data <- data[-1,]
    label<- label[-1]
    
    if (length(label)==1) return(fig %>% add_trace(y=data, name = label[1]))
    
    for (i in (1:length(label))){
      fig <- fig %>% add_trace(y=data[i,], name = label[i])
    }
    fig
    
  } else 
    stop("Ilegal arguments to the function")
}


plot_doc2topic <- function(docs, predictions=NULL, embeddings=NULL, ...) {
  
  # If some necessary parameters are missing, we attempt to estimate them from
  # others to allow for greater flexibility of the code.
  if(is.null(predictions)) {
    if(is.null(embeddings)) stop("Illegal arguments to the function: either the embeddings or the predictions must be provided")
    
    # Compute missing predictions based on the embeddings yielded on a
    # training set.
    predictions <- getPredictions(embeddings, docs=docs, ...)
  }
  
  vocab <- rownames(predictions)
  indices <- (lapply(docs, function(doc){which(vocab==doc)}) %>% unlist(use.names=F))
  
  if(length(indices)==1) return(plot_ly(x=colnames(predictions),
                                        y=predictions[indices[1],],
                                        type="bar")
  )
  
  if(length(indices) > 1){
    data <-predictions[indices,]
    label <- rownames(data)
    
    fig <- plot_ly(x=factor(colnames(data),levels=colnames(data)), 
                   y=data[1,],
                   name=trimLabel(label[1]),
                   hovertemplate = getHoverTemplate(label[1]),
                   type="bar")
    data <- data[-1,]
    label<- label[-1]
    
    if (length(label)==1) return(fig %>% add_trace(y=data, name = label[1]))
    
    for (i in (1:length(label))){
      fig <- fig %>% add_trace(y=data[i,], name = trimLabel(label[i]), hovertemplate = getHoverTemplate(label[i]))
    }
    fig
    
  } else 
    stop("Ilegal arguments to the function")
}

topic_search <- function(data, word_search, predictions, 
                         sample_size=4, seed=NULL, verbose=F,...){
  
  docs <- data[lapply(data,function(x){grepl(word_search, x, fixed=T)}) %>% unlist(use.names=F)]
  
  if(!length(docs)) stop(paste0("No documents matched for word '", word_search, "'."))
  
  if (seed) set.seed(seed)
  
  docs <- if (length(docs)>sample_size) sample(docs, sample_size) else docs
  
  if(verbose) print(docs)
  
  plot_doc2topic(docs, predictions=predictions)
  
}

