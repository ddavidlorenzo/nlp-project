library(textmineR)
library(quanteda)
library(readr)
library("spacyr")

set.seed(0)

sample_size <- 1000

df.tweets <- sample(read.csv('tweets.csv', header=FALSE)$V6, sample_size)

df.corpus <- iconv(corpus(df.tweets), from = "UTF-8", to = "ASCII", sub = "")

# TWITTER
df.tokens <- tokens(df.corpus, 
                    remove_numbers = TRUE, 
                    remove_punct = TRUE, 
                    remove_url = TRUE, 
                    remove_symbols = TRUE
                    ) %>% tokens_select(
                      pattern = "@\\S+", selection="remove", valuetype ="regex"
                    ) %>% tokens_select(
                      pattern = "amp", selection="remove"
                    ) %>% tokens_select(
                      pattern = "quot", selection="remove"
                    ) %>% tokens_select(
                      pattern = stopwords("en"), selection = "remove"
                    ) %>% tokens_tolower()


reduce <- function(x) { Reduce(paste, x)}

spacy_initialize(model = "en_core_web_sm")

df.spacy <- lapply(df.tokens, reduce) %>% unlist(use.names = F) %>% spacy_parse(pos = F, tag = F, entity = F)

head(lapply(df.tokens, reduce), 3)

spacy_finalize()

df.processed <- aggregate(df.spacy$lemma, list(df.spacy$doc_id), FUN=reduce)$x

head(df.processed, 3)


tcm <- CreateTcm(doc_vec = df.processed,
                 #skipgram_window = 3,
                 verbose = FALSE,
                 cpus = 4)

embeddings <- FitLdaModel(dtm = tcm,
                          k = 20,
                          iterations = 200,
                          burnin = 175,
                          alpha = 0.1,
                          beta = 0.05,
                          optimize_alpha = TRUE,
                          calc_coherence = TRUE,
                          calc_r2 = TRUE,
                          cpus = 4)

# Get an R-squared for general goodness of fit
embeddings$r2

rownames(embeddings$phi)
embeddings$phi[1:10]
# Get coherence (relative to the TCM) for goodness of fit
summary(embeddings$coherence)


length(df.tokens)

length(rownames(embeddings$theta))

head(embeddings$gamma, 2)
# Get top terms, no labels because we don't have bigrams
embeddings$top_terms <- GetTopTerms(phi = embeddings$phi,
                                    M = 5)

embeddings$top_terms

embeddings$summary <- data.frame(topic = rownames(embeddings$phi),
                                 prevalence = round(colSums(embeddings$theta), 3),
                                 coherence = round(embeddings$coherence, 3),
                                 top_terms = apply(embeddings$top_terms, 2, paste, collapse=", "),
                                 stringsAsFactors = FALSE)

embeddings$summary[ order(embeddings$summary$prevalence, decreasing = TRUE) , ]


embeddings$summary[ order(embeddings$summary$coherence, decreasing = TRUE) , ]


# predict on held-out documents using gibbs sampling "fold in"
p1 <- predict(embeddings, tcm, method = "gibbs", iterations = 200, burnin = 175)

p1[1,]

library(plotly)



plot_words2topic(c('aaron','asfsf','diooos','bye', "market", "hairdresser", "fun"), p1)


head(as.data.frame(embedding_assignments), 2)

plot_doc2topic(df.processed[1:3], embeddings=embeddings, predictions=p)

doc_queries <- c(
  "I love music so much and partying all night. Good night!",
  "Police attempted to murder my cousin yesterday. It is so unfair!",
  "Got COVID 19 tested. My head hurts so bad!"
)


tweet.test <- df.processed[1:100]
# Make a DTM from our documents
head(tweet.test)

dtm.embeddings <- getDtm(tweet.test)

# Project the documents into the embedding space
dtm.predictions <- getPredictions(embeddings, dtm.embeddings)

word_search<-"mom"

indices <- which(lapply(tweet.test, function(x){grepl(word_search, x, fixed=T)}
                        ) %>% unlist(use.names=F) == T)

x<- c(df.processed[indices])

x

