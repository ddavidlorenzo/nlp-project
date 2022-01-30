library(textmineR)
library(quanteda)
library(readr)
library("spacyr")
#library(udpipe)

spacy_initialize(model = "en_core_web_sm")

# x <- udpipe_download_model(language = "english")
# x$file_model
# ud_english <- udpipe_load_model(x$file_model)
# 
# u <- udpipe_annotate(ud_english, df.processed[[15]])$x
# u


set.seed(0)

sample_size <- 200000

df.tweets <- sample(read.csv('tweets.csv', header=FALSE)$V6, sample_size)

#df.news <- sample(read.csv('abcnews.csv')$headline_text, sample_size)

#df.abstracts <- sample(read.csv('abstracts.csv')$abstract, sample_size)

sample(df.tweets,10)

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



df.tokens <- tokens(df.corpus, 
                    remove_punct = TRUE, 
                    remove_url = TRUE, 
                    remove_symbols = TRUE
) %>% tokens_replace(
  pattern = "\n", replacement=" "
) %>% tokens_tolower(
) %>% tokens_select(
  pattern = stopwords("en"), selection = "remove"
)


join <- function(x) { Reduce(paste, spacy_parse(x, pos = FALSE, tag = FALSE, entity = FALSE)$lemma)}

reduce <- function(x) { Reduce(paste, x)}

df.spacy <- lapply(df.tokens, reduce) %>% unlist(use.names = F) %>% spacy_parse(pos = F, tag = F, entity = F)

spacy_finalize()

df.processed <- aggregate(df.spacy$lemma, list(df.spacy$doc_id), FUN=reduce)$x

head(df.processed, 3)
#write_lines(df.processed, "tweets-200k-prep.txt")


# Much slower!
#df.processed <- unlist(lapply(df.tokens[1:100], join), use.names = F)


tcm <- CreateTcm(doc_vec = df.processed,
                 #skipgram_window = 3,
                 verbose = FALSE,
                 cpus = 4)

embeddings <- FitLdaModel(dtm = tcm,
                          k = 20,
                          iterations = 100,
                          burnin = 80,
                          alpha = 0.1,
                          beta = 0.05,
                          optimize_alpha = TRUE,
                          calc_likelihood = FALSE,
                          calc_coherence = TRUE,
                          calc_r2 = TRUE,
                          cpus = 4)

# Get an R-squared for general goodness of fit
embeddings$r2

#write_lines(df.processed, "tweets-200k-bad-prep.txt")

# sink("tweets-150k-bad-prep.txt")
# 
# print(df.processed)
# cat(df.processed[1:10])
# cat(df.tokens[[1]])
# sink()

# Get coherence (relative to the TCM) for goodness of fit
summary(embeddings$coherence)

# Get top terms, no labels because we don't have bigrams
embeddings$top_terms <- GetTopTerms(phi = embeddings$phi,
                                    M = 5)


embeddings$summary <- data.frame(topic = rownames(embeddings$phi),
                                 coherence = round(embeddings$coherence, 3),
                                 prevalence = round(colSums(embeddings$theta), 2),
                                 top_terms = apply(embeddings$top_terms, 2, function(x){
                                   paste(x, collapse = ", ")
                                 }),
                                 stringsAsFactors = FALSE)

embeddings$summary[ order(embeddings$summary$prevalence, decreasing = TRUE) , ]


embeddings$summary[ order(embeddings$summary$coherence, decreasing = TRUE) , ][ 1:20 , ]


min(embeddings$beta)


