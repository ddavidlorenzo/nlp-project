# Tweets topic modeling

### David Lorenzo Alfaro

#### ETSIINF, UPM


## Motivation

The increasingly overwhelming amount of available natural language motivates the pressing need to find efficient and reliable computational techniques capable of processing and analysing this type of data to achieve human-like natural language understanding for a wide range of downstream tasks. Over the last decade, Natural Language Processing (NLP) has seen impressively fast growth, primarily favoured by the increase in computational power and the progress on unsupervised learning in linguistics.

Dealing with unannotated data poses as an unsupervised learning problem, a paradigm within Machine Learning in which no outputs or target variables are provided, i.e., the model is only trained with inputs, with the goal being to discover or extract patterns present in data. Often referred to as knowledge discover in the literature, this class of problem is much less defined and since it lacks from ground truth, it is not trivial to find efficient metrics to use for the model to learn about relationships in data and to assess goodness of the identified groups.

In this assignment, we use Latent Dirichlet Allocation (LDA), a generative probabilistic model for collections of discrete data such as text corpora in which each item is modelled as a finite mixture over an underlying set of topics. In this context, we attempt to obtain meaningful representations of documents according to the topic probabilities yielded by a LDA model.


## Code dependencies

Packages `quanteda` and `spacyr` are used for preprocessing purposes, `textmineR` to induce a topic model from data using LDA and to perform queries at inference time; packages `readr` and `rlist` to serialize information allowing to load (store) data from disk to provide with faster execution times by avoiding repeating calculations; and `plotly`, used to generate interactive graphs to visualize topic assignations for words and documents.

The `requirements.R` script checks whether all necessary packages are installed, and installs those that are not automatically from the `CRAN` repository.


## Contents of the directory

```
.
|	README.md
|   Contents of the directory.txt
|   DISCLAIMER.txt
|   requirements.R
|   tweets-topic-modeling.Rmd
|   utils.R
|   
+---data
|   |   news-train-200k-prep.txt
|   |   tweets-test-100k-prep.txt
|   |   tweets-train-200k-prep.txt
|   |   
|   \---serialized
|           doc_predictions_k20.dat
|           tcm_tweets.dat
|           word_predictions_k20.dat
|           
\---documents
        (document) Tweets topic modeling.pdf
        Tweets topic modeling (code).html
        Tweets topic modeling (code).pdf
```
