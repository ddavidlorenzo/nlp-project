library(reticulate)

reticulate::virtualenv_create("r-nlp", python = "C:\\Users\\David\\anaconda3_8\\python")

use_virtualenv("r-nlp")

import("numpy")

library(keras)
library(tensorflow)
library(dplyr)
library(devtools)
library(tfdatasets)

#use_condaenv("C:\\Users\\David\\ANACON~1\\envs\\SPACY_~1\\python.exe")

install_keras()

install_tensorflow()

transformer = reticulate::import('transformers')

devtools::install_github("rstudio/tensorflow") 
devtools::install_github("rstudio/keras")


Sys.which("python")
 
# "C:\\Users\\David\\ANACON~1\\envs\\SPACY_~1\\python.exe"
