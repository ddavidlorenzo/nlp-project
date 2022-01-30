options(warn=-1)

library(textmineR)
library(quanteda)
library(readr)
library(rlist)
library("spacyr")

packages <- c('textmineR', 
              'quanteda', 
              'readr',
              'rlist',
              'plotly')

install.packages(setdiff(packages, rownames(installed.packages())), repos = 'https://cran.rediris.es/')