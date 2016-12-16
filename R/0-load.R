library(dplyr)
library(data.table)
library(dtplyr)
library(magrittr)
library(stringr)
library(tidyr)
library(stringi)
library(bnlearn)
library(lubridate)
library(ggplot2)

sapply(dir('./R/0-funcoes/', full.names = T), source)

source("./R/0-tabelas_cnj.R")