#This script organizes the week 7 homework data into a data frame using bilbiometrix

library(bibliometrix)
library(dplyr)

cul.articles <- list.files(path="data/articles", full.names = TRUE)

culture.df <- convert2df(file=cul.articles, dbsource="isi", format="plaintext")

culture.df$text <- paste(culture.df$DE, culture.df$TI, culture.df$AB)

culture.df <- culture.df %>% filter(PY<2023)

culture.df$text <- tolower(culture.df$text)

saveRDS(culture.df, file="data/culture_corpus.RDS")

