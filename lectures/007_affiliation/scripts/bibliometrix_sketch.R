library(bibliometrix)

sh.articles <- list.files(path="data/articles/", full.names = TRUE)

sh.df <- convert2df(file=sh.articles, dbsource="isi", format="plaintext")

sh.biblio <- biblioAnalysis(sh.df, sep=";")

summary(sh.biblio, k=10)

plot(x=sh.biblio, k=5, pause=FALSE)

coauth <- biblioNetwork(sh.df, analysis="collaboration", network="authors", sep=";", 
                        short=TRUE)

networkPlot(coauth, remove.isolates=TRUE, label.n=10)