#install.packages("devtools")
library(devtools)
#install_github("cbail/textnets")
library(textnets)
library(dplyr)

comp <- read.csv("comp_socsci.csv")


comp <- comp %>% filter(AB != "")

comp$text <- paste(comp$DE, comp$TI, comp$AB)

comp_nouns <- PrepTextNounPhrases(comp, textvar="text", groupvar="TI", node_type="groups", top_phrases=TRUE)

dictionary <- data.frame(word=comp_nouns$word, count=comp_nouns$count)

word_count <- dictionary %>% group_by(word) %>% summarise(freq = sum(count))

comp_nouns$length <- nchar(comp_nouns$word)

comp_nouns <- comp_nouns %>% filter(length>3)

comp_text_network <- CreateTextnet(comp_nouns, node_type="groups")

comp_communities <- TextCommunities(comp_text_network)

top_words_modularity_classes <- InterpretText(comp_text_network, comp_nouns)

library(ggplot2)

VisualizeText(comp_text_network, .50, label_degree_cut=100)

library(htmlwidgets)
vis <- VisualizeTextD3js(comp_text_network, 
                         prune_cut=.96,
                         height=1000,
                         width=1400,
                         bound=FALSE,
                         zoom=TRUE,
                         charge=-30)
saveWidget(vis, "comp_textnet.html")

