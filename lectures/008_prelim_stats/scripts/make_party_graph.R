party.df <- data.frame(id=seq(1:16), party=party)
party.g <- graph.data.frame(party.df)
V(party.g)$type <- bipartite_mapping(party.g)$type
proj <- bipartite.projection(party.g)
floparty <- proj$proj1

save(floparty, flobusiness, flomarriage, file="data/flonets.Rdata")
