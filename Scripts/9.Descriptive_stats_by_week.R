setwd()
options(scipen = 999)

library(data.table)
library(tidyverse)
library(igraph)
library(tidygraph)
library(ineq)


#WLUNA ----

WLUNA <- fread("WLUNA.csv")

WLUNA$week <- format(as.Date(WLUNA$block_timestamp, format = "%Y-%m-%d %H:%M:%S"), format= "%W")


#for loop ----

week_number <- unique(WLUNA$week)


for (week_num in week_number) {
  

  filtered_data <- WLUNA %>% filter(week == week_num)
  
  
  edges <- filtered_data[, 2:3]
  vertices <- unique(c(as.character(filtered_data$from_address_id), as.character(filtered_data$to_address_id)))
  
 
  net <- graph_from_data_frame(edges, vertices = vertices, directed = TRUE)
  

  E(net)$weight <- as.numeric(filtered_data$value)
  

  n_nodes <- vcount(net)
  n_edges <- ecount(net)
  indegree <- mean(degree(net, mode = "in"))
  outdegree <- mean(degree(net, mode = "out"))
  weighted_indegree <- mean(strength(net, mode = "in", weights = E(net)$weight))
  weighted_outdegree <- mean(strength(net, mode = "out", weights = E(net)$weight))
  transitivity_global <- transitivity(net, type = "global")
  transitivity_average <- transitivity(net, type = "average")
  transitivity_global_w <- transitivity(net, type = "global", weights = E(net)$weight)
  transitivity_average_w <- transitivity(net, type = "average", weights = E(net)$weight)
  assortativity <- assortativity_degree(net, directed = TRUE)
  assortativity_in <- assortativity_degree(net, (degree(net, mode = "in")))
  assortativity_out <- assortativity_degree(net, (degree(net, mode = "in")))
  assortativity_in_w <- assortativity_degree(net, (strength(net, mode = "in", weights = E(net)$weight)))
  assortativity_out_w <- assortativity_degree(net, (strength(net, mode = "out", weights = E(net)$weight)))
  dyadcensus <- dyad.census(net)
  power_law_in <- power.law.fit(degree(net, mode = "in"), implementation = "plfit")
  power_law_out <- power.law.fit(degree(net, mode = "out"), implementation = "plfit")
  power_law_in_w <- power.law.fit(strength(net, mode = "in", weights = E(net)$weight), implementation = "plfit")
  power_law_out_w <- power.law.fit(degree(net, mode = "out"), implementation = "plfit")
  components <- components(net)
  gini_index <- Gini(components$csize)
  gini_index_weak <- Gini(weak_components$csize)
  gini_index_strong <- Gini(strong_components$csize)
  
  
  strong_components <- components(net, mode = "strong")
  weak_components <- components(net, mode = "weak")
  
  write_rds(strong_components, paste0("str_c_c_", week_num, ".rds"))
  write_rds(weak_components, paste0("weak_c_c_", week_num, ".rds"))
  
  
  #triadcensus <- triad.census(net)
  
  #write_rds(triadcensus, paste0("triad_", week_num, ".rds"))
  
  
  net <- simplify(net)
  knn_in <- knn(net, mode = "in")
  knn_out <- knn(net, mode = "out")
  
  write_rds(knn_in, paste0("knn_in_", week_num, ".rds"))
  write_rds(knn_out, paste0("knn_out_", week_num, ".rds"))
  
  
  
  statistics <- data.frame(
    week = week_num, 
    n_nodes = n_nodes,
    n_edges = n_edges,
    indegree = indegree,
    outdegree = outdegree,
    weighted_indegree = weighted_indegree,
    weighted_outdegree = weighted_outdegree,
    transitivity_global = transitivity_global,
    transitivity_average = transitivity_average,
    transitivity_global_w = transitivity_global_w,
    transitivity_average_w = transitivity_average_w,
    assortativity = assortativity,
    assortativity_in = assortativity_in,
    assortativity_out = assortativity_out,
    assortativity_in_w = assortativity_in_w,
    assortativity_out_w = assortativity_out_w,
    dyad_mut = dyadcensus$mut,
    dyad_asym = dyadcensus$asym,
    dyad_null = dyadcensus$null,
    alpha_in =  power_law_in$alpha,
    alpha_out =  power_law_out$alpha,
    alpha_in_w =  power_law_in_w$alpha,
    alpha_out_w =  power_law_out_w$alpha,
    xmin_in =  power_law_in$xmin,
    xmin_out =  power_law_out$xmin,
    xmin_in_w =  power_law_in_w$xmin,
    xmin_out_w =  power_law_out_w$xmin,
    pvalue_in = power_law_in$KS.p,
    pvalue_out = power_law_out$KS.p,
    pvalue_in_w = power_law_in_w$KS.p,
    pvalue_out_w = power_law_out_w$KS.p,
    gini_index = gini_index,
    gini_index_weak = gini_index_weak,
    gini_index_strong = gini_index_strong
  )
  

  write_csv(statistics, paste0("stats_WLUNA_", week_num, ".csv"))
}















