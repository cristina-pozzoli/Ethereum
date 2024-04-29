setwd()
options(scipen = 999)

library(data.table)
library(tidyverse)
library(igraph)
library(tidygraph)
library(ineq)
library(ggplot2)



#WLUNA ----

WLUNA_stats <- fread("statistics_combined_WLUNA.csv")


### Nodes and edges ----

ggplot(WLUNA_stats, aes(x = week, y = n_nodes)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Nodes by week",
       x = "Week",
       y = "Nodes") +
  theme_minimal()

ggplot(WLUNA_stats, aes(x = week, y = n_edges)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Edges by week",
       x = "Week",
       y = "Edges") +
  theme_minimal()


### Indegree -----

ggplot(WLUNA_stats, aes(x = week, y = indegree)) +
  geom_line() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Indegree by week",
       x = "Week",
       y = "Indegree") +
  theme_minimal()

# weighted indegree 

WLUNA_stats$log_weighted_indegree <- log10(as.numeric(WLUNA_stats$weighted_indegree))

ggplot(WLUNA_stats, aes(x = week, y = log_weighted_indegree)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Weighted indegree by week",
       x = "Week",
       y = "Weighted indegree") +
  theme_minimal()


### Outdegree ----

ggplot(WLUNA_stats, aes(x = week, y = outdegree)) +
  geom_line() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Outdegree by week",
       x = "Week",
       y = "Outdegree") +
  theme_minimal()

#weighted outdegree 

WLUNA_stats$log_weighted_outdegree <- log10(as.numeric(WLUNA_stats$weighted_outdegree))

ggplot(WLUNA_stats, aes(x = week, y = log_weighted_outdegree)) +
  geom_line() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Weighted outdegree by week",
       x = "Week",
       y = "Weighted outdegree") +
  theme_minimal()



### Clustering coefficients ----

#Global transitivity 

ggplot(WLUNA_stats, aes(x = week, y = transitivity_global)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Global transitivity by week",
       x = "Week",
       y = "Global transitivity") +
  theme_minimal()


#Weighted 

ggplot(WLUNA_stats, aes(x = week, y = transitivity_global_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Global weighted transitivity by week",
       x = "Week",
       y = "Global weighted transitivity") +
  theme_minimal()


#Average transitivity 

ggplot(WLUNA_stats, aes(x = week, y = transitivity_average)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Average transitivity by week",
       x = "Week",
       y = "Average transitivity") +
  theme_minimal()


#Weighted

ggplot(WLUNA_stats, aes(x = week, y = transitivity_average_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Average weighted transitivity by week",
       x = "Week",
       y = "Average weighted transitivity") +
  theme_minimal()


#Combined plot

WLUNA_long <- WLUNA_stats %>%
  pivot_longer(cols = c(transitivity_global, transitivity_average, transitivity_global_w, transitivity_average_w), 
               names_to = "transitivity_type", 
               values_to = "transitivity_value")

# Plot
ggplot(WLUNA_long, aes(x = week, y = transitivity_value, color = transitivity_type)) +
  geom_line() +
  facet_wrap(~ transitivity_type, scales = "free_y", ncol = 2) +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Clustering coefficient by week",
       x = "Week",
       y = "Values") +
  theme_minimal()



### Assortativity ----

ggplot(WLUNA_stats, aes(x = week, y = assortativity)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Assortativity by week",
       x = "Week",
       y = "Assortativity") +
  theme_minimal()


# Indegree assortativty 

ggplot(WLUNA_stats, aes(x = week, y = assortativity_in)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Indegree assortativity by week",
       x = "Week",
       y = "Indegree assortativity") +
  theme_minimal()

#Weighted

ggplot(WLUNA_stats, aes(x = week, y = assortativity_in_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Weighted indegree assortativity by week",
       x = "Week",
       y = "Weighted indegree assortativity") +
  theme_minimal()


# Outdegree assortativty 

ggplot(WLUNA_stats, aes(x = week, y = assortativity_out)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Outdegree assortativity by week",
       x = "Week",
       y = "Outdegree assortativity") +
  theme_minimal()

#Weighted

ggplot(WLUNA_stats, aes(x = week, y = assortativity_out_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Weighted outdegree assortativity by week",
       x = "Week",
       y = "Weighted outdegree assortativity") +
  theme_minimal()


#Combined plot 

WLUNA_long2 <- WLUNA_stats %>%
  pivot_longer(cols = c(assortativity, assortativity_in, assortativity_out, assortativity_in_w, assortativity_out_w), 
               names_to = "assortativity_type", 
               values_to = "assortativity_value")

# Plot
ggplot(WLUNA_long2, aes(x = week, y = assortativity_value, color = assortativity_type)) +
  geom_line() +
  facet_wrap(~ assortativity_type, scales = "free_y", ncol = 2) +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Assortativity by week",
       x = "Week",
       y = "Values") +
  theme_minimal()


### Dyad census ----

#Mutual

ggplot(WLUNA_stats, aes(x = week, y = dyad_mut)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Mutual vertices by week",
       x = "Week",
       y = "Number of mutual vertices") +
  theme_minimal()

#Asymmetric 

ggplot(WLUNA_stats, aes(x = week, y = dyad_asym)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Asymmetric vertices by week",
       x = "Week",
       y = "Number of asymmetric vertices") +
  theme_minimal()

#Null 

ggplot(WLUNA_stats, aes(x = week, y = dyad_null)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Null vertices by week",
       x = "Week",
       y = "Number of null vertices") +
  theme_minimal()


#Combined plot

WLUNA_long3 <- WLUNA_stats %>%
  pivot_longer(cols = c(dyad_mut, dyad_asym, dyad_null), 
               names_to = "dyad_type", 
               values_to = "dyad_value")

# Plot
ggplot(WLUNA_long3, aes(x = week, y = dyad_value, color = dyad_type)) +
  geom_line() +
  facet_wrap(~ dyad_type, scales = "free_y", ncol = 2) +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Dyad census by week",
       x = "Week",
       y = "Values") +
  theme_minimal()




###Triad census ----

triad <- fread("combined_triad_census.csv")

triad_long <- triad %>%
  pivot_longer(-week, names_to = "triad_type", values_to = "count")

# Plot
ggplot(triad_long, aes(x = week, y = count, color = triad_type)) +
  geom_line() +
  facet_wrap(~ triad_type, scales = "free_y", ncol = 4) +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Triad values by week",
       x = "Week",
       y = "Values") +
  theme_minimal()


###Power law distribution ----

####Alpha ---- 

#Indegree 

ggplot(WLUNA_stats, aes(x = week, y = alpha_in)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Indegree alpha by week",
       x = "Week",
       y = "Indegree alpha") +
  theme_minimal()

#Weighted 

ggplot(WLUNA_stats, aes(x = week, y = alpha_in_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Indegree alpha weighted by week",
       x = "Week",
       y = "Indegree alpha weighted") +
  theme_minimal()


#Outdegree

ggplot(WLUNA_stats, aes(x = week, y = alpha_out)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Outdegree alpha by week",
       x = "Week",
       y = "Outdegree alpha") +
  theme_minimal()

#Weighted 
ggplot(WLUNA_stats, aes(x = week, y = alpha_out_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Outdegree alpha weighted by week",
       x = "Week",
       y = "Outdegree alpha weighted") +
  theme_minimal()


####Xmin ----

#Indegree

ggplot(WLUNA_stats, aes(x = week, y = xmin_in)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Indegree xmin by week",
       x = "Week",
       y = "Indegree xmin") +
  theme_minimal()

#Weighted 

ggplot(WLUNA_stats, aes(x = week, y = xmin_in_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "xmin weighted indegree by week",
       x = "Week",
       y = "xmin weighted indegree") +
  theme_minimal()


#Outdegree

ggplot(WLUNA_stats, aes(x = week, y = xmin_out)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "xmin outdegree by week",
       x = "Week",
       y = "xmin outdegree") +
  theme_minimal()


#weighted 

ggplot(WLUNA_stats, aes(x = week, y = xmin_out_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "xmin weighted outdegree by week",
       x = "Week",
       y = "xmin weighted outdegree") +
  theme_minimal()


####P-value ----

#Indegree

ggplot(WLUNA_stats, aes(x = week, y = pvalue_in)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "P-value indegree by week",
       x = "Week",
       y = "P-value indegree") +
  theme_minimal()

#Weighted 

ggplot(WLUNA_stats, aes(x = week, y = pvalue_in_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "P-value weighted indegree by week",
       x = "Week",
       y = "P-value weighted indegree") +
  theme_minimal()


#Outdegree

ggplot(WLUNA_stats, aes(x = week, y = pvalue_out)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "P-value outdegree by week",
       x = "Week",
       y = "P-value outdegree") +
  theme_minimal()

#Weighted 

ggplot(WLUNA_stats, aes(x = week, y = pvalue_out_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "P-value weighted outdegree by week",
       x = "Week",
       y = "P-value weighted outdegree") +
  theme_minimal()


#Combined plot 

WLUNA_long4 <- WLUNA_stats %>%
  pivot_longer(cols = c(pvalue_in, pvalue_out, pvalue_in_w, pvalue_out_w), 
               names_to = "pvalue_type", 
               values_to = "pvalue_value")

# Plot
ggplot(WLUNA_long4, aes(x = week, y = pvalue_value, color = pvalue_type)) +
  geom_line() +
  facet_wrap(~ pvalue_type, scales = "free_y", ncol = 2) +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Pvalue by week",
       x = "Week",
       y = "Values") +
  theme_minimal()



###Gini-index ----

ggplot(WLUNA_stats, aes(x = week, y = gini_index)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Gini-index by week",
       x = "Week",
       y = "Gini-index") +
  theme_minimal()


#Weakly connected components 

ggplot(WLUNA_stats, aes(x = week, y = gini_index_weak)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Gini-index weak by week",
       x = "Week",
       y = "Gini-index weak") +
  theme_minimal()


#Strongly connected components 

ggplot(WLUNA_stats, aes(x = week, y = gini_index_strong)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Gini-index strong by week",
       x = "Week",
       y = "Gini-index strong") +
  theme_minimal()


#Combined plot 

WLUNA_long5 <- WLUNA_stats %>%
  pivot_longer(cols = c(gini_index, gini_index_weak, gini_index_strong), 
               names_to = "gini_type", 
               values_to = "gini_value")

# Plot
ggplot(WLUNA_long5, aes(x = week, y = gini_value, color = gini_type)) +
  geom_line() +
  facet_wrap(~ gini_type, scales = "free_y", ncol = 2) +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Gini index by week",
       x = "Week",
       y = "Values") +
  theme_minimal()



#UST ----

UST_stats <- fread("statistics_combined_UST.csv")


### Nodes and edges ----

ggplot(UST_stats, aes(x = week, y = n_nodes)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Nodes by week",
       x = "Week",
       y = "Nodes") +
  theme_minimal()

ggplot(UST_stats, aes(x = week, y = n_edges)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Edges by week",
       x = "Week",
       y = "Edges") +
  theme_minimal()


### Indegree -----

ggplot(UST_stats, aes(x = week, y = indegree)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Indegree by week",
       x = "Week",
       y = "Indegree") +
  theme_minimal()

# weighted indegree 

UST_stats$log_weighted_indegree <- log10(as.numeric(UST_stats$weighted_indegree))

ggplot(UST_stats, aes(x = week, y = log_weighted_indegree)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Weighted indegree by week",
       x = "Week",
       y = "Weighted indegree") +
  theme_minimal()


### Outdegree ----

ggplot(UST_stats, aes(x = week, y = outdegree)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Outdegree by week",
       x = "Week",
       y = "Outdegree") +
  theme_minimal()

#weighted outdegree 

UST_stats$log_weighted_outdegree <- log10(as.numeric(UST_stats$weighted_outdegree))

ggplot(UST_stats, aes(x = week, y = log_weighted_outdegree)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Weighted outdegree by week",
       x = "Week",
       y = "Weighted outdegree") +
  theme_minimal()


### Clustering coefficients ----

#Global transitivity 

ggplot(UST_stats, aes(x = week, y = transitivity_global)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Global transitivity by week",
       x = "Week",
       y = "Global transitivity") +
  theme_minimal()


#Weighted 

ggplot(UST_stats, aes(x = week, y = transitivity_global_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Global weighted transitivity by week",
       x = "Week",
       y = "Global weighted transitivity") +
  theme_minimal()


#Average transitivity 

ggplot(UST_stats, aes(x = week, y = transitivity_average)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Average transitivity by week",
       x = "Week",
       y = "Average transitivity") +
  theme_minimal()


#Weighted

ggplot(UST_stats, aes(x = week, y = transitivity_average_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Average weighted transitivity by week",
       x = "Week",
       y = "Average weighted transitivity") +
  theme_minimal()


### Assortativity ----

ggplot(UST_stats, aes(x = week, y = assortativity)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Assortativity by week",
       x = "Week",
       y = "Assortativity") +
  theme_minimal()


# Indegree assortativty 

ggplot(UST_stats, aes(x = week, y = assortativity_in)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Indegree assortativity by week",
       x = "Week",
       y = "Indegree assortativity") +
  theme_minimal()

#Weighted

ggplot(UST_stats, aes(x = week, y = assortativity_in_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Weighted indegree assortativity by week",
       x = "Week",
       y = "Weighted indegree assortativity") +
  theme_minimal()


# Outdegree assortativty 

ggplot(UST_stats, aes(x = week, y = assortativity_out)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Outdegree assortativity by week",
       x = "Week",
       y = "Outdegree assortativity") +
  theme_minimal()

#Weighted

ggplot(UST_stats, aes(x = week, y = assortativity_out_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Weighted outdegree assortativity by week",
       x = "Week",
       y = "Weighted outdegree assortativity") +
  theme_minimal()


### Dyad census ----

#Mutual

ggplot(UST_stats, aes(x = week, y = dyad_mut)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Mutual vertices by week",
       x = "Week",
       y = "Number of mutual vertices") +
  theme_minimal()

#Asymmetric 

ggplot(UST_stats, aes(x = week, y = dyad_asym)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Asymmetric vertices by week",
       x = "Week",
       y = "Number of asymmetric vertices") +
  theme_minimal()

#Null 

ggplot(UST_stats, aes(x = week, y = dyad_null)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Null vertices by week",
       x = "Week",
       y = "Number of null vertices") +
  theme_minimal()


###Triad census ----

triad <- fread("combined_triad_census.csv")

triad_long <- triad %>%
  pivot_longer(-week, names_to = "triad_type", values_to = "count")

# Plot
ggplot(triad_long, aes(x = week, y = count, color = triad_type)) +
  geom_line() +
  facet_wrap(~ triad_type, scales = "free_y", ncol = 4) +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Triad values by week",
       x = "Week",
       y = "Values") +
  theme_minimal()


###Power law distribution ----

####Alpha ---- 

#Indegree 

ggplot(UST_stats, aes(x = week, y = alpha_in)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Indegree alpha by week",
       x = "Week",
       y = "Indegree alpha") +
  theme_minimal()

#Weighted 

ggplot(UST_stats, aes(x = week, y = alpha_in_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Indegree alpha weighted by week",
       x = "Week",
       y = "Indegree alpha weighted") +
  theme_minimal()


#Outdegree

ggplot(UST_stats, aes(x = week, y = alpha_out)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Outdegree alpha by week",
       x = "Week",
       y = "Outdegree alpha") +
  theme_minimal()

#Weighted 
ggplot(UST_stats, aes(x = week, y = alpha_out_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Outdegree alpha weighted by week",
       x = "Week",
       y = "Outdegree alpha weighted") +
  theme_minimal()


####Xmin ----

#Indegree

ggplot(UST_stats, aes(x = week, y = xmin_in)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Indegree xmin by week",
       x = "Week",
       y = "Indegree xmin") +
  theme_minimal()

#Weighted 

ggplot(UST_stats, aes(x = week, y = xmin_in_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "xmin weighted indegree by week",
       x = "Week",
       y = "xmin weighted indegree") +
  theme_minimal()


#Outdegree

ggplot(UST_stats, aes(x = week, y = xmin_out)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "xmin outdegree by week",
       x = "Week",
       y = "xmin outdegree") +
  theme_minimal()


#weighted 

ggplot(UST_stats, aes(x = week, y = xmin_out_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "xmin weighted outdegree by week",
       x = "Week",
       y = "xmin weighted outdegree") +
  theme_minimal()


####P-value ----

#Indegree

ggplot(UST_stats, aes(x = week, y = pvalue_in)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "P-value indegree by week",
       x = "Week",
       y = "P-value indegree") +
  theme_minimal()

#Weighted 

ggplot(UST_stats, aes(x = week, y = pvalue_in_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "P-value weighted indegree by week",
       x = "Week",
       y = "P-value weighted indegree") +
  theme_minimal()


#Outdegree

ggplot(UST_stats, aes(x = week, y = pvalue_out)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "P-value outdegree by week",
       x = "Week",
       y = "P-value outdegree") +
  theme_minimal()

#Weighted 

ggplot(UST_stats, aes(x = week, y = pvalue_out_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "P-value weighted outdegree by week",
       x = "Week",
       y = "P-value weighted outdegree") +
  theme_minimal()


###Gini-index ----

ggplot(UST_stats, aes(x = week, y = gini_index)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Gini-index by week",
       x = "Week",
       y = "Gini-index") +
  theme_minimal()


#Weakly connected components 

ggplot(UST_stats, aes(x = week, y = gini_index_weak)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Gini-index weak by week",
       x = "Week",
       y = "Gini-index weak") +
  theme_minimal()


#Strongly connected components 

ggplot(UST_stats, aes(x = week, y = gini_index_strong)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Gini-index strong by week",
       x = "Week",
       y = "Gini-index strong") +
  theme_minimal()



#DAI ----

DAI_stats <- fread("statistics_combined_DAI.csv")


### Nodes and edges ----

ggplot(DAI_stats, aes(x = week, y = n_nodes)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Nodes by week",
       x = "Week",
       y = "Nodes") +
  theme_minimal()

ggplot(DAI_stats, aes(x = week, y = n_edges)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Edges by week",
       x = "Week",
       y = "Edges") +
  theme_minimal()


### Indegree -----

ggplot(DAI_stats, aes(x = week, y = indegree)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Indegree by week",
       x = "Week",
       y = "Indegree") +
  theme_minimal()

# weighted indegree 

DAI_stats$log_weighted_indegree <- log10(as.numeric(DAI_stats$weighted_indegree))

ggplot(DAI_stats, aes(x = week, y = log_weighted_indegree)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Weighted indegree by week",
       x = "Week",
       y = "Weighted indegree") +
  theme_minimal()


### Outdegree ----

ggplot(DAI_stats, aes(x = week, y = outdegree)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Outdegree by week",
       x = "Week",
       y = "Outdegree") +
  theme_minimal()

#weighted outdegree 

DAI_stats$log_weighted_outdegree <- log10(as.numeric(DAI_stats$weighted_outdegree))

ggplot(DAI_stats, aes(x = week, y = log_weighted_outdegree)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Weighted outdegree by week",
       x = "Week",
       y = "Weighted outdegree") +
  theme_minimal()


### Clustering coefficients ----

#Global transitivity 

ggplot(DAI_stats, aes(x = week, y = transitivity_global)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Global transitivity by week",
       x = "Week",
       y = "Global transitivity") +
  theme_minimal()


#Weighted 

ggplot(DAI_stats, aes(x = week, y = transitivity_global_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Global weighted transitivity by week",
       x = "Week",
       y = "Global weighted transitivity") +
  theme_minimal()


#Average transitivity 

ggplot(DAI_stats, aes(x = week, y = transitivity_average)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Average transitivity by week",
       x = "Week",
       y = "Average transitivity") +
  theme_minimal()


#Weighted

ggplot(DAI_stats, aes(x = week, y = transitivity_average_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Average weighted transitivity by week",
       x = "Week",
       y = "Average weighted transitivity") +
  theme_minimal()


### Assortativity ----

ggplot(DAI_stats, aes(x = week, y = assortativity)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Assortativity by week",
       x = "Week",
       y = "Assortativity") +
  theme_minimal()


# Indegree assortativty 

ggplot(DAI_stats, aes(x = week, y = assortativity_in)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Indegree assortativity by week",
       x = "Week",
       y = "Indegree assortativity") +
  theme_minimal()

#Weighted

ggplot(DAI_stats, aes(x = week, y = assortativity_in_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Weighted indegree assortativity by week",
       x = "Week",
       y = "Weighted indegree assortativity") +
  theme_minimal()


# Outdegree assortativty 

ggplot(DAI_stats, aes(x = week, y = assortativity_out)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Outdegree assortativity by week",
       x = "Week",
       y = "Outdegree assortativity") +
  theme_minimal()

#Weighted

ggplot(DAI_stats, aes(x = week, y = assortativity_out_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Weighted outdegree assortativity by week",
       x = "Week",
       y = "Weighted outdegree assortativity") +
  theme_minimal()


### Dyad census ----

#Mutual

ggplot(DAI_stats, aes(x = week, y = dyad_mut)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Mutual vertices by week",
       x = "Week",
       y = "Number of mutual vertices") +
  theme_minimal()

#Asymmetric 

ggplot(DAI_stats, aes(x = week, y = dyad_asym)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Asymmetric vertices by week",
       x = "Week",
       y = "Number of asymmetric vertices") +
  theme_minimal()

#Null 

ggplot(DAI_stats, aes(x = week, y = dyad_null)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Null vertices by week",
       x = "Week",
       y = "Number of null vertices") +
  theme_minimal()


###Triad census ----

triad <- fread("combined_triad_census.csv")

triad_long <- triad %>%
  pivot_longer(-week, names_to = "triad_type", values_to = "count")

# Plot
ggplot(triad_long, aes(x = week, y = count, color = triad_type)) +
  geom_line() +
  facet_wrap(~ triad_type, scales = "free_y", ncol = 4) +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Triad values by week",
       x = "Week",
       y = "Values") +
  theme_minimal()



###Power law distribution ----

####Alpha ---- 

#Indegree 

ggplot(DAI_stats, aes(x = week, y = alpha_in)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Indegree alpha by week",
       x = "Week",
       y = "Indegree alpha") +
  theme_minimal()

#Weighted 

ggplot(DAI_stats, aes(x = week, y = alpha_in_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Indegree alpha weighted by week",
       x = "Week",
       y = "Indegree alpha weighted") +
  theme_minimal()


#Outdegree

ggplot(DAI_stats, aes(x = week, y = alpha_out)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Outdegree alpha by week",
       x = "Week",
       y = "Outdegree alpha") +
  theme_minimal()

#Weighted 
ggplot(DAI_stats, aes(x = week, y = alpha_out_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Outdegree alpha weighted by week",
       x = "Week",
       y = "Outdegree alpha weighted") +
  theme_minimal()


####Xmin ----

#Indegree

ggplot(DAI_stats, aes(x = week, y = xmin_in)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Indegree xmin by week",
       x = "Week",
       y = "Indegree xmin") +
  theme_minimal()

#Weighted 

ggplot(DAI_stats, aes(x = week, y = xmin_in_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "xmin weighted indegree by week",
       x = "Week",
       y = "xmin weighted indegree") +
  theme_minimal()


#Outdegree

ggplot(DAI_stats, aes(x = week, y = xmin_out)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "xmin outdegree by week",
       x = "Week",
       y = "xmin outdegree") +
  theme_minimal()


#weighted 

ggplot(DAI_stats, aes(x = week, y = xmin_out_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "xmin weighted outdegree by week",
       x = "Week",
       y = "xmin weighted outdegree") +
  theme_minimal()


####P-value ----

#Indegree

ggplot(DAI_stats, aes(x = week, y = pvalue_in)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "P-value indegree by week",
       x = "Week",
       y = "P-value indegree") +
  theme_minimal()

#Weighted 

ggplot(DAI_stats, aes(x = week, y = pvalue_in_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "P-value weighted indegree by week",
       x = "Week",
       y = "P-value weighted indegree") +
  theme_minimal()


#Outdegree

ggplot(DAI_stats, aes(x = week, y = pvalue_out)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "P-value outdegree by week",
       x = "Week",
       y = "P-value outdegree") +
  theme_minimal()

#Weighted 

ggplot(DAI_stats, aes(x = week, y = pvalue_out_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "P-value weighted outdegree by week",
       x = "Week",
       y = "P-value weighted outdegree") +
  theme_minimal()


###Gini-index ----

ggplot(DAI_stats, aes(x = week, y = gini_index)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Gini-index by week",
       x = "Week",
       y = "Gini-index") +
  theme_minimal()


#Weakly connected components 

ggplot(DAI_stats, aes(x = week, y = gini_index_weak)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Gini-index weak by week",
       x = "Week",
       y = "Gini-index weak") +
  theme_minimal()


#Strongly connected components 

ggplot(DAI_stats, aes(x = week, y = gini_index_strong)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Gini-index strong by week",
       x = "Week",
       y = "Gini-index strong") +
  theme_minimal()



#USDP ----

USDP_stats <- fread("statistics_combined_USDP.csv")


### Nodes and edges ----

ggplot(USDP_stats, aes(x = week, y = n_nodes)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Nodes by week",
       x = "Week",
       y = "Nodes") +
  theme_minimal()

ggplot(USDP_stats, aes(x = week, y = n_edges)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Edges by week",
       x = "Week",
       y = "Edges") +
  theme_minimal()


### Indegree -----

ggplot(USDP_stats, aes(x = week, y = indegree)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Indegree by week",
       x = "Week",
       y = "Indegree") +
  theme_minimal()

# weighted indegree 

USDP_stats$log_weighted_indegree <- log10(as.numeric(USDP_stats$weighted_indegree))

ggplot(USDP_stats, aes(x = week, y = log_weighted_indegree)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Weighted indegree by week",
       x = "Week",
       y = "Weighted indegree") +
  theme_minimal()


### Outdegree ----

ggplot(USDP_stats, aes(x = week, y = outdegree)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Outdegree by week",
       x = "Week",
       y = "Outdegree") +
  theme_minimal()

#weighted outdegree 

USDP_stats$log_weighted_outdegree <- log10(as.numeric(USDP_stats$weighted_outdegree))

ggplot(USDP_stats, aes(x = week, y = log_weighted_outdegree)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Weighted outdegree by week",
       x = "Week",
       y = "Weighted outdegree") +
  theme_minimal()


### Clustering coefficients ----

#Global transitivity 

ggplot(USDP_stats, aes(x = week, y = transitivity_global)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Global transitivity by week",
       x = "Week",
       y = "Global transitivity") +
  theme_minimal()


#Weighted 

ggplot(USDP_stats, aes(x = week, y = transitivity_global_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Global weighted transitivity by week",
       x = "Week",
       y = "Global weighted transitivity") +
  theme_minimal()


#Average transitivity 

ggplot(USDP_stats, aes(x = week, y = transitivity_average)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Average transitivity by week",
       x = "Week",
       y = "Average transitivity") +
  theme_minimal()


#Weighted

ggplot(USDP_stats, aes(x = week, y = transitivity_average_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Average weighted transitivity by week",
       x = "Week",
       y = "Average weighted transitivity") +
  theme_minimal()


### Assortativity ----

ggplot(USDP_stats, aes(x = week, y = assortativity)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Assortativity by week",
       x = "Week",
       y = "Assortativity") +
  theme_minimal()


# Indegree assortativty 

ggplot(USDP_stats, aes(x = week, y = assortativity_in)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Indegree assortativity by week",
       x = "Week",
       y = "Indegree assortativity") +
  theme_minimal()

#Weighted

ggplot(USDP_stats, aes(x = week, y = assortativity_in_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Weighted indegree assortativity by week",
       x = "Week",
       y = "Weighted indegree assortativity") +
  theme_minimal()


# Outdegree assortativty 

ggplot(USDP_stats, aes(x = week, y = assortativity_out)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Outdegree assortativity by week",
       x = "Week",
       y = "Outdegree assortativity") +
  theme_minimal()

#Weighted

ggplot(USDP_stats, aes(x = week, y = assortativity_out_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Weighted outdegree assortativity by week",
       x = "Week",
       y = "Weighted outdegree assortativity") +
  theme_minimal()


### Dyad census ----

#Mutual

ggplot(USDP_stats, aes(x = week, y = dyad_mut)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Mutual vertices by week",
       x = "Week",
       y = "Number of mutual vertices") +
  theme_minimal()

#Asymmetric 

ggplot(USDP_stats, aes(x = week, y = dyad_asym)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Asymmetric vertices by week",
       x = "Week",
       y = "Number of asymmetric vertices") +
  theme_minimal()

#Null 

ggplot(USDP_stats, aes(x = week, y = dyad_null)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Null vertices by week",
       x = "Week",
       y = "Number of null vertices") +
  theme_minimal()


###Triad census ----

triad <- fread("combined_triad_census.csv")

triad_long <- triad %>%
  pivot_longer(-week, names_to = "triad_type", values_to = "count")

# Plot
ggplot(triad_long, aes(x = week, y = count, color = triad_type)) +
  geom_line() +
  facet_wrap(~ triad_type, scales = "free_y", ncol = 4) +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Triad values by week",
       x = "Week",
       y = "Values") +
  theme_minimal()





###Power law distribution ----

####Alpha ---- 

#Indegree 

ggplot(USDP_stats, aes(x = week, y = alpha_in)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Indegree alpha by week",
       x = "Week",
       y = "Indegree alpha") +
  theme_minimal()

#Weighted 

ggplot(USDP_stats, aes(x = week, y = alpha_in_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Indegree alpha weighted by week",
       x = "Week",
       y = "Indegree alpha weighted") +
  theme_minimal()


#Outdegree

ggplot(USDP_stats, aes(x = week, y = alpha_out)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Outdegree alpha by week",
       x = "Week",
       y = "Outdegree alpha") +
  theme_minimal()

#Weighted 
ggplot(USDP_stats, aes(x = week, y = alpha_out_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Outdegree alpha weighted by week",
       x = "Week",
       y = "Outdegree alpha weighted") +
  theme_minimal()


####Xmin ----

#Indegree

ggplot(USDP_stats, aes(x = week, y = xmin_in)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Indegree xmin by week",
       x = "Week",
       y = "Indegree xmin") +
  theme_minimal()

#Weighted 

ggplot(USDP_stats, aes(x = week, y = xmin_in_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "xmin weighted indegree by week",
       x = "Week",
       y = "xmin weighted indegree") +
  theme_minimal()


#Outdegree

ggplot(USDP_stats, aes(x = week, y = xmin_out)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "xmin outdegree by week",
       x = "Week",
       y = "xmin outdegree") +
  theme_minimal()


#weighted 

ggplot(USDP_stats, aes(x = week, y = xmin_out_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "xmin weighted outdegree by week",
       x = "Week",
       y = "xmin weighted outdegree") +
  theme_minimal()


####P-value ----

#Indegree

ggplot(USDP_stats, aes(x = week, y = pvalue_in)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "P-value indegree by week",
       x = "Week",
       y = "P-value indegree") +
  theme_minimal()

#Weighted 

ggplot(USDP_stats, aes(x = week, y = pvalue_in_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "P-value weighted indegree by week",
       x = "Week",
       y = "P-value weighted indegree") +
  theme_minimal()


#Outdegree

ggplot(USDP_stats, aes(x = week, y = pvalue_out)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "P-value outdegree by week",
       x = "Week",
       y = "P-value outdegree") +
  theme_minimal()

#Weighted 

ggplot(USDP_stats, aes(x = week, y = pvalue_out_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "P-value weighted outdegree by week",
       x = "Week",
       y = "P-value weighted outdegree") +
  theme_minimal()


###Gini-index ----

ggplot(USDP_stats, aes(x = week, y = gini_index)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Gini-index by week",
       x = "Week",
       y = "Gini-index") +
  theme_minimal()


#Weakly connected components 

ggplot(USDP_stats, aes(x = week, y = gini_index_weak)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Gini-index weak by week",
       x = "Week",
       y = "Gini-index weak") +
  theme_minimal()


#Strongly connected components 

ggplot(USDP_stats, aes(x = week, y = gini_index_strong)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Gini-index strong by week",
       x = "Week",
       y = "Gini-index strong") +
  theme_minimal()




#USDC ----

USDC_stats <- fread("statistics_combined_USDC.csv")


### Nodes and edges ----

ggplot(USDC_stats, aes(x = week, y = n_nodes)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Nodes by week",
       x = "Week",
       y = "Nodes") +
  theme_minimal()

ggplot(USDC_stats, aes(x = week, y = n_edges)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Edges by week",
       x = "Week",
       y = "Edges") +
  theme_minimal()


### Indegree -----

ggplot(USDC_stats, aes(x = week, y = indegree)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Indegree by week",
       x = "Week",
       y = "Indegree") +
  theme_minimal()

# weighted indegree 

USDC_stats$log_weighted_indegree <- log10(as.numeric(USDC_stats$weighted_indegree))

ggplot(USDC_stats, aes(x = week, y = log_weighted_indegree)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Weighted indegree by week",
       x = "Week",
       y = "Weighted indegree") +
  theme_minimal()


### Outdegree ----

ggplot(USDC_stats, aes(x = week, y = outdegree)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Outdegree by week",
       x = "Week",
       y = "Outdegree") +
  theme_minimal()

#weighted outdegree 

USDC_stats$log_weighted_outdegree <- log10(as.numeric(USDC_stats$weighted_outdegree))

ggplot(USDC_stats, aes(x = week, y = log_weighted_outdegree)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Weighted outdegree by week",
       x = "Week",
       y = "Weighted outdegree") +
  theme_minimal()


### Clustering coefficients ----

#Global transitivity 

ggplot(USDC_stats, aes(x = week, y = transitivity_global)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Global transitivity by week",
       x = "Week",
       y = "Global transitivity") +
  theme_minimal()


#Weighted 

ggplot(USDC_stats, aes(x = week, y = transitivity_global_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Global weighted transitivity by week",
       x = "Week",
       y = "Global weighted transitivity") +
  theme_minimal()


#Average transitivity 

ggplot(USDC_stats, aes(x = week, y = transitivity_average)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Average transitivity by week",
       x = "Week",
       y = "Average transitivity") +
  theme_minimal()


#Weighted

ggplot(USDC_stats, aes(x = week, y = transitivity_average_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Average weighted transitivity by week",
       x = "Week",
       y = "Average weighted transitivity") +
  theme_minimal()


### Assortativity ----

ggplot(USDC_stats, aes(x = week, y = assortativity)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Assortativity by week",
       x = "Week",
       y = "Assortativity") +
  theme_minimal()


# Indegree assortativty 

ggplot(USDC_stats, aes(x = week, y = assortativity_in)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Indegree assortativity by week",
       x = "Week",
       y = "Indegree assortativity") +
  theme_minimal()

#Weighted

ggplot(USDC_stats, aes(x = week, y = assortativity_in_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Weighted indegree assortativity by week",
       x = "Week",
       y = "Weighted indegree assortativity") +
  theme_minimal()


# Outdegree assortativty 

ggplot(USDC_stats, aes(x = week, y = assortativity_out)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Outdegree assortativity by week",
       x = "Week",
       y = "Outdegree assortativity") +
  theme_minimal()

#Weighted

ggplot(USDC_stats, aes(x = week, y = assortativity_out_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Weighted outdegree assortativity by week",
       x = "Week",
       y = "Weighted outdegree assortativity") +
  theme_minimal()


### Dyad census ----

#Mutual

ggplot(USDC_stats, aes(x = week, y = dyad_mut)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Mutual vertices by week",
       x = "Week",
       y = "Number of mutual vertices") +
  theme_minimal()

#Asymmetric 

ggplot(USDC_stats, aes(x = week, y = dyad_asym)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Asymmetric vertices by week",
       x = "Week",
       y = "Number of asymmetric vertices") +
  theme_minimal()

#Null 

ggplot(USDC_stats, aes(x = week, y = dyad_null)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Null vertices by week",
       x = "Week",
       y = "Number of null vertices") +
  theme_minimal()

###Power law distribution ----

####Alpha ---- 

#Indegree 

ggplot(USDC_stats, aes(x = week, y = alpha_in)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Indegree alpha by week",
       x = "Week",
       y = "Indegree alpha") +
  theme_minimal()

#Weighted 

ggplot(USDC_stats, aes(x = week, y = alpha_in_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Indegree alpha weighted by week",
       x = "Week",
       y = "Indegree alpha weighted") +
  theme_minimal()


#Outdegree

ggplot(USDC_stats, aes(x = week, y = alpha_out)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Outdegree alpha by week",
       x = "Week",
       y = "Outdegree alpha") +
  theme_minimal()

#Weighted 
ggplot(USDC_stats, aes(x = week, y = alpha_out_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Outdegree alpha weighted by week",
       x = "Week",
       y = "Outdegree alpha weighted") +
  theme_minimal()


####Xmin ----

#Indegree

ggplot(USDC_stats, aes(x = week, y = xmin_in)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Indegree xmin by week",
       x = "Week",
       y = "Indegree xmin") +
  theme_minimal()

#Weighted 

ggplot(USDC_stats, aes(x = week, y = xmin_in_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "xmin weighted indegree by week",
       x = "Week",
       y = "xmin weighted indegree") +
  theme_minimal()


#Outdegree

ggplot(USDC_stats, aes(x = week, y = xmin_out)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "xmin outdegree by week",
       x = "Week",
       y = "xmin outdegree") +
  theme_minimal()


#weighted 

ggplot(USDC_stats, aes(x = week, y = xmin_out_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "xmin weighted outdegree by week",
       x = "Week",
       y = "xmin weighted outdegree") +
  theme_minimal()


####P-value ----

#Indegree

ggplot(USDC_stats, aes(x = week, y = pvalue_in)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "P-value indegree by week",
       x = "Week",
       y = "P-value indegree") +
  theme_minimal()

#Weighted 

ggplot(USDC_stats, aes(x = week, y = pvalue_in_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "P-value weighted indegree by week",
       x = "Week",
       y = "P-value weighted indegree") +
  theme_minimal()


#Outdegree

ggplot(USDC_stats, aes(x = week, y = pvalue_out)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "P-value outdegree by week",
       x = "Week",
       y = "P-value outdegree") +
  theme_minimal()

#Weighted 

ggplot(USDC_stats, aes(x = week, y = pvalue_out_w)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "P-value weighted outdegree by week",
       x = "Week",
       y = "P-value weighted outdegree") +
  theme_minimal()


###Gini-index ----

ggplot(USDC_stats, aes(x = week, y = gini_index)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Gini-index by week",
       x = "Week",
       y = "Gini-index") +
  theme_minimal()


#Weakly connected components 

ggplot(USDC_stats, aes(x = week, y = gini_index_weak)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Gini-index weak by week",
       x = "Week",
       y = "Gini-index weak") +
  theme_minimal()


#Strongly connected components 

ggplot(USDC_stats, aes(x = week, y = gini_index_strong)) +
  geom_point() +
  geom_vline(xintercept = 19, linetype = "dashed", color = "red") +
  labs(title = "Gini-index strong by week",
       x = "Week",
       y = "Gini-index strong") +
  theme_minimal()







