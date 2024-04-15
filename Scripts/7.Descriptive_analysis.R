setwd()
options(scipen = 999)

library(data.table)
library(tidyverse)
library(igraph)
library(ggraph)
library(tidygraph)
library(dplyr)
library(ggplot2)
library(sna)

df <- fread("token_transfers_dict.csv")

### USDT ----

USDT <- df[df$symbol == "USDT", ]

#select only the columns of interest

edges <- USDT[, 2:3]


# igraph object -----

vertices <- unique(c(as.character(USDT$from_address_id), as.character(USDT$to_address_id)))

USDT_net <- graph_from_data_frame(edges, directed = TRUE, vertices = vertices)


#edge attributes ----

attributes <- USDT[, 4:6]

edge_attr(USDT_net) <- attributes 
edge_attr_names(USDT_net)


#number of nodes and edges ----

vcount(USDT_net)
ecount(USDT_net)

# 4606308 nodes/vertices
# 19297191 edges/links 


#edge density ----

edge_density(USDT_net)

# 0.0000009094697


#Centrality measures ----

#indegree and outdegree
USDT_tblgr <- USDT_net %>%
  as_tbl_graph()

USDT_tblgr <- USDT_tblgr %>%
  mutate(
    indegree = centrality_degree(mode = "in"),
    outdegree = centrality_degree(mode="out")
  )


indegree_centrality1 <- igraph::degree(USDT_net, mode = "in")
which(indegree_centrality1 == max(indegree_centrality1))

outdegree_centrality1 <- igraph::degree(USDT_net, mode = "out")
which(outdegree_centrality1 == max(outdegree_centrality1))

#nodes 1079552 and 814 have highest indegree centrality, nodes 4469565 2241 highest outdegree


#betweenness 

betweenness_centrality1 <- igraph::betweenness(USDT_net)
which(betweennes_centrality1 == max(betweenness_centrality1))

#Transitivity ----
transitivity(USDT_net, type = "global")

# 0.00001583414
#low level of clustering 


#Assortativity ----

indegree <- igraph::degree(USDT_net, mode = "in")
assortativity(USDT_net, indegree)

#-0.1211742

outdegree <- igraph::degree(USDT_net, mode = "out")
assortativity(USDT_net, outdegree)

#-0.2170874

#Average degree ----
mean(indegree)
mean(outdegree)

# 4.19

#Transactions ----

USDT[which.max(USDT$value), ]



### USDC ----

USDC <- df[df$symbol == "USDC", ]

#select only the columns of interest

edges2 <- USDC[, 2:3]


# igraph object -----

vertices2 <- unique(c(as.character(USDC$from_address_id), as.character(USDC$to_address_id)))

USDC_net <- graph_from_data_frame(edges2, directed = TRUE, vertices = vertices2)


#edge attributes ----

attributes2 <- USDC[, 4:6]

edge_attr(USDC_net) <- attributes2 
edge_attr_names(USDC_net)


#number of nodes and edges ----

vcount(USDC_net)
ecount(USDC_net)

# 2137262 nodes/vertices
# 14546807 edges/links 


#edge density ----

edge_density(USDC_net)

# 0.000003184581


#Centrality measures ----

#indegree and outdegree
USDC_tblgr <- USDC_net %>%
  as_tbl_graph()

USDC_tblgr <- USDC_tblgr %>%
  mutate(
    indegree = centrality_degree(mode = "in"),
    outdegree = centrality_degree(mode="out")
  )


indegree_centrality2 <- igraph::degree(USDC_net, mode="in")
which(indegree_centrality2 == max(indegree_centrality2))

outdegree_centrality2 <- igraph::degree(USDC_net, mode="out")
which(outdegree_centrality2 == max(outdegree_centrality2))

#nodes 2606725 and 113 have highest indegree and outdegree centrality 


#Transitivity ----
transitivity(USDC_net, type = "global")

#0.0000503989
#low level of clustering 


#Assortativity ----

indegree <- igraph::degree(USDC_net, mode = "in")
assortativity(USDC_net, indegree)

#-0.027728880

outdegree <- igraph::degree(USDC_net, mode = "out")
assortativity(USDC_net, outdegree)

#-0.03279181

#Average degree ----
mean(indegree)
mean(outdegree)

# 6.81

#Transactions ----

USDC[which.max(USDC$value), ]



### USDP ----

USDP <- df[df$symbol == "USDP", ]

#select only the columns of interest

edges3 <- USDP[, 2:3]


# igraph object -----

vertices3 <- unique(c(as.character(USDP$from_address_id), as.character(USDP$to_address_id)))

USDP_net <- graph_from_data_frame(edges3, directed = TRUE, vertices = vertices3)


#edge attributes ----

attributes3 <- USDP[, 4:6]

edge_attr(USDP_net) <- attributes3
edge_attr_names(USDP_net)


#number of nodes and edges ----

vcount(USDP_net)
ecount(USDP_net)

# 12790 nodes/vertices
# 66438 edges/links 


#edge density ----

edge_density(USDP_net)

# 0.0004061715


#Centrality measures ----

#indegree and outdegree
USDP_tblgr <- USDP_net %>%
  as_tbl_graph()

USDP_tblgr <- USDP_tblgr %>%
  mutate(
    indegree = centrality_degree(mode = "in"),
    outdegree = centrality_degree(mode="out")
  )


indegree_centrality3 <- igraph::degree(USDP_net, mode = "in")
which(indegree_centrality3 == max(indegree_centrality3))

outdegree_centrality3 <- igraph::degree(USDP_net, mode = "out")
which(outdegree_centrality3 == max(outdegree_centrality3))

#nodes 5683337 and 2 have highest indegree and outdegree centrality 


#Transitivity ----
transitivity(USDP_net, type = "global")

#0.002485692
#low level of clustering 


#Assortativity ----

indegree <- igraph::degree(USDP_net, mode = "in")
assortativity(USDP_net, indegree)

# 0.5396724

outdegree <- igraph::degree(USDP_net, mode = "out")
assortativity(USDP_net, outdegree)

# 0.5570173

#Average degree ----
mean(indegree)
mean(outdegree)

# 5.19


#Transactions ----

USDP[which.max(USDP$value), ]


### DAI ----

DAI <- df[df$symbol == "DAI", ]

#select only the columns of interest

edges4 <- DAI[, 2:3]


# igraph object -----

vertices4 <- unique(c(as.character(DAI$from_address_id), as.character(DAI$to_address_id)))

DAI_net <- graph_from_data_frame(edges4, directed = TRUE, vertices = vertices4)


#edge attributes ----

attributes4 <- DAI[, 4:6]

edge_attr(DAI_net) <- attributes4
edge_attr_names(DAI_net)


#number of nodes and edges ----

vcount(DAI_net)
ecount(DAI_net)

# 328098 nodes/vertices
# 2140217 edges/links 


#edge density ----

edge_density(DAI_net)

# 0.00001988163


#Centrality measures ----

#indegree and outdegree
DAI_tblgr <- DAI_net %>%
  as_tbl_graph()

DAI_tblgr <- DAI_tblgr %>%
  mutate(
    indegree = centrality_degree(mode = "in"),
    outdegree = centrality_degree(mode="out")
  )



indegree_centrality4 <- igraph::degree(DAI_net, mode="in")
which(indegree_centrality4 == max(indegree_centrality4))

outdegree_centrality4 <- igraph::degree(DAI_net, mode="out")
which(outdegree_centrality4 == max(outdegree_centrality4))

#nodes 2541378 and 2523 have highest indegree and outdegree centrality 


#Transitivity ----
transitivity(DAI_net, type = "global")

#0.000348559
#low level of clustering 


#Assortativity ----

indegree <- igraph::degree(DAI_net, mode = "in")
assortativity(DAI_net, indegree)

# 0.06755814

outdegree <- igraph::degree(DAI_net, mode = "out")
assortativity(DAI_net, outdegree)

# 0.06882734

#Average degree ----
mean(indegree)
mean(outdegree)

# 6.523103


#Transactions ----

DAI[which.max(DAI$value), ]


### WLUNA ----

WLUNA <- df[df$symbol == "WLUNA", ]

#select only the columns of interest

edges6 <- WLUNA[, 2:3]


# igraph object -----

vertices6 <- unique(c(as.character(WLUNA$from_address_id), as.character(WLUNA$to_address_id)))

WLUNA_net <- graph_from_data_frame(edges6, directed = TRUE, vertices = vertices6)


#edge attributes ----

attributes6 <- WLUNA[, 4:6]

edge_attr(WLUNA_net) <- attributes6
edge_attr_names(WLUNA_net)


#number of nodes and edges ----

vcount(WLUNA_net)
ecount(WLUNA_net)

# 104769 nodes/vertices
# 454227 edges/links 


#edge density ----

edge_density(WLUNA_net)

# 0.000041382


#Centrality measures ----

#indegree and outdegree
WLUNA_tblgr <- WLUNA_net %>%
  as_tbl_graph()

WLUNA_tblgr <- WLUNA_tblgr %>%
  mutate(
    indegree = centrality_degree(mode = "in"),
    outdegree = centrality_degree(mode="out")
  )



indegree_centrality5 <- igraph::degree(WLUNA_net, mode= "in")
which(indegree_centrality5 == max(indegree_centrality5))

outdegree_centrality5 <- igraph::degree(WLUNA_net, mode= "out")
which(outdegree_centrality5 == max(outdegree_centrality5))

#nodes 3080504 and 6853 have highest indegree centrality, while 3147284 and 6777 the highest outdegree


#Transitivity ----
transitivity(WLUNA_net, type = "global")

# 0.0000894983
#low level of clustering 


#Assortativity ----

indegree <- igraph::degree(WLUNA_net, mode = "in")
assortativity(WLUNA_net, indegree)

# -0.08560004

outdegree <- igraph::degree(WLUNA_net, mode = "out")
assortativity(WLUNA_net, outdegree)

# -0.3617205

#Average degree ----
mean(indegree)
mean(outdegree)

# 4.900637


#Transactions ----

WLUNA[which.max(WLUNA$value), ]


### UST ----

UST <- df[df$symbol == "UST", ]

#select only the columns of interest

edges5 <- UST[, 2:3]


# igraph object -----

vertices5 <- unique(c(as.character(UST$from_address_id), as.character(UST$to_address_id)))

UST_net <- graph_from_data_frame(edges5, directed = TRUE, vertices = vertices5)


#edge attributes ----

attributes5 <- UST[, 4:6]

edge_attr(UST_net) <- attributes5
edge_attr_names(UST_net)


#number of nodes and edges ----

vcount(UST_net)
ecount(UST_net)

# 48489 nodes/vertices
# 237627 edges/links 


#edge density ----

edge_density(UST_net)

# 0.0001010691


#Centrality measures ----

#indegree and outdegree
UST_tblgr <- UST_net %>%
  as_tbl_graph()

UST_tblgr <- UST_tblgr %>%
  mutate(
    indegree = centrality_degree(mode = "in"),
    outdegree = centrality_degree(mode="out")
  )


indegree_centrality6 <- igraph::degree(UST_net, mode = "in")
which(indegree_centrality6 == max(indegree_centrality6))

outdegree_centrality6 <- igraph::degree(UST_net, mode = "out")
which(outdegree_centrality6 == max(outdegree_centrality6))

#nodes 3662139 and 6856 have highest indegree and outdegree centrality 


#Transitivity ----
transitivity(UST_net, type = "global")

#0.0001405661
#low level of clustering 


#Assortativity ----

indegree <- igraph::degree(UST_net, mode = "in")
assortativity(UST_net, indegree)

# -0.1784298

outdegree <- igraph::degree(UST_net, mode = "out")
assortativity(UST_net, outdegree)

# -0.2159408

#Average degree ----
mean(indegree)
mean(outdegree)

# 4.900637


#Transactions ----

UST[which.max(UST$value), ]


### NETWORK -----

#select only the columns of interest

edges6 <- df[, 2:3]


# igraph object -----

vertices6 <- unique(c(as.character(df$from_address_id), as.character(df$to_address_id)))

df_net <- graph_from_data_frame(edges6, directed = TRUE, vertices = vertices6)


#edge attributes ----

attributes6 <- df[, 4:6]

edge_attr(df_net) <- attributes6 
edge_attr_names(df_net)


#number of nodes and edges ----

vcount(df_net)
ecount(df_net)

# 6723388 nodes/vertices
# 36742507 edges/links 


#edge density ----

edge_density(df_net)

# 0.0000008128165


#Centrality measures ----

#indegree and outdegree
df_tblgr <- df_net %>%
  as_tbl_graph()

df_tblgr <- df_tblgr %>%
  mutate(
    indegree = centrality_degree(mode = "in"),
    outdegree = centrality_degree(mode="out")
  )


indegree_centrality7 <- igraph::degree(df_net, mode="in")
which(indegree_centrality7 == max(indegree_centrality7))

outdegree_centrality7 <- igraph::degree(df_net, mode="out")
which(outdegree_centrality7 == max(outdegree_centrality7))

#nodes 1079552 and 15437 have highest degree centrality, while 3080504 and 17384 the highest otdegree



#Transitivity ----
transitivity(df_net, type = "global")

# 0.00002024167
#low level of clustering 


#Assortativity ----

indegree <- igraph::degree(df_net, mode = "in")
assortativity(df_net, indegree)

#-0.04517084

outdegree <- igraph::degree(df_net, mode = "out")
assortativity(df_net, outdegree)

#-0.05431581

#Average degree ----
mean(indegree)
mean(outdegree)

# 5.47

#Transactions ----

df[which.max(df$value), ]  #WLUNA transaction








