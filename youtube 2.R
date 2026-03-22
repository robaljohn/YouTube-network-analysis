
# ----------------------------
# 1. Clean environment
# ----------------------------
rm(list = ls())
Sys.setenv(LANG = "en")

# ----------------------------
# 2. Load libraries
# ----------------------------
library(igraph)
library(dplyr)
library(ggplot2)

# ----------------------------
# 3. Load dataset
# ----------------------------

setwd("C:/Users/johnr/Desktop/coursera/social network analysis")
sns <- read.csv("SNS.csv", stringsAsFactors = FALSE)

#  project applies bipartite network analysis
#  to model and examine the structural relationships
#  between YouTube channels and videos, 
#  focusing on degree patterns, connectivity,
#  and channel interactions derived from shared content.

# Inspect internal structure of my dataset

str(sns)
head(sns)
tail(sns)

# ----------------------------
# 4. Prepare edge list
# ----------------------------
# construct a clean edge list where each row represents 
# a unique directed link from a channel to a video


edges <- select(sns, source, Target)
edges <- filter(edges, !is.na(source), !is.na(Target))
edges <- distinct(edges)


# ----------------------------
# 5. Build directed bipartite network
# ----------------------------
# construct a directed network where 
# edges represent channels linking to videos

g <- graph_from_data_frame(edges, directed = TRUE)

# ----------------------------
# 6. Define node types (bipartite)

# Is this node listed as a source in the edge list?
# TRUE  = Channel
# FALSE = Video

V(g)$type <- V(g)$name %in% edges$source

V(g)$type

# Sanity checks

stopifnot(is.logical(V(g)$type))
stopifnot(!any(is.na(V(g)$type)))
stopifnot(is_bipartite(g))

# ----------------------------
# 7. Basic network statistics
# ----------------------------
# density means how many of the possible directed edges present in network

cat("Number of nodes:", gorder(g), "\n")
cat("Number of edges:", gsize(g), "\n")
cat("Network density:", edge_density(g), "\n")

# ----------------------------
# Average degree
# ----------------------------
# to know the mean number of connection per node in network

avg_total_degree <- mean(degree(g, mode = "all"))
avg_in_degree    <- mean(degree(g, mode = "in"))
avg_out_degree   <- mean(degree(g, mode = "out"))

cat("Average total degree:", avg_total_degree, "\n")
cat("Average in-degree:", avg_in_degree, "\n")
cat("Average out-degree:", avg_out_degree, "\n")

# ----------------------------
# 8. Network visualization
# ----------------------------
V(g)$color <- ifelse(V(g)$type, "lightblue", "gold")
V(g)$size  <- ifelse(V(g)$type, 10,6)

set.seed(123)
plot(
  g,
  vertex.label = NA,
  vertex.size = V(g)$size,
  vertex.color = V(g)$color,
  edge.arrow.size = 0.2,
  edge.color = "grey40",
  main = "YouTube Channel–Video Bipartite Network"
)


# ----------------------------
# 9. Degree centrality 
# ----------------------------
# Out-degree measures how much a channel produces video
# in-degree measures how much attention a video receives from channel

out_deg <- degree(g, mode = "out")   # Channel activity
in_deg  <- degree(g, mode = "in")    # Video attention

channel_outdegree <- out_deg[V(g)$type]
video_indegree    <- in_deg[!V(g)$type]

# ranks channels by activity and prints the most prolific ones
cat("\nTop Channels by Out-Degree:\n")
print(head(sort(channel_outdegree, decreasing = TRUE), 2))

# lists the videos receiving the most incoming links
cat("\nTop Videos by In-Degree:\n")
print(head(sort(video_indegree, decreasing = TRUE), 30))

# ----------------------------
# Degree distribution
# ----------------------------

# tells how ties are distributed among the nodes
# how many nodes have 1 connection, 2 connection e.t.c
# Out-degree distribution for channels
channel_out_deg <- out_deg[V(g)$type]
out_deg_distribution <- table(channel_out_deg)

cat("\nOut-Degree Distribution (Channels):\n")
print(out_deg_distribution)

# In-degree distribution for videos
video_in_deg <- in_deg[!V(g)$type]
in_deg_distribution <- table(video_in_deg)

cat("\nIn-Degree Distribution (Videos):\n")
print(in_deg_distribution)


# ----------------------------
# 10. Connected components (undirected)

# removed direction to study pure connectivity 
# and identified groups of nodes that belong together

g_und <- as_undirected(g, mode = "collapse")

# Finds connected components

comp <- components(g_und)

cat("\nNumber of connected components:", comp$no, "\n")
cat("Size of largest component:", max(comp$csize), "\n")

# ----------------------------
# 11. Clustering coefficient
# NOTE: Bipartite networks have zero clustering by definition
# ----------------------------
# Clustering is zero because 
# Channel–Video network cannot form triangles by design

# Global clustering summarizes the whole network
cat("\nGlobal clustering coefficient:",
    transitivity(g_und, type = "global"), "\n")

# local clustering describes each node’s immediate neighborhood
cat("\nLocal clustering coefficient:",
    transitivity(g_und, type = "local"), "\n")

# ----------------------------
# 12. Channel–Channel projection
# ----------------------------
# An edge would exist only if two channels share at least one video

channel_projection <- bipartite_projection(g, which = "true")

# The two channels are related through shared video content
plot(
  channel_projection,
  vertex.size = 10,
  vertex.label = NA,
  main = "Channel–Channel Projection (Shared Videos)"
)

# to show how many videos they share

E(channel_projection)$weight


# ----------------------------
# 13. Centrality measures on projection
# ----------------------------
# Number of other channels a channel is connected to
deg_proj <- degree(channel_projection)

# Measures how often a node lies on shortest paths
# those nodes which control the information flow
bet_proj <- betweenness(channel_projection)

# A node is important if it is connected to other important nodes
# Both channels are equally important relative to each other
eig_proj <- eigen_centrality(channel_projection)$vector

# Measures how close a node is to all others
# symmetric two-node structure of the channel–channel projection
clo_proj <- closeness(channel_projection)


# Create centrality dataframe
centrality_df <- data.frame(
  channel = V(channel_projection)$name,
  degree = deg_proj,
  betweenness = bet_proj,
  eigenvector = eig_proj,
  closeness = clo_proj
)


############################################################
############################################################
