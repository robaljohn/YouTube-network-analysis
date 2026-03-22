# YouTube Bipartite Network Analysis

## Overview
This project applies **bipartite network analysis** to model relationships between YouTube channels and videos.

The goal is to analyze:
- Degree patterns
- Connectivity structure
- Channel interactions via shared videos

## Dataset
- Nodes: Channels and Videos
- Edges: Channel → Video relationships

## Methods
- Graph construction using `igraph`
- Degree centrality analysis
- Connected components
- Bipartite projection (channel–channel)
- Centrality measures:
  - Degree
  - Betweenness
  - Eigenvector
  - Closeness

## Key Insights
- Highly active channels identified via out-degree
- Popular videos identified via in-degree
- Network is sparse with low density
- Channel projection reveals shared content structure

## Tools
- R
- igraph
- dplyr
- ggplot2

## How to Run
```r
source("scripts/analysis.R")
