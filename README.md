# Integrating Data Exchange and Analysis for Networks (IDEANet)
The IDEANet project (NSF Grant # 2024271) aims to maximize scientific discovery in network science by significantly lowering the analytic and access barriers-to-entry for
researchers. As part of this effort, the `ideanet` package offers a set of integrated modules to securely access, process, analyze, and visualize existing network data using
expert-level analytics while conforming to requirements set by source institutions. Our hope is that this project will increase collaboration on intensive, cross-disciplinary data science questions across the social and behavioral sciences.

`ideanet`’s core analytic tools automatically generate node- and system-level measures commonly used in the analysis of sociocentric and egocentric network data. These default computations maximize the ability of entry-level users and non-expert practitioners to employ network measurements in further analyses while making all users less prone to common data analytic errors.

## Sociocentric Data Processing

Users applying `ideanet` to sociocentric data can use the `netwrite` function to generate an extensive set of measures and summaries of their networks. By applying a single, convenient function to an edgelist, adjacency matrix, or adjacency list, users can quickly produce the following measures:

### Node-Level Measures

- Degree (In, Out, Undirected)
- Closeness Centrality
- Betweenness Centrality
- Bonacich Centrality
- Eigenvector Centrality
- Burt's Hierarchy
- Burt's Constraint
- Burt's Effective Size
- Reachability
- Component Membership (Weak, Strong, Bicomponent)

### System-Level Measures

- Directedness
- Weightedness
- Network Size (Nodes, Edges)
- Density
- Multiple Edge Types
- Number of Components (Weak, Strong, Bicomponent)
- Size of Components
- Proportion of Nodes in Components
- Dyad Census
- Degree Assortativity
- Reciprocity Rate
- Transitivity Rate
- Transitivity Correlation
- Global Clustering Coefficient
- Average Geodesic
- Multi-level Edge Correlation
- Pairwise Reachability
- Matrix Singularity

`netwrite` includes full support for networks with weighted edges, as well as networks with multiple "types" or "level" of edges. `netwrite` also produces several additional outputs that aid in sociocentric network analysis. These include cleanly-formatted edgelists, summary visualizations, and `igraph` objects for aggregate networks and networks of specific edge types, 

## Egocentric Data Processing


## Advanced Analytic Modules

## Support and Compatibility with Other Network Analysis Packages
