# Integrating Data Exchange and Analysis for Networks (IDEANet)

<p align="center">
    <img width="400" src="https://github.com/Tom-Wolff/ideanet/assets/36702189/9f0b61b5-9106-4ca4-baa3-c3b2309b4c0f" alt="IDEANet logo">
</p>

The IDEANet project (NSF Grant # 2024271) aims to maximize scientific discovery in network science by significantly lowering the analytic and access barriers-to-entry for
researchers. As part of this effort, the `ideanet` package offers a set of integrated modules to securely access, process, analyze, and visualize existing network data using
expert-level analytics while conforming to requirements set by source institutions. Our hope is that this project will increase collaboration on intensive, cross-disciplinary data science questions across the social and behavioral sciences.

`ideanet`’s core analytic tools automatically generate node- and system-level measures commonly used in the analysis of sociocentric and egocentric network data. These default computations maximize the ability of entry-level users and non-expert practitioners to employ network measurements in further analyses while making all users less prone to common data analytic errors. Moreover, we hope that the `ideanet` package will be a valuable resource in educational settings, providing an accessible starting point for training the next generation of network scholars.

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
- Number of Isolates
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

`netwrite` includes support for networks with weighted edges, as well as for networks with multiple "types" or "levels" of edges. `netwrite` also produces several additional outputs that aid in sociocentric network analysis. These include cleanly-formatted edgelists, summary visualizations, and `igraph` objects for aggregate networks and networks of specific edge types.

## Egocentric Data Processing

`ideanet` features a set of additional functions designed for working with egocentric data. The primary function in this set, `ego_netwrite`, reads in a data frame of egos, a second data frame of alters nominated by each ego, and an optional third data frame containing edges existing between alters as reported by an ego. Using these data frames, `ego_netwrite` generates measures of centrality and position for each node in an ego network, summaries of each individual ego network, and a summary of the data as a whole. These outputs provide users with the means to make inferences from their data at various levels of analysis, and allow users to identify typical properties of networks in their data. Measures featured in `ego_netwrite`'s output include:

### Node-Level Measures

- Degree (In, Out, Undirected)
- Closeness Centrality
- Betweenness Centrality
- Bonacich Centrality
- Eigenvector Centrality
- Burt's Constraint
- Burt's Effective Size
- Reachability

### Network-Level Measures

- Network Size (Nodes, Edges)
- Number of Components
- Network Density
- Mean Alter Degree
- Number of Isolate Alters
- Multi-level Edge Correlation
- Fragmentation Index
- Pairwise Reachability
- E-I Index
- Herfindahl Index
- Index of Qualitative Variation (IQV)
- Pearson's Phi
- Dyad Census
- Triad Census
- Position (Motif) Census

### Dataset-Level Measures

- Number of Egos
- Number of Nominated Alters
- Number of Isolate Egos
- Average Network Size
- Smallest Non-Isolate Network Size
- Largest Network Size
- Average Density
- Average Fragmentation
 
## Advanced Analysis Modules

`ideanet` includes modules for advanced analysis, allowing researchers to extend the utility of `netwrite` and its outputs. Modules for Multiple Regression Quadratic Assignment Procedure (MRQAP) and Positional (Role) Analysis are currently available, and additional modules are expected to come in the near future.


## Interactive GUI for Sociocentric Networks

<br>
<p align="center">
<img width="800" alt="ideanetViz_screen" src="https://github.com/Tom-Wolff/ideanet/assets/36702189/20907279-d195-4439-ab1f-e44e6d7f070f">
</p>
<br>

The `ideanetViz` Shiny app presents the output of `ideanet`'s workflow for sociocentric data in a clear and accessible GUI. This GUI is convenient for users with limited R experience and is useful for classrooms, workshops, and other educational spaces. It is also useful for experienced users interested in quick exploration of network data. Moreover, `ideanetViz` streamlines customization of network visualizations and provides quick access into `ideanet`'s advanced analysis modules.

## Support and Compatibility with Other Network Analysis Packages

`ideanet` is designed to be versatile and compatible with other tools for social network analysis. The package includes a convenient function for reading several types of sociocentric network data files into R (`netread`), including those associated with software packages like UCINet and Pajek. This affords users a greater ability to access and work with network data even if they decide to use tools other than `netwrite` for analysis.

`ideanet` gives similar consideration to egocentric data. Although `ego_netwrite` requires three separate data frames for egos, alters, and edges between alters, ego networks are often stored in a single wide dataset. With this in mind, the `ego_reshape` function allows users to reshape their data into a structure more compatible with `ego_netwrite` and other popular R packages for ego network analysis. Additionally, our package includes a function specifically designed to read and process data generated using [Network Canvas](https://networkcanvas.com/), an increasingly popular tool for capturing egocentric network data. Further, `ego_netwrite` gives users the option to export their data as an `egor` object for use with the `egor` R package, which enables users to fit exponential random graph models using egocentric data.
