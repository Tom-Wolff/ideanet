#' Reading Network Data Files and Initial Cleaning (`netread`)
#'
#' @description The `netread` function reads in various files storing relational data converts them into edgelists that ensure their compatibility with other `ideanet` functions.
#'
#' @param path A character value indicating the path of the file which the data are to be read from. If `netread` is converting `igraph` or `network` objects, no file path is needed.
#' @param filetype A character value indicating the type of file being read. Valid arguments are `"csv"`, `"excel"` (.xls, .xlsx), `"igraph"` (for `igraph` objects), `"network"` or `"sna"` (for `network` objects), `"pajek"` (for Pajek files), and `"ucinet"` (for UCINet files).
#' @param sheet If reading in an Excel file with multiple sheets, a character value indicating the name of the sheet on which the core relational data are stored.
#' @param nodelist If the relational data being read have a corresponding file for node-level information, a character value indicating the path of the file which this data are to be read from.
#' @param node_sheet If reading in an Excel file with multiple sheets, a character value indicating the name of the sheet on which the node-level information is store.
#' @param object If converting an `igraph` or `network` object, the object to be converted.
#' @param format For reading CSV and Excel files, a character value indicating the format in which relational data are structured in the file. Valid arguments include `"edgelist"`, `"adjacency_matrix"`, and `"adjacency_list"`.
#' @param col_names For reading CSV and Excel files, a logical value indicating whether the first row in the file serves as the file's header and contains the names of each column.
#' @param row_names For reading CSV and Excel files, a logical value indicating whether the first column in the file contains ID values for each row and should not be treated as part of the core data.
#' @param i_elements If `format` is set to `edgelist`, a character value indicating the name of the column containing the sender of ties in the edgelist. If not specified, `netread` assumes the first column of the data represents tie senders.
#' @param j_elements  If `format` is set to `edgelist`, a character value indicating the name of the column containing the receiver of ties in the edgelist. If not specified, `netread` assumes the second column of the data represents tie receivers
#' @param net_name A character value indicating the name of the network being read from the file(s). This name will be used as a prefix for both outputs created by `netread`.
#' @param missing_code A numeric value indicating "missing" values in the data being read. Such "missing" values are sometimes included to identify the presence of isolated nodes in an edgelist when a corresponding nodelist is unavailable.
#'
#' @return `netread` creates an edgelist and a nodelist in the R Global Environment, both of which are formatted to be compatible with the `netwrite` function. These objects are names `[net_name]_edgelist` and `[net_name]_nodelist`, respectively.
#' @export

require(readxl)
require(stringr)
require(dplyr)
require(tidyr)

netread <- function(path = NULL,
                    filetype = NULL,
                    sheet = NULL,
                    nodelist,
                    node_sheet = NULL,
                    object = NULL,
                    col_names = TRUE,
                    row_names = FALSE,
                    format = NULL,
                    net_name = "network",
                    missing_code = 99999) {

  # CSV
  if (filetype == "csv" | stringr::str_detect(path, "csv$")) {

    netread_csv(path = path,
                nodelist = nodelist,
                col_names = col_names,
                row_names = row_names,
                format = format,
                i_elements = i_elements,
                j_elements = j_elements,
                net_name = net_name,
                missing_code = missing_code)

  } else if (filetype == "excel" | stringr::str_detect(path, "xls$") | stringr::str_detect(path, "xlsx$")) {

    netread_excel(path = path,
                  nodelist = nodelist,
                  sheet = sheet,
                  node_sheet = node_sheet,
                  col_names = col_names,
                  row_names = row_names,
                  format = format,
                  i_elements = i_elements,
                  j_elements = j_elements,
                  net_name = net_name,
                  missing_code = missing_code)

  } else if (is.null(path) & filetype == "igraph" | class(object) == "igraph") {

    netread_igraph(object = object, net_name = net_name)

  } else if (is.null(path) & filetype %in% c("network", "sna" | class(object) == "network")) {

    netread_sna(object = object, net_name = net_name)

  } else if (filetype == "pajek") {

    netread_pajek(path = path, net_name = net_name)

  } else if (filetype == "ucinet") {

    netread_ucinet(path = path, net_name = net_name)

  } else {

    base::message("Error: File type misspecified or not supported")
  }


}




# CSV
netread_csv <- function(path,
                        nodelist = NULL,
                        col_names = TRUE,
                        row_names = FALSE,
                        format = "edgelist",
                        i_elements = NULL,
                        j_elements = NULL,
                        net_name = "network",
                        missing_code = 99999) {


  # Read in CSV file
  main_data <- read.csv(file = path,
                        header = col_names)


  # Read in nodelist if applicable
  if (!is.null(nodelist)) {

    nodes <- read.csv(file = nodelist,
                      header = col_names)

    assign(x = paste(net_name, "nodelist", sep = "_"), value = nodes, envir = .GlobalEnv)

  }


  ########## Edgelist handing
  if (format == "edgelist") {

    if (col_names == TRUE) {

      ### If names of i and j columns specified, relabel
      if (!is.null(i_elements)) {
        colnames(main_data) <- stringr::str_replace_all(colnames(main_data), i_elements, "i_elements")
      }

      if (!is.null(j_elements)) {
        colnames(main_data) <- stringr::str_replace_all(colnames(main_data), j_elements, "j_elements")
      }

      ### If no columns are specified, relabel columns 1 and 2 as `i_elements` and `j_elements`, respectively
      if (is.null(i_elements)) {
        colnames(main_data)[[1]] <- "i_elements"
      }

      if (is.null(j_elements)) {
        colnames(main_data)[[2]] <- "j_elements"
      }

    } else {
      # Make ad hoc column names
      num_columns <- ncol(main_data)
      if (num_columns > 2) {
        column_names <- c("i_elements", "j_elements",
                          paste("var", ((3:num_columns)-2), sep = "_"))
      } else {
        column_names <- c("i_elements", "j_elements")
      }

      colnames(main_data) <- column_names
    }

    # Generate nodelist if not given one
    if (is.null(nodelist)) {

      nodes <- data.frame(id = unique(c(main_data$i_elements, this_el$j_elements)))
      assign(x = paste(net_name, "nodelist", sep = "_"), value = nodes, envir = .GlobalEnv)

    }


    # Assign to global environment
    assign(x = paste(net_name, "edgelist", sep = "_"), value = main_data, envir = .GlobalEnv)


    ######### ADJMAT

  } else if (format == "adjacency_matrix" | format == "adjmat") {

    # Replace missing values with `NA`
    main_data[main_data == missing_code] <- NA

    if (col_names == FALSE) {
      colnames(main_data) <- 1:ncol(main_data)
    }

    if (row_names == TRUE) {
      colnames(main_data)[[1]] <- "i_elements"
    } else {
      main_data$i_elements <- 1:nrow(main_data)
      main_data <- dplyr::select(main_data, i_elements, dplyr::everything())
    }

    full_el <- tidyr::pivot_longer(main_data, 2:ncol(main_data), names_to = "j_elements", values_to = "weight")
    this_el <- full_el[full_el$weight != 0,]
    this_el <- this_el[!is.na(this_el$weight), ]

    # Check matrix dimensions
    main_data <- main_data[,which(names(main_data) != "i_elements")]

    # If not a square matrix, treat as bipartite
    if (nrow(main_data) != ncol(main_data)) {
      base::message("Number of rows and columns in adjacency matrix do not match. Adjacency matrix will be treated as a bipartite network.")
      this_el$i_elements <- paste(this_el$i_elements, "1", sep = "_")
      this_el$j_elements <- paste(this_el$j_elements, "2", sep = "_")
      full_el$i_elements <- paste(full_el$i_elements, "1", sep = "_")
      full_el$j_elements <- paste(full_el$j_elements, "2", sep = "_")
    }


    # Convert to numerics where available
    for (i in 1:ncol(this_el)) {

      this_col <- unlist(this_el[,i])
      this_full <- unlist(full_el[,i])
      if (can.be.numeric(this_col)) {
        this_el[,i] <- as.numeric(this_col)
        full_el[,i] <- as.numeric(this_full)
      }

    }

    # Generate nodelist if not given one
    if (is.null(nodelist)) {

      nodes <- data.frame(id = unique(c(full_el$i_elements, full_el$j_elements)))
      assign(x = paste(net_name, "nodelist", sep = "_"), value = nodes, envir = .GlobalEnv)

    }

    # Assign edgelist to global environment
    assign(x = paste(net_name, "edgelist", sep = "_"), value = this_el, envir = .GlobalEnv)


    ######## ADJLIST

  } else if (format == "adjacency_list" | format == "adjlist" | format == "nodelist") {

    # This should be easy. Just loop across columns and store as edgelist
    # Make a dataframe for storing
    this_el <- data.frame(i_elements = "remove_this_entry",
                          j_elements = "remove_this_entry")

    # Loop across columns
    for (i in 2:ncol(main_data)) {

      temp_el <- data.frame(i_elements = main_data[,1],
                            j_elements = main_data[,i])
      colnames(temp_el) <- c("i_elements", "j_elements")
      this_el <- rbind(this_el, temp_el)
    }

    # Remove that first row
    this_el <- this_el[2:nrow(this_el),]
    # Remove rows where there are missing or blank values
    this_el <- this_el[(this_el$j_elements != missing_code), ]
    this_el <- this_el[(this_el$j_elements != ""), ]
    this_el <- this_el[!is.na(this_el$j_elements),]

    # Convert to numerics where available
    for (i in 1:ncol(this_el)) {

      this_col <- unlist(this_el[,i])
      this_full <- unlist(full_el[,i])
      if (can.be.numeric(this_col)) {
        this_el[,i] <- as.numeric(this_col)
        full_el[,i] <- as.numeric(this_full)
      }

    }


    # Sort order of edgelist
    this_el <- this_el[order(this_el$i_elements),]

    # Generate nodelist if not given one
    if (is.null(nodelist)) {

      nodes <- data.frame(id = unique(c(this_el$i_elements, this_el$j_elements)))
      assign(x = paste(net_name, "nodelist", sep = "_"), value = nodes, envir = .GlobalEnv)

    }

    # Assign edgelist to global environment
    assign(x = paste(net_name, "edgelist", sep = "_"), value = this_el, envir = .GlobalEnv)


  } else {

    base::message("Error: Incorrect format entered.")

  }

}

# Excel
netread_excel <- function(path,
                          nodelist = NULL,
                          sheet = NULL,
                          node_sheet = NULL,
                          col_names = TRUE,
                          row_names = FALSE,
                          format = "edgelist",
                          i_elements = NULL,
                          j_elements = NULL,
                          net_name = "network",
                          missing_code = 99999) {

  # Is this an `xls` or `xlsx` file
  if (stringr::str_detect(path, "xlsx$")) {
    main_data <- readxl::read_xlsx(path = path,
                                   sheet = sheet,
                                   col_names = col_names)
  } else {
    main_data <- readxl::read_xls(path = path,
                                  sheet = sheet,
                                  col_names = col_names)
  }

  # Read in nodelist if applicable
  if (!is.null(nodelist)) {

    if (stringr::str_detect(nodelist, "xlsx$")) {
      nodes <- readxl::read_xlsx(path = nodelist,
                                 sheet = node_sheet,
                                 col_names = col_names)
    } else {
      nodes <- readxl::read_xls(path = nodelist,
                                sheet = node_sheet,
                                col_names = col_names)
    }

    assign(x = paste(net_name, "nodelist", sep = "_"), value = nodes, envir = .GlobalEnv)

  }


  ########## Edgelist handing
  if (format == "edgelist") {

    if (col_names == TRUE) {

      ### If names of i and j columns specified, relabel
      if (!is.null(i_elements)) {
        colnames(main_data) <- stringr::str_replace_all(colnames(main_data), i_elements, "i_elements")
      }

      if (!is.null(j_elements)) {
        colnames(main_data) <- stringr::str_replace_all(colnames(main_data), j_elements, "j_elements")
      }

      ### If no columns are specified, relabel columns 1 and 2 as `i_elements` and `j_elements`, respectively
      if (is.null(i_elements)) {
        colnames(main_data)[[1]] <- "i_elements"
      }

      if (is.null(j_elements)) {
        colnames(main_data)[[2]] <- "j_elements"
      }

    } else {
      # Make ad hoc column names
      num_columns <- ncol(main_data)
      if (num_columns > 2) {
        column_names <- c("i_elements", "j_elements",
                          paste("var", ((3:num_columns)-2), sep = "_"))
      } else {
        column_names <- c("i_elements", "j_elements")
      }

      colnames(main_data) <- column_names
    }

    # Generate nodelist if not given one
    if (is.null(nodelist)) {

      nodes <- data.frame(id = unique(c(main_data$i_elements, main_data$j_elements)))
      assign(x = paste(net_name, "nodelist", sep = "_"), value = nodes, envir = .GlobalEnv)

    }


    # Assign to global environment
    assign(x = paste(net_name, "edgelist", sep = "_"), value = main_data, envir = .GlobalEnv)


    ######### ADJMAT

  } else if (format == "adjacency_matrix" | format == "adjmat") {

    # Replace missing values with `NA`
    main_data[main_data == missing_code] <- NA

    if (col_names == FALSE) {
      colnames(main_data) <- 1:ncol(main_data)
    }

    if (row_names == TRUE) {
      colnames(main_data)[[1]] <- "i_elements"
    } else {
      main_data$i_elements <- 1:nrow(main_data)
      main_data <- dplyr::select(main_data, i_elements, dplyr::everything())
    }

    full_el <- tidyr::pivot_longer(main_data, 2:ncol(main_data), names_to = "j_elements", values_to = "weight")
    this_el <- full_el[full_el$weight != 0,]
    this_el <- this_el[!is.na(this_el$weight), ]

    # Check matrix dimensions
    main_data <- main_data[,which(names(main_data) != "i_elements")]

    # If not a square matrix, treat as bipartite
    if (nrow(main_data) != ncol(main_data)) {
      base::message("Number of rows and columns in adjacency matrix do not match. Adjacency matrix will be treated as a bipartite network.")
      this_el$i_elements <- paste(this_el$i_elements, "1", sep = "_")
      this_el$j_elements <- paste(this_el$j_elements, "2", sep = "_")
      full_el$i_elements <- paste(full_el$i_elements, "1", sep = "_")
      full_el$j_elements <- paste(full_el$j_elements, "2", sep = "_")
    }


    # Convert to numerics where available
    for (i in 1:ncol(this_el)) {

      this_col <- unlist(this_el[,i])
      this_full <- unlist(full_el[,i])
      if (can.be.numeric(this_col)) {
        this_el[,i] <- as.numeric(this_col)
        full_el[,i] <- as.numeric(this_full)
      }

    }

    # Generate nodelist if not given one
    if (is.null(nodelist)) {

      nodes <- data.frame(id = unique(c(full_el$i_elements, full_el$j_elements)))
      assign(x = paste(net_name, "nodelist", sep = "_"), value = nodes, envir = .GlobalEnv)

    }

    # Assign edgelist to global environment
    assign(x = paste(net_name, "edgelist", sep = "_"), value = this_el, envir = .GlobalEnv)


    ######## ADJLIST

  } else if (format == "adjacency_list" | format == "adjlist" | format == "nodelist") {

    # This should be easy. Just loop across columns and store as edgelist
    # Make a dataframe for storing
    this_el <- data.frame(i_elements = "remove_this_entry",
                          j_elements = "remove_this_entry")

    # Loop across columns
    for (i in 2:ncol(main_data)) {

      temp_el <- data.frame(i_elements = main_data[,1],
                            j_elements = main_data[,i])
      colnames(temp_el) <- c("i_elements", "j_elements")
      this_el <- rbind(this_el, temp_el)
    }

    # Remove that first row
    this_el <- this_el[2:nrow(this_el),]
    # Remove rows where there are missing or blank values
    this_el <- this_el[(this_el$j_elements != missing_code), ]
    this_el <- this_el[(this_el$j_elements != ""), ]
    this_el <- this_el[!is.na(this_el$j_elements),]

    # Convert to numerics where available
    for (i in 1:ncol(this_el)) {

      this_col <- unlist(this_el[,i])
      this_full <- unlist(full_el[,i])
      if (can.be.numeric(this_col)) {
        this_el[,i] <- as.numeric(this_col)
        full_el[,i] <- as.numeric(this_full)
      }

    }


    # Sort order of edgelist
    this_el <- this_el[order(this_el$i_elements),]

    # Generate nodelist if not given one
    if (is.null(nodelist)) {

      nodes <- data.frame(id = unique(c(this_el$i_elements, this_el$j_elements)))
      assign(x = paste(net_name, "nodelist", sep = "_"), value = nodes, envir = .GlobalEnv)

    }

    # Assign edgelist to global environment
    assign(x = paste(net_name, "edgelist", sep = "_"), value = this_el, envir = .GlobalEnv)


  } else {

    base::message("Error: Incorrect format entered.")

  }

}


# igraph
netread_igraph <- function(object,
                           net_name = "network") {

  igraph_extract <- igraph::as_data_frame(object, what = "both")
  edges <- igraph_extract$edges %>% rename(i_elements = from,
                                           j_elements = to)
  nodes <- igraph_extract$vertices

  assign(x = paste(net_name, "edgelist", sep = "_"), value = edges, envir = .GlobalEnv)
  assign(x = paste(net_name, "nodelist", sep = "_"), value = nodes, envir = .GlobalEnv)

}

# network/sna
netread_sna <- function(object,
                        net_name = "network") {

  edges <- network::as.data.frame.network(object, unit = "edges") %>%
    rename(i_elements = .tail,
           j_elements = .head)
  nodes <- network::as.data.frame.network(object, unit = "vertices")

  assign(x = paste(net_name, "edgelist", sep = "_"), value = edges, envir = .GlobalEnv)
  assign(x = paste(net_name, "nodelist", sep = "_"), value = nodes, envir = .GlobalEnv)

}

# Pajek

# Reading in Network
netread_pajek <- function(path, net_name = "network") {
  # Pulling-In Network File
  net <- readLines(path)

  # Scanning for Lines with *
  astrisk_lines <- vector("logical", length(net))
  for(i in seq_along(astrisk_lines)){
    astrisk_lines[[i]] <- grepl("*", net[[i]], fixed = TRUE)
  }
  star_index <- cbind(as.data.frame(seq(1, length(astrisk_lines), 1)), astrisk_lines)
  colnames(star_index)[[1]] <- c("Obs_ID")

  # Creating Cut Points
  cut_points <- star_index[(star_index[,2] == TRUE),]
  if(nrow(cut_points) > 2){
    types_index <- cut_points$Obs_ID
    cut_points <- cut_points[[1]]
    end_points <- cut_points[-c(1)]
    end_points <- end_points - 1
    end_points <- c(end_points, length(net))
    cut_points <- cbind(as.data.frame(cut_points), end_points)
    cut_points[,1] <- cut_points[,1] + 1
  }else{
    types_index <- cut_points$Obs_ID
    cut_points <- cut_points[[1]]
    start_points <- cut_points + 1
    end_points <- c(cut_points[[2]] - 1, length(net))
    cut_points <- data.frame(cut_points = start_points, end_points = end_points)
  }

  # Getting Types
  types <- vector('character', length(types_index))
  for(i in seq_along(types)){
    types[[i]] <- strsplit(net[types_index[[i]]], ' ')[[1]][[1]]
  }
  cut_points$type <- types

  # Extracting the nodes list from the network file
  nodes_list <- vector('list', length(types[(types == "*Vertices")]))
  node_cut_points <- cut_points[(cut_points[,3] == "*Vertices"),]
  for(i in seq_along(nodes_list)){
    # Isolating node cut-points
    vertices_cut_points <- node_cut_points[i,]

    # Isolating Nodes Information
    nodes_list[[i]] <- net[c(vertices_cut_points[[1]]:vertices_cut_points[[2]])]
    nodes_list[[i]] <- trim(nodes_list[[i]])

    # Splitting Values Out
    nodes_list[[i]] <- strsplit(as.character(nodes_list[[i]]),'n/')
    for(j in seq_along(nodes_list[[i]])){
      element <- strsplit(as.character(nodes_list[[i]][[j]]),'"')[[1]]
      if(length(element) > 2) {
        supplemental_data <- strsplit(element[[3]],' ')[[1]]
        supplemental_data <- supplemental_data[supplemental_data != ""]
        element <- c(element[[1]], element[[2]], supplemental_data)
        rm(supplemental_data)
      } else{
        if(length(element) < 2){
          element <- strsplit(as.character(nodes_list[[i]][[j]]),' ')[[1]]
          element <- element[element != ""]
        }else{
          element <- element
        }
      }

      nodes_list[[i]][[j]] <- element
    }

    # Cleaning-Up Split
    nodes_list[[i]] <- lapply(nodes_list[[i]], function(x) x[x != ""])

    # Isolating Shape Information
    shapes <- nodes_list[[i]]
    for (j in seq_along(shapes)){
      shapes[[j]] <- nodes_list[[i]][[j]][-c(1:5)]
    }

    shapes <- lapply(shapes, function(x) paste(x,collapse=" "))

    # Transforming Nodes into a Matrix
    for (j in seq_along(nodes_list[[i]])){
      nodes_list[[i]][[j]] <- nodes_list[[i]][[j]][1:5]
    }

    nodes_list[[i]] <-  as.data.frame(matrix(unlist(nodes_list[[i]]), nrow = length(nodes_list[[i]]), byrow = TRUE), stringsAsFactors = FALSE)
    colnames(nodes_list[[i]]) <- c('ID', 'Label', 'x-coord', 'y-coord', 'z-coord')

    # Transforming Shape Data into a Matrix
    shapes <-  as.data.frame(matrix(unlist(shapes), nrow = length(shapes), byrow = TRUE),  stringsAsFactors = FALSE)
    colnames(shapes) <- c('shapes information')

    # Binding Shape Data to Nodes List
    nodes_list[[i]] <- cbind(nodes_list[[i]], shapes)

    # Removing shape information if there no information
    if(sum(nodes_list[[i]]$`shapes information` == "") > 1){
      nodes_list[[i]] <- nodes_list[[i]][-c(ncol(nodes_list[[i]]))]
    }else{
      nodes_list[[i]] <- nodes_list[[i]][,]
    }
    rm(shapes, j)
  }

  # Creating Edges File
  edges_list <- vector('list', length(types[(types != "*Vertices")]))
  edge_cut_points <- cut_points[(cut_points[,3] != "*Vertices"),]
  for(i in seq_along(edges_list)){
    # Isolating edge cut-points
    tie_cut_points <- edge_cut_points[i,]

    # Isolating Nodes Information
    edges_list[[i]] <- net[c(tie_cut_points[[1]]:tie_cut_points[[2]])]
    edges_list[[i]] <- trim(edges_list[[i]])

    # Splitting Values Out
    edges_list[[i]] <- strsplit(as.character(edges_list[[i]]),'n/')

    # Checking for Stem-Leaf Format
    edge_element_strings <- lapply(edges_list[[i]], function(x) strsplit(as.character(x),' ')[[1]])
    edge_element_strings <- lapply(edge_element_strings, function(x) x[x != ""])
    edge_element_lengths <- lapply(edge_element_strings, function(x) length(x))
    edge_element_lengths <- unique(as.integer(edge_element_lengths))

    if(length(edge_element_lengths) > 1){
      # Splitting by Element to Account for Stem-Leaf Notation
      ties__list <- vector('list', length(edges_list[[i]]))
      for(j in seq_along(edges_list[[i]])){
        # Identify Stem & Leaves
        stem_leaves <- base::strsplit(edges_list[[i]][[j]], ' ')[[1]]
        stem <- stem_leaves[[1]]
        leaves <- stem_leaves[2:length(stem_leaves)]

        # Transforming into DataFrame
        stem <- rep(stem, length(leaves))
        weights <- rep(1, length(leaves))
        ties__list[[j]] <- data.frame(person_j = stem, person_i = leaves, weight = weights)
      }

      # Binding List into a Single DataFrame
      edges_list[[i]] <- do.call('rbind', ties__list)
      colnames(edges_list[[i]])[c(1:3)] <- c('i_elements', 'j_elements', 'weight')

      # Adding Type Type information
      tie_type <- tie_cut_points[[3]]
      edges_list[[i]]$`tie_type` <- tie_type
    }else{
      # Splitting Values Out
      edges_list[[i]] <- strsplit(as.character(edges_list[[i]]),' ')

      # Cleaning-Up Split
      edges_list[[i]] <- lapply(edges_list[[i]], function(x) x[x != ""])

      # Transforming into a Matrix
      edges_list[[i]] <-  as.data.frame(matrix(unlist(edges_list[[i]]), nrow = length(edges_list[[i]]), byrow = TRUE), stringsAsFactors = FALSE)
      colnames(edges_list[[i]])[c(1:3)] <- c('i_elements', 'j_elements', 'weight')

      # Removing edge color information as it adds little value when importing data
      edges_list[[i]] <- edges_list[[i]][-c(4:5)]

      # Adding Type Type information
      tie_type <- tie_cut_points[[3]]
      edges_list[[i]]$`tie_type` <- tie_type
    }
  }

  # Collapsing Nodes List & Writing-Out Vertices File
  nodes <- do.call("rbind", nodes_list)
  if(length(nodes_list) > 1){
    # Getting length necessary for ID vectors
    node_lengths <- lapply(nodes_list, function(x) nrow(x))

    # Creating ID vectors
    id_vectors <- vector('list', length(node_lengths))
    for (i in seq_along(node_lengths)){
      id_vectors[[i]] <- rep(i, node_lengths[[i]])
    }

    # Stacking IDs into a Common Vector
    id_vector <- unlist(id_vectors)

    # Adding ID vector
    nodes$data_id <- id_vector
  }else{
    nodes <- nodes
  }

  # Defining Type Correction Function
  type_setter <- function(data_type){
    for(i in seq_along(colnames(data_type))){
      # Checking if Numeric
      if(sum(grepl("[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+", data_type[[i]])) == length(data_type[[i]])){
        # Converting Column to Numeric
        data_type[[i]] <- as.numeric(data_type[[i]])

        # Checking if Integer
        if(sum(!is.na(as.numeric(data_type[[i]]))) == length(data_type[[i]])){
          data_type[[i]] <- as.integer(data_type[[i]])
        }else{
          data_type[[i]] <- data_type[[i]]
        }
      }else{
        # Keeping Type
        data_type[[i]] <- data_type[[i]]
      }
    }

    return(data_type)
  }

  # Fixing Node File Types
  nodes <- type_setter(nodes)

  # Outputting Vertices
  vertices <-  assign(x = paste(net_name, "nodelist", sep = "_"), value = nodes,.GlobalEnv)

  # Collapsing Edges List & Writing-Out Edges File
  edges <- do.call("rbind", edges_list)
  if(length(edges_list) > 1){
    # Getting length necessary for ID vectors
    edge_lengths <- lapply(edges_list, function(x) nrow(x))

    # Creating ID vectors
    id_vectors <- vector('list', length(edge_lengths))
    for (i in seq_along(edge_lengths)){
      id_vectors[[i]] <- rep(i, edge_lengths[[i]])
    }

    # Stacking IDs into a Common Vector
    id_vector <- unlist(id_vectors)

    # Adding ID vector
    edges$data_id <- id_vector
  }else{
    edges <- edges
  }

  # Fixing Node File Types
  edges <- type_setter(edges)

  # Outputting Edges
  ties <-  assign(x = paste(net_name, "edgelist", sep = "_"), value = edges,.GlobalEnv)
}


# UCINet (Non-Binary)
netread_ucinet <- function(path, net_name = "network") {


### Edgelist1
x <- read.table(file = path, sep = "\t")
x$lower <- stringr::str_to_lower(x[,1])
# Remove any quotation marks and similar characters
x$lower <- stringr::str_replace_all(x$lower, "\"", " ")
x$lower <- stringr::str_replace_all(x$lower, "\'", " ")
x$V1 <- stringr::str_replace_all(x$V1, "\"", " ")
x$V1 <- stringr::str_replace_all(x$V1, "\'", " ")
x$break_point <- stringr::str_detect(x$lower, ":") | stringr::str_detect(x$lower, "\\bdl\\b")

break_points <- c(which(x$break_point == T), (nrow(x)+1))

uci_list <- list()
uci_list_names <- character()

for (i in 1:(length(break_points)-1)) {
  this_df <- x[break_points[[i]]:(break_points[[i+1]]-1), ]

  if (stringr::str_detect(this_df$lower[[1]], "\\bdl\\b")) {

    uci_list[[i]] <- this_df
    uci_list_names <- c(uci_list_names, "metadata")

  } else {

    uci_list[[i]] <- this_df[2:nrow(this_df), ]
    this_name <- this_df[1,]$lower
    this_name <- str_replace(this_name, ":", "")
    uci_list_names <- c(uci_list_names, this_name)

  }

}

# Correct spelling of `lables`, if applicable
uci_list_names <- str_replace_all(uci_list_names, "lable", "label")
# Rename "column" to "col" if applicable
uci_list_names <- str_replace_all(uci_list_names, "column", "col")
names(uci_list) <- uci_list_names

# Parsing metadata
### Number of nodes
uci_list$metadata$num_nodes <- stringr::str_detect(uci_list$metadata$lower, "n\\s*=\\s*[0-9]+")
##### If `n` value exists, parse
if (sum(uci_list$metadata$num_nodes) > 0) {
###### Get row with n value
node_row <- uci_list$metadata[uci_list$metadata$num_nodes == T,]
###### Extract the `n = [number]` expression
num_nodes <- unlist(stringr::str_extract_all(node_row$lower, "n\\s*=\\s*[0-9]+"))
###### Extract the number in this expression and make numeric
num_nodes <- as.numeric(stringr::str_extract(num_nodes, "[0-9]+"))
}

#### Number of matrices contained in data
uci_list$metadata$num_mat <- (stringr::str_detect(uci_list$metadata$lower, "nmat\\s*=\\s*[0-9]+")) | (stringr::str_detect(uci_list$metadata$lower, "nmat\\s*=\\s*[0-9]+")) | (stringr::str_detect(uci_list$metadata$lower, "nm\\s*=\\s*[0-9]+"))
###### If an `nmat` or `nm` value exists, parse
if (sum(uci_list$metadata$num_mat) > 0) {
###### Get row with nmat value
mat_row <- uci_list$metadata[uci_list$metadata$num_mat == T, ]
###### Extract the `nmat = [number]` expression
num_mat <- unlist(stringr::str_extract_all(mat_row$lower, c("nmat\\s*=\\s*[0-9]+", "nm\\s*=\\s*[0-9]+")))
###### Extract the number in this expression and make numeric
num_mat <- as.numeric(stringr::str_extract(num_mat, "[0-9]+"))
}

###### Number of rows
uci_list$metadata$num_rows <- stringr::str_detect(uci_list$metadata$lower, "nr\\s*=\\s*[0-9]+")
###### If `nr` value exists, parse
if (sum(uci_list$metadata$num_rows) > 0) {
###### Get row with `nr` value
nr_row <- uci_list$metadata[uci_list$metadata$num_rows == T, ]
###### Extract the `nr = [number]` expression
num_rows <- unlist(stringr::str_extract_all(nr_row$lower, "nr\\s*=\\s*[0-9]+"))
###### Extract thhee number in this expression and make numeric
num_rows <- as.numeric(stringr::str_extract(num_rows, "[0-9]+"))
}


###### Number of rows
uci_list$metadata$num_cols <- stringr::str_detect(uci_list$metadata$lower, "nc\\s*=\\s*[0-9]+")
###### If `nr` value exists, parse
if (sum(uci_list$metadata$num_cols) > 0) {
  ###### Get row with `nr` value
  nc_row <- uci_list$metadata[uci_list$metadata$num_cols == T, ]
  ###### Extract the `nr = [number]` expression
  num_cols <- unlist(stringr::str_extract_all(nc_row$lower, "nc\\s*=\\s*[0-9]+"))
  ###### Extract thhee number in this expression and make numeric
  num_cols <- as.numeric(stringr::str_extract(num_cols, "[0-9]+"))
}

###### Data format
uci_list$metadata$format <- stringr::str_detect(uci_list$metadata$lower, "format")
format_row <- uci_list$metadata[uci_list$metadata$format == T, ]
format_val <- format_row$lower[stringr::str_detect(format_row$lower, "format")]
format <- c("edgelist1", "edgelist2", "fullmatrix", "nodelist1", "nodelist2")[str_detect(format_val, c("edgelist1", "edgelist2", "fullmatrix", "nodelist1", "nodelist2"))]


####### Compile metadata into new list and update `metadata` in `uci_list`
metadata <- list(format = format)

if (sum(uci_list$metadata$num_nodes) > 0) {
  metadata$num_nodes <- num_nodes
}

if (sum(uci_list$metadata$num_mat) > 0) {
  metadata$num_mat <- num_mat
}

if (sum(uci_list$metadata$num_rows) > 0) {
  metadata$num_rows <- num_rows
}

if (sum(uci_list$metadata$num_cols) > 0) {
  metadata$num_cols <- num_cols
}

uci_list$metadata <- metadata


# Properly extracting row, column, and matrix labels
for (i in 1:length(uci_list)) {

  if ((stringr::str_detect(names(uci_list)[[i]], "label") | stringr::str_detect(names(uci_list)[[i]], "lable")) == TRUE) {

    these_labels <- uci_list[[i]]$V1
    uci_list[[i]] <- these_labels

  }
}

# Converting data element into netwrite-compatible edgelists

if (uci_list$metadata$format == "edgelist1" | uci_list$metadata$format == "edgelist2") {

  uci_list$data$V1 <- stringr::str_trim(uci_list$data$V1)
  data_df <- do.call(rbind.data.frame, strsplit(uci_list$data$V1, " +"))

  if (ncol(data_df) == 2) {
    colnames(data_df) <- c("i_elements", "j_elements")
  } else {
    colnames(data_df) <- c("i_elements", "j_elements", paste("val", (3:ncol(data_df) - 2), sep = ""))
  }


# In the event that some columns in the edgelist are numeric values,
  # be sure to convert into a numeric format
  data_df <- as.data.frame(lapply(data_df, function(col) {
    if (can.be.numeric(col)) {
      as.numeric(col)
    } else {
      col
    }
  }))

# If more than one matrix exists in the data file, be sure to
# label edgelist rows to indicate which matrix it belongs to

if ("num_mat" %in% names(uci_list$metadata)) {

  # Make placeholder values for storing matrix identifiers
  data_df$mat <- NA
  # If this data file has matrix labels, add another column to store these labels
  if ("matrix labels" %in% names(uci_list)) {
    data_df$mat_label <- NA
  }
  # Make row number indicator
  data_df$row_number <- 1:nrow(data_df)

  # Need to identify index points for assigning matrix labels
  index_points <- c(0, which(data_df$i_elements == "!"), nrow(data_df)+1)

  for (i in 1:(length(index_points)-1)) {

    data_df$mat <- ifelse((data_df$row_number > index_points[[i]] & data_df$row_number < index_points[[i+1]]),
                          i,
                          data_df$mat)

   # Assign matrix label if available
   if ("matrix labels" %in% names(uci_list)) {
     data_df$mat_label <- ifelse((data_df$row_number > index_points[[i]] & data_df$row_number < index_points[[i+1]]),
                           uci_list$`matrix labels`[[i]],
                           data_df$mat_label)

   }


  }

  # Remove row number column, no longer needed
  data_df$row_number <- NULL

  # Remove rows indicating cutoff points
  data_df <- data_df[which(data_df$i_elements != "!"),]

}

 # Assign reformatted edgelist into `data` element of `uci_list`
  uci_list$data <- data_df


  # If dataset contains labels, add columns to indicate
  if ("labels" %in% names(uci_list)) {
    i_labels <- data.frame(i_elements = 1:max(data_df$i_elements),
                           i_label = stringr::str_squish(uci_list$`labels`))
    j_labels <- data.frame(j_elements = 1:max(data_df$j_elements),
                           j_label = stringr::str_squish(uci_list$`labels`))

    data_df <- dplyr::left_join(data_df, i_labels, by = "i_elements")
    data_df <- dplyr::left_join(data_df, j_labels, by = "j_elements")

  }

  # If row labels exist, add to edgelist
  if ("row labels" %in% names(uci_list)) {
    i_labels <- data.frame(i_elements = 1:max(data_df$i_elements),
                           i_label = stringr::str_squish(uci_list$`row labels`))

    data_df <- dplyr::left_join(data_df, i_labels, by = "i_elements")

  }

  # If col labels exist, add to edgelist
  if ("col labels" %in% names(uci_list)) {
    j_labels <- data.frame(j_elements = 1:max(data_df$j_elements),
                           j_label = stringr::str_squish(uci_list$`col labels`))

    data_df <- dplyr::left_join(data_df, j_labels, by = "j_elements")

  }


# The `edgelist2` format is designed to handle rectangular matrices and
# two-mode networks. Because of this, items in `i_elements` and `j_elements`
# should be distinct from one another

  if (uci_list$metadata$format == "edgelist2") {

    data_df$i_elements <- paste(data_df$i_elements, "1", sep = "_")
    data_df$j_elements <- paste(data_df$j_elements, "2", sep = "_")

  }


assign(x = paste(net_name, "edgelist", sep = "_"), value = data_df, envir = .GlobalEnv)

#################### FULLMATRIX

} else if (uci_list$metadata$format == "fullmatrix") {

  # Remove any leading white space
  uci_list$data$V1 <- stringr::str_trim(uci_list$data$V1)

  data_df <- do.call(rbind.data.frame, strsplit(uci_list$data$V1, " +"))


  # If level labels are embedded in the data section, it creates some extra and unnecessary rows.
  # Let's go ahead and take those rows out before proceeing
  if (nrow(data_df) %% uci_list$metadata$num_nodes != 0) {

  data_df <- as.data.frame(t(data_df))
  #data_df <- data_df[2:nrow(data_df), ]

  data_df <- as.data.frame(lapply(data_df, function(col) {
    if (can.be.numeric(col)) {
      as.numeric(col)
    } else {
      col
    }
  }))

  # Remove any non-numeric columns
  data_df <- data_df[, unlist(lapply(data_df, is.numeric))]

  data_df <- as.data.frame(t(data_df))

  }

  # In the event that some columns in the edgelist are numeric values,
  # be sure to convert into a numeric format
  data_df <- as.data.frame(lapply(data_df, function(col) {
    if (can.be.numeric(col)) {
      as.numeric(col)
    } else {
      col
    }
  }))

  # Remove any non-numeric columns
  data_df <- data_df[, unlist(lapply(data_df, is.numeric))]

  # Initial relabeling of rows and columns
  rownames(data_df) <- as.character(1:nrow(data_df))
  colnames(data_df) <- as.character(1:ncol(data_df))

  # If there are column labels, append these:
  if ("col labels" %in% names(uci_list)) {
    colnames(data_df) <- uci_list$`col labels`[1:uci_list$metadata$num_nodes]
  }

# Handling if multiple matrices are contained in data file
if ("num_mat" %in% names(uci_list$metadata)) {

  if (uci_list$metadata$num_mat > 1) {

  data_df$mat <- rep(1:uci_list$metadata$num_mat, each = uci_list$metadata$num_nodes)

  # If level labels are included, will also need to append these
  if ("level labels" %in% names(uci_list)) {
    data_df$lvl_label <- rep(uci_list$`level labels`[1:uci_list$metadata$num_mat], each = uci_list$metadata$num_nodes)
  }

  # If level labels are included, will also need to append these
  if ("matrix labels" %in% names(uci_list)) {
    data_df$mat_label <- rep(uci_list$`matrix labels`[1:uci_list$metadata$num_mat], each = uci_list$metadata$num_nodes)
  }

  # If row labels are included, will also need to append these:
  if ("row labels" %in% names(uci_list)) {
    data_df$i_elements <- rep(uci_list$`row labels`[1:uci_list$metadata$num_nodes], uci_list$metadata$num_mat)
  } else {
    data_df$i_elements <- rep(1:uci_list$metadata$num_nodes, uci_list$metadata$num_mat)
  }

} else {

  # If row labels are included, will also need to append these (single matrix):
  if ("row labels" %in% names(uci_list)) {
    data_df$i_elements <- rep(uci_list$`row labels`[1:uci_list$metadata$num_nodes])
  } else {
    data_df$i_elements <- rownames(data_df)
  }

}

} else {

  # If row labels are included, will also need to append these (single matrix):
  if ("row labels" %in% names(uci_list)) {
    data_df$i_elements <- rep(uci_list$`row labels`[1:uci_list$metadata$num_nodes])
  } else {
    data_df$i_elements <- rownames(data_df)
  }


}

  data_df <- tidyr::pivot_longer(data_df, 1:uci_list$metadata$num_nodes, names_to = "j_elements", values_to = "weight")

  # Remove non-edges (where weight is equal to zero)
  data_df <- data_df[data_df$weight != 0, ]


  assign(x = paste(net_name, "edgelist", sep = "_"), value = data_df, envir = .GlobalEnv)


} else if (uci_list$metadata$format == "nodelist1" | uci_list$metadata$format == "nodelist2") {

  data_df <- do.call(rbind.data.frame, strsplit(uci_list$data$V1, "\\n"))

  data_list <- strsplit(data_df[,1], " +")

  this_edgelist <- data.frame(i_elements = character(),
                              j_elements = character())
  nodelist <- character()

  for (i in 1:length(data_list)) {

    this_item <- data_list[[i]]
    this_item <- this_item[this_item != ""]

      nodelist <- c(nodelist, this_item[1])

      if (length(this_item) > 1) {

        this_row <- data.frame(i_elements = this_item[1],
                               j_elements = this_item[2:length(this_item)])

        this_edgelist <- rbind(this_edgelist, this_row)


      }
  }

  # Remove any `!` values from nodelist
  nodelist <- nodelist[nodelist != "!"]

  # Convert to numeric values if applicable
  if (can.be.numeric(nodelist)) {
    nodelist <- as.numeric(nodelist)
  }

  data_df <- as.data.frame(lapply(this_edgelist, function(col) {
    if (can.be.numeric(col)) {
      as.numeric(col)
    } else {
      col
    }
  }))


# If dataset contains labels, add columns to indicate
  if ("labels" %in% names(uci_list)) {
    i_labels <- data.frame(i_elements = 1:max(data_df$i_elements),
                           i_label = stringr::str_squish(uci_list$`labels`))
    j_labels <- data.frame(j_elements = 1:max(data_df$j_elements),
                           j_label = stringr::str_squish(uci_list$`labels`))

    data_df <- dplyr::left_join(data_df, i_labels, by = "i_elements")
    data_df <- dplyr::left_join(data_df, j_labels, by = "j_elements")

  }

# If row labels exist, add to edgelist
  if ("row labels" %in% names(uci_list)) {
    i_labels <- data.frame(i_elements = 1:max(data_df$i_elements),
                           i_label = stringr::str_squish(uci_list$`row labels`))

    data_df <- dplyr::left_join(data_df, i_labels, by = "i_elements")

  }

  # If col labels exist, add to edgelist
  if ("col labels" %in% names(uci_list)) {
    j_labels <- data.frame(j_elements = 1:max(data_df$j_elements),
                           j_label = stringr::str_squish(uci_list$`col labels`))

    data_df <- dplyr::left_join(data_df, j_labels, by = "j_elements")

  }

# If format is `nodelist2`, it's presumably a two-mode network and should
# be handled that way

  if (uci_list$metadata$format == "nodelist2") {

    data_df$i_elements <- paste(data_df$i_elements, "1", sep = "_")
    data_df$j_elements <- paste(data_df$j_elements, "2", sep = "_")

  }

  assign(x = paste(net_name, "edgelist", sep = "_"), value = data_df, envir = .GlobalEnv)
  assign(x = paste(net_name, "nodelist", sep = "_"), value = nodelist, envir = .GlobalEnv)

} else {

  base::message("Error: Unable to identify UCINet dl format")
}

}


can.be.numeric <- function(x) {
  stopifnot(is.atomic(x) || is.list(x)) # check if x is a vector
  numNAs <- sum(is.na(x))
  numNAs_new <- suppressWarnings(sum(is.na(as.numeric(x))))
  return(numNAs_new == numNAs)
}

# Utilities for Jon's Pajek reader
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Good
# netread_ucinet(file = "./netread_test/base_dl/Camp92_edgelist1.dl",
#                net_name = "camp92_el1")
# netread_ucinet(file = "./netread_test/base_dl/Camp92_edgelist2.dl",
#                net_name = "camp92_el2")
# netread_ucinet(file = "./netread_test/base_dl/Camp92_fullmatrix_nolabel.dl",
#                net_name = "camp92_fullmat_nolabel")
# netread_ucinet(file = "./netread_test/base_dl/random1_type1.dl",
#                net_name = "random1_type1")
# netread_ucinet(file = "./netread_test/base_dl/random1_type3.dl",
#                net_name = "random1_type3")
# netread_ucinet(file = "./netread_test/base_dl/PRISON_edgelist1.dl",
#                net_name = "prison_el1")
# netread_ucinet(file = "./netread_test/base_dl/PRISON_edgelist2.dl",
#                net_name = "prison_el2")
# netread_ucinet(file = "./netread_test/base_dl/PRISON_fullmatrix.dl",
#                net_name = "prison_mat")
# netread_ucinet(file = "./netread_test/base_dl/Camp92_fullmatrix_wlabel.dl",
#                net_name = "camp92_fullmat_label")
# netread_ucinet(file = "./netread_test/base_dl/random1_type2.dl",
#                net_name = "random1_type2")
# netread_ucinet(file = "./netread_test/base_dl/random1_type4.dl",
#                net_name = "random1_type4")








####################################


# Gephi





