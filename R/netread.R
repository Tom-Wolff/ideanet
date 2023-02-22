#######################
#                     #
#    N E T R E A D    #
#                     #
#######################




# CSV

# Excel

# igraph

# network/sna

# Pajek



# file = "./netread_test/base_dl/random1_type2.dl"
# net_name = "campmat_label"



# UCINet (Non-Binary)
netread_ucinet <- function(file, net_name = "network") {


### Edgelist1
x <- read.table(file = file, sep = "\t")
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





