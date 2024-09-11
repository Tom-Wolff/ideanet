#' Reading and Reshaping Network Canvas Data (\code{nc_read})
#'
#' @description The \code{nc_read} function reads in and processes CSV files produced by \href{https://networkcanvas.com/}{Network Canvas}, a popular tool for egocentric data capture. \code{nc_read} produces three dataframes optimized for use with \code{\link{ego_netwrite}}.
#'
#' @param path A character value indicating the directory in which Network Canvas CSVs are located. \code{nc_read} will read in all CSV files located in this directory and process them.
#' @param protocol A character value indicating the pathname of the Network Canvas protocol file corresponding to the data being read. Reading in the protocol is optional but recommended for accurate encoding of categorical variables.
#' @param cat.to.factor A logical value indicating whether categorical variables, originally stored as a series of TRUE/FALSE columns, should be converted into a single factor column.
#'
#' @return \code{nc_read} returns a list containing three items: an ego list, an ego-alter edgelist, and an alter-alter edgelist. If multiple edge types exist for ego-alter and/or alter-alter ties, edgelists for each type of tie will be stored as individual data frames as elements in a list. All data frames are optimized for use with \code{\link{ego_netwrite}}.  \cr \cr
#' Note that in the \code{alters} data frame(s), column \code{node_type} reflects the "node type" assigned to a given alter as specified in a Network Canvas protocol. Values in \code{node_type} are not necessarily those which should be fed into the \code{alter_types} argument in \code{\link{ego_netwrite}}.
#'
#' @export
#'
#' @importFrom rlang .data

nc_read <- function(
    # ARGUMENTS:

  # Path to folder containing exported NC .csv files
  path,

  # Path to Network Canvas Protocol, if available (.netcanvas)
  protocol = NULL,

  # Whether to convert categorical booleans to a factor
  # (i.e. apply Pat's catToFactor function)
  # See lines 192-235 to see how I tried to automatically
  # detect which sets of variables should be treated as
  # booleans corresponding to a single response item
  cat.to.factor = TRUE

) {

  # browser()

  # If user puts a forward slash at the end of the directory path they define,
  # it will cause problems. Check if this happens and correct
  if (stringr::str_detect(path, "/$")) {
    path <- stringr::str_replace(path, "/$", "")
  }


  # Get list of files in directory
  file_list <- list.files(path)
  # Remove non-CSV files in case graphMLs are exported
  just_csv <- file_list[stringr::str_detect(file_list, "csv$")]

  # Compile Ego information
  ego_files <- just_csv[stringr::str_detect(just_csv, "ego.csv$")]

  for (i in 1:length(ego_files)) {
    if (i == 1) {
      egos <- utils::read.csv(paste(path, ego_files[[i]], sep = "/"), header = TRUE)
      egos$networkCanvasCaseID <- as.character(egos$networkCanvasCaseID)
    } else {
      this_ego <- utils::read.csv(paste(path, ego_files[[i]], sep = "/"), header = TRUE)
      this_ego$networkCanvasCaseID <- as.character(this_ego$networkCanvasCaseID)
      egos <- dplyr::bind_rows(egos, this_ego)
    }
  }

  # Compile Alter information
  ### This one is a bit tricky because alter properties for each node type have
  ### their own unique CSV files, and they won't necessarily have the same
  ### column names

  alter_files <- just_csv[stringr::str_detect(just_csv, "attributeList")]

  for (i in 1:length(alter_files)) {
    if (i == 1) {
      alters <- utils::read.csv(paste(path, alter_files[[i]], sep = "/"), header = T)
      # Need to handle the case in which ego 1 is an isolate
      if (nrow(alters) == 0) {
        alters[1,] <- NA
      }
      ### Record type of alter
      node_type <- stringr::str_extract(alter_files[[i]], "attributeList.*.csv")
      node_type <- stringr::str_replace(node_type, "attributeList_", "")
      node_type <- stringr::str_replace(node_type, ".csv", "")
      alters$node_type <- node_type
      alters$data_file <- paste(path, alter_files[[i]], sep = "/")
    } else {
      this_alter <- utils::read.csv(paste(path, alter_files[[i]], sep = "/"), header = T)
      # Only need to do the rest if there are actually alter nominated by ego,
      # otherwise can skip
      if (nrow(this_alter) > 0) {
        ### Record type of alter
        node_type <- stringr::str_extract(alter_files[[i]], "attributeList.*.csv")
        node_type <- stringr::str_replace(node_type, "attributeList_", "")
        node_type <- stringr::str_replace(node_type, ".csv", "")
        this_alter$node_type <- node_type
        this_alter$data_file <- paste(path, alter_files[[i]], sep = "/")
        alters <- dplyr::bind_rows(alters, this_alter)
      }
    }
  }

  # If the first ego was an isolate, go ahead and remove the first row in `alters`
  alters <- alters[!is.na(alters$nodeID), ]



  # Compile Alter-Alter Edgelists

  edge_files <- just_csv[stringr::str_detect(just_csv, "edgeList")]

  for (i in 1:length(edge_files)) {
    if (i == 1) {
      el <- utils::read.csv(paste(path, edge_files[[i]], sep = "/"), header = T)
      # Handling if first ego is an isolate
      if (nrow(el) == 0) {
        el[1,] <- NA
      }

      ### Record type of edge
      edge_type <- stringr::str_extract(edge_files[[i]], "edgeList.*.csv")
      edge_type <- stringr::str_replace(edge_type, "edgeList_", "")
      edge_type <- stringr::str_replace(edge_type, ".csv", "")
      el$edge_type <- edge_type
      el$data_file <- paste(path, edge_files[[i]], sep = "/")
    } else {
      this_el <- utils::read.csv(paste(path, edge_files[[i]], sep = "/"), header = T)

      if (nrow(this_el) == 0) {
        next
      } else {
        ### Record type of edge
        edge_type <- stringr::str_extract(edge_files[[i]], "edgeList.*.csv")
        edge_type <- stringr::str_replace(edge_type, "edgeList_", "")
        edge_type <- stringr::str_replace(edge_type, ".csv", "")
        this_el$edge_type <- edge_type
        this_el$data_file <- paste(path, edge_files[[i]], sep = "/")
        el <- dplyr::bind_rows(el, this_el)
      }
    }
  }

  # If the first ego was an isolate, go ahead and remove the first row in `el`
  el <- el[!is.na(el$edgeID), ]

  # Record if there's actually an alter-alter edgelist on record based on number
  # of rows in `el`

  no_el <- nrow(el) == 0

  # Which variables to keep for each level/type?



  # Since Josh wants to potentially split data into lists, it might be easier to set up
  # Ego IDs, etc. here

  # Converting boolean columns to R logical vectors (Egos)
  egos <- dplyr::mutate_all(egos, to_logical)
  # Convert date columns to POSIXct objects (Egos)
  egos <- dplyr::mutate_all(egos, to_date)

  # Convert session info to POSIXct
  egos$sessionStart <- to_posix(egos$sessionStart)
  egos$sessionFinish <- to_posix(egos$sessionFinish)
  egos$sessionExported <- to_posix(egos$sessionExported)

  # Easier ego ID numbers
  egos <- egos %>%
    dplyr::group_by(.data$networkCanvasEgoUUID) %>%
    dplyr::mutate(ego_id = dplyr::cur_group_id()) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$ego_id, dplyr::everything())

  # Get the basics to merge into other dataframes
  ego_basic <- egos %>%
    dplyr::select(.data$ego_id, .data$networkCanvasEgoUUID)

  alters <- alters %>%
    dplyr::left_join(ego_basic, by = "networkCanvasEgoUUID") %>%
    dplyr::select(.data$ego_id, alter_id = .data$nodeID, .data$node_type, dplyr::everything())

  if (nrow(el) > 0) {

    el <- el %>%
      dplyr::left_join(ego_basic, by = "networkCanvasEgoUUID") %>%
      dplyr::select(.data$ego_id, edge_id = .data$edgeID, .data$from, .data$to, .data$edge_type, dplyr::everything())

  }





  ##### Josh wants unique alter and aa-edge types isolated into their own dataframes
  ##### Need to detect if multiple types exist and then restructure.

  if (length(unique(alters$node_type)) > 1) {

    alters_list <- list()

    for (i in 1:length(unique(alters$node_type))) {

      # Extract alters for this type
      these_alters <- alters %>% dplyr::filter(node_type == unique(alters$node_type)[[i]])

      # Use first CSV file stored in `data_file` column to read in which variables to keep for this type
      keep_cols <- colnames(read.csv(these_alters[[1, "data_file"]]))
      # Finalize list of variables to keep based on earlier processing
      keep_cols <- c("ego_id", "alter_id", "node_type",
                     keep_cols[!keep_cols %in% c("nodeID", "null")])
      # Extract columns to keep
      these_alters <- these_alters[, keep_cols]

      # Converting boolean columns to R logical vectors
      these_alters <- dplyr::mutate_all(these_alters, to_logical)
      # Convert date columns to POSIXct objects
      these_alters <- dplyr::mutate_all(these_alters, to_date)

      # Remove `data_file` column
      these_alters$data_file <- NULL

      # Store in list
      alters_list[[i]] <- these_alters
    }

    names(alters_list) <- unique(alters$node_type)

    alters <- alters_list

  } else {
    # Converting boolean columns to R logical vectors
    alters <- dplyr::mutate_all(alters, to_logical)
    # Convert date columns to POSIXct objects
    alters <- dplyr::mutate_all(alters, to_date)
    # Remove `data_file` column
    alters$data_file <- NULL
  }


  if (length(unique(el$edge_type)) > 1) {

    el_list <- list()

    for (i in 1:length(unique(el$edge_type))) {

      this_el <- el %>% dplyr::filter(edge_type == unique(el$edge_type)[[i]])

      # Use first CSV file stored in `data_file` column to read in which variables to keep for this type
      keep_cols <- colnames(read.csv(this_el[[1, "data_file"]]))
      # Finalize list of variables to keep based on earlier processing
      keep_cols <- c("ego_id", "edge_id", "edge_type",
                     keep_cols[!keep_cols %in% c("edgeID", "null")])
      # Extract columns to keep
      this_el <- this_el[, keep_cols]

      # Converting boolean columns to R logical vectors
      this_el <- dplyr::mutate_all(this_el, to_logical)
      # Convert date columns to POSIXct objects
      this_el <- dplyr::mutate_all(this_el, to_date)

      # Remove `data_file` column
      this_el$data_file <- NULL

      # Store in list
      el_list[[i]] <- this_el

    }

    names(el_list) <- unique(el$edge_type)

    el <- el_list

  } else {

    # Converting boolean columns to R logical vectors
    el <- dplyr::mutate_all(el, to_logical)
    # Convert date columns to POSIXct objects
    el <- dplyr::mutate_all(el, to_date)

    # Remove `data_file` column
    el$data_file <- NULL

  }


  #################################################
  #    N C   P R O T O C O L   C O D E B O O K    #
  #################################################

  if (!is.null(protocol)) {

    # Remove filename from protocol path
    exdir_path <- paste(stringr::str_extract(protocol, ".*\\/"), "nc_unzip/", sep = "")

    # Extract protocol file contents
    unzip(protocol, exdir = exdir_path)

    # Read in JSON
    nc_json <- jsonlite::fromJSON(paste(exdir_path, "protocol.json", sep = ""))

    # Extract codebook
    codebook <- nc_json$codebook

    # Identify categorical variables for each data level
    ego_extract <- codebook_extract(codebook$ego)
    ego_extract$level = "ego"

    if (nrow(ego_extract) > 0) {
      ego_extract2 <- ego_extract %>%
        dplyr::filter(var_type == "categorical")
    } else {
      ego_extract2 <- data.frame()
    }


    node_extract <- dplyr::bind_rows(lapply(codebook$node, codebook_extract))
    node_extract$level <- "node"
    if (nrow(node_extract) > 0) {
      node_extract2 <- node_extract %>%
        dplyr::filter(var_type == "categorical")
    } else {
      node_extract2 <- data.frame()
    }

    edge_extract <- dplyr::bind_rows(lapply(codebook$edge, codebook_extract))
    edge_extract$level <- "edge"
    if (nrow(edge_extract) > 0) {
      edge_extract2 <- edge_extract %>%
        dplyr::filter(var_type == "categorical")
    } else {
      edge_extract2 <- data.frame()
    }


    # If applicable, recode categorical variables (Ego)
    if (nrow(ego_extract2) > 0) {

      # Get unique variables to iterate over
      var_names <- unique(ego_extract2$var_name)

      for (i in 1:length(var_names)) {
        egos[,var_names[[i]]] <- catToFactor(egos, var_names[[i]])
      }
    }


    # If applicable, recode categorical variables (Alters)
    if (nrow(node_extract2) > 0) {

      node_levels <- unique(node_extract2$level_name)

      if (length(node_levels) > 1) {

        for (i in 1:length(node_levels)) {

          level_extract <- node_extract2 %>% dplyr::filter(level_name == node_levels[[i]])
          var_names <- unique(level_extract$var_name)

          for (j in 1:length(var_names)) {
            alters[[node_levels[[i]]]][,var_names[[j]]] <- catToFactor(alters[[node_levels[[i]]]],
                                                                       var_names[[j]])

            ### Remove if recoding doesn't happen
            if (sum(is.na(alters[[node_levels[[i]]]][,var_names[[j]]])) == length(is.na(alters[[node_levels[[i]]]][,var_names[[j]]]))) {
              alters[[node_levels[[i]]]][,var_names[[j]]] <- NULL
            }

          }
        }

      } else {

        # Get unique variables to iterate over
        var_names <- unique(node_extract2$var_name)

        for (i in 1:length(var_names)) {
          alters[,var_names[[i]]] <- catToFactor(alters, var_names[[i]])
        }

      }

    }

    # If applicable, recode categorical variables (Alter-Alter Ties)
    if (nrow(edge_extract2) > 0) {

      edge_levels <- unique(edge_extract2$level_name)

      if (length(edge_levels) > 1) {

        for (i in 1:length(edge_levels)) {

          level_extract <- edge_extract2 %>% dplyr::filter(level_name == edge_levels[[i]])
          var_names <- unique(level_extract$var_name)

          for (j in 1:length(var_names)) {
            el[[edge_levels[[i]]]][,var_names[[j]]] <- catToFactor(el[[edge_levels[[i]]]],
                                                                   var_names[[j]])

            ### Remove if recoding doesn't happen
            if (sum(is.na(el[[edge_levels[[i]]]][,var_names[[j]]])) == length(is.na(el[[edge_levels[[i]]]][,var_names[[j]]]))) {
              el[[edge_levels[[i]]]][,var_names[[j]]] <- NULL
            }

          }
        }

      } else {

        # Get unique variables to iterate over
        var_names <- unique(edge_extract2$var_name)

        for (i in 1:length(var_names)) {
          el[,var_names[[i]]] <- catToFactor(el, var_names[[i]])
        }

      }


    }




  } else {


    ############################################################
    # Apply catToFactor, if desired

    if (cat.to.factor == TRUE) {
      warning(paste0("Network Canvas protocol file not provided. `nc_read` will select categorical variables to recode as factors based on patterns in column names, but accuracy is not guaranteed."))
      egos <- apply_catToFactor(egos)

      if (is.data.frame(alters)) {
        alters <- apply_catToFactor(alters)
      } else {
        alters <- lapply(alters, apply_catToFactor)
      }

      if (is.data.frame(el)) {
        el <- apply_catToFactor(el)
      } else {
        el <- lapply(el, apply_catToFactor)
      }

    }

    # End non-protocol condition
  }
  #
  #   # Converting boolean columns to R logical vectors
  #   egos <- dplyr::mutate_all(egos, to_logical)
  #   alters <- dplyr::mutate_all(alters, to_logical)
  #   el <- dplyr::mutate_all(el, to_logical)
  #
  #   # Convert date columns to POSIXct objects
  #   egos <- dplyr::mutate_all(egos, to_date)
  #   alters <- dplyr::mutate_all(alters, to_date)
  #   el <- dplyr::mutate_all(el, to_date)
  #
  #
  #   # Convert session info to POSIXct
  #   egos$sessionStart <- to_posix(egos$sessionStart)
  #   egos$sessionFinish <- to_posix(egos$sessionFinish)
  #   egos$sessionExported <- to_posix(egos$sessionExported)
  #
  #   # Easier ego ID numbers
  #   egos <- egos %>%
  #     dplyr::group_by(.data$networkCanvasEgoUUID) %>%
  #     dplyr::mutate(ego_id = dplyr::cur_group_id()) %>%
  #     dplyr::ungroup() %>%
  #     dplyr::select(.data$ego_id, dplyr::everything())
  #
  #   # Get the basics to merge into other dataframes
  #   ego_basic <- egos %>%
  #     dplyr::select(.data$ego_id, .data$networkCanvasEgoUUID)
  #
  #   alters <- alters %>%
  #     dplyr::left_join(ego_basic, by = "networkCanvasEgoUUID") %>%
  #     dplyr::select(.data$ego_id, alter_id = .data$nodeID, .data$node_type, dplyr::everything())
  #
  #   if (nrow(el) > 0) {
  #
  #       el <- el %>%
  #         dplyr::left_join(ego_basic, by = "networkCanvasEgoUUID") %>%
  #         dplyr::select(.data$ego_id, edge_id = .data$edgeID, .data$from, .data$to, .data$edge_type, dplyr::everything())
  #
  #   }


  # We'll want to extract the protocol name from the filenames for naming
  # the objects we output to the global environment
  protocol_name <- egos$networkCanvasProtocolName[[1]]

  if (no_el == FALSE) {

    nc_list <- list(egos = egos,
                    alters = alters,
                    alter_edgelists = el)

  } else {

    nc_list <- list(egos = egos,
                    alters = alters)
  }

  return(nc_list)


}





#####################################
# S U P P O R T   F U N C T I O N S #
#####################################

# Pat's `catToFactor` function for cleaning NC data
catToFactor <- function(dataframe,variableName) {
  fullVariableName <- paste0(variableName,"_")
  catVariables <- grep(fullVariableName, names(dataframe), value=TRUE)
  # Check if variable exists
  if (identical(catVariables, character(0))){
    warning(paste0("Cannot find variable named -",variableName,"- in the data. This variable will not be recoded."))
    return(c(NA))
    # Check if "true" in multiple columns of a single row
  } else if (sum(apply(dataframe[,catVariables], 1, function(x) sum(x %in% TRUE)>1))>0) {
    #  stop(paste0("Your variable -",variableName,"  - appears to take multiple values.")) }
    warning(paste0("Your variable -",variableName,"  - appears to take multiple values. This variable will not be recoded."))
    return(c(NA))
  } else {
    catValues <- sub(paste0('^',fullVariableName), '', catVariables)
    factorVariable <- c()
    for(i in 1:length(catVariables)){
      factorVariable[dataframe[catVariables[i]]==TRUE] <- catValues[i]
    }
    return(factor(factorVariable,levels=catValues))
  }
}



# Identifying which variables to which we apply `catToFactor`
# Identifying which variables to which we apply `catToFactor`
apply_catToFactor <- function(df) {
  ### Extract only columns with "true" and "false" codings

  # Store copy of original df
  original_df <- df

  # LIMIT TO ONLY THE T/F VARIABLES FIRST
  df_keep <- c()
  df_keep_na <- c()
  for (i in 1:ncol(df)) {
    df_keep[i] <- sum(!(df[,i] %in% c(TRUE, FALSE, NA))) == 0
    # If we have variables that are technically logicals but only contain `NA`
    # values, we'll want to skip them
    df_keep_na[i] <- sum(!is.na(df[,i])) == 0
  }
  df_keep[df_keep_na] <- FALSE
  df <- df[,df_keep]

  # THEN REMOVE STUFF FOLLOWING THE LAST UNDERSCORE
  ##### Create vector of column names
  df_names <- colnames(df)
  prefixes <- stringr::str_remove(df_names, "_(?:.(?!_))+$")

  for (i in 1:length(prefixes)) {
    this_prefix <- prefixes[i]

    last_prefix <- prefixes[i-1]
    next_prefix <- prefixes[i+1]

    if (i > 1){
      if (stringr::str_detect(last_prefix, paste("^", this_prefix, sep = "")) == TRUE) {
        prefixes[i-1] <- this_prefix
      }}

    if (i < length(prefixes)){
      if (stringr::str_detect(next_prefix, paste("^", this_prefix, sep = "")) == TRUE) {
        prefixes[i+1] <- this_prefix
      }}

  }

  ### 2. Use `rle` to identify consecutive repeats of prefixes
  consec_rep <- rle(prefixes)
  ### 3. Extract prefixes with consecutive repeats
  rep_prefixes <- consec_rep$values[consec_rep$lengths > 1]

  ######### Note, if no variables are found to need recoding, just skip the rest
  if (length(rep_prefixes > 0)) {
    ### 4. Verify that values of these variables only contain "true", "false", or NA
    final_prefixes <- c()
    for (i in 1:length(rep_prefixes)) {
      ##### Pull just the columns beginning with this prefix
      these_cols <- df[, prefixes == rep_prefixes[[i]]]
      ##### Put all values in these columns in a single column
      these_vals <- unname(unlist(these_cols))
      ##### Check if this vector contains any values that aren't "true", "false",
      ##### or `NA`
      if (sum(!(these_vals %in% c(TRUE, FALSE, NA))) == 0) {
        final_prefixes <- c(final_prefixes, rep_prefixes[[i]])
      } else {
        next
      }
    }

    ### 5. Store vector of prefixes to which `catToFactor` will be applied
    ##### We do this where we define `final_prefixes`
    ### 6. Run `catToFactor`
    for (i in 1:length(final_prefixes)) {
      this_var <- catToFactor(df, final_prefixes[i])
      if ((NA %in% this_var) & length(this_var) == 1) {
        next
      } else {
        df_names <- colnames(original_df)
        original_df[,(ncol(original_df)+1)] <- this_var
        colnames(original_df) <- c(df_names, final_prefixes[i])
      }
    }
    return(original_df)
  } else {
    return(original_df)
  }
}

# Function for turning NC booleans into R logicals
to_logical <- function(x) {
  if (sum(!(x %in% c("true", "false", NA, ""))) == 0) {
    x <- as.logical(x)
    return(x)
  } else {
    return(x)
  }
}

# Function for converting dates
to_date <- function(x) {

  # Empty character values should be recoded as `NA`s
  if (is.character(x)) {
    x[x == ""] <- NA
  }

  # Need to handle situation where NAs might be mixed in with NAs
  string_check <- stringr::str_detect(x, "^\\d\\d\\d\\d-\\d\\d-\\d\\d$")
  string_check2 <- stringr::str_detect(x, "^\\d\\d\\d\\d-\\d\\d$")
  # Remove NAs since they mess up the validation process
  check_na <- string_check[!is.na(string_check)]
  check_na2 <- string_check2[!is.na(string_check2)]

  # If variable contains only `NA` values, just return the original vector
  if (length(check_na) == 0 & length(check_na2) == 0) {
    return(x)
  } else {

    # Check if criteria are met and process
    if (sum(check_na) == length(check_na)) {
      x <- as.POSIXct(x, format = "%Y-%m-%d")
      return(x)
    } else if (sum(check_na2) == length(check_na2)) {
      x <- paste(x, "-01", sep = "")
      x <- as.POSIXct(x, format = "%Y-%m-%d")
      return(x)
    } else {
      return(x)
    }
  }
}

# POSIXct converter for session info

to_posix <- function(x) {
  x <- stringr::str_replace_all(x, "T", " ")
  x <- stringr::str_replace_all(x, "Z", "")
  x <- as.POSIXct(x, format = "%Y-%m-%d %H:%M:%OS")
}


# Extract variable information from JSON codebook
var_info <- function(x) {
  this_name <- x$name
  this_type <- x$type
  this_value <- x$value

  if ("options" %in% names(x)) {
    var_vals <- x$options$value
  } else {
    var_vals <- NA
  }

  return(data.frame(var_name = this_name,
                    var_type = this_type,
                    col_name = paste(this_name, var_vals, sep = "_")))

}

# Higher-order function for applying `var_info`
codebook_extract <- function(x) {

  if ("name" %in% names(x)) {
    this_name <- x$name
  } else {
    this_name <- NA
  }

  these_vars <- dplyr::bind_rows(lapply(x$variables, var_info))

  these_vars$level_name <- this_name

  return(these_vars)
}


