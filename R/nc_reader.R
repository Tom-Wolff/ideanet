# Calling tidyverse for magrittr pipes. All non-base functions SHOULD be
# manually called otherwise. ideanet loads magrittr pipes by default so
# this is only here for testing purposes.

library(tidyverse)

nc_read <- function(
                    # ARGUMENTS:

                    # Path to folder containing exported NC .csv files
                    path,

                    # Whether to convert categorical booleans to a factor
                    # (i.e. apply Pat's catToFactor function)
                    # See lines 192-235 to see how I tried to automatically
                    # detect which sets of variables should be treated as
                    # booleans corresponding to a single response item
                    cat.to.factor = TRUE,

                    # Is this being used within ideanet?
                    # If yes, store output as a list for downstream processing
                    # If no, assign data frames to global environment
                    ideanet = TRUE

                    ) {

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
    egos <- read.csv(paste(path, ego_files[[i]], sep = "/"), header = TRUE)
  } else {
    this_ego <- read.csv(paste(path, ego_files[[i]], sep = "/"), header = TRUE)
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
    alters <- read.csv(paste(path, alter_files[[i]], sep = "/"), header = T)
    ### Record type of alter
    alter_type <- stringr::str_extract(alter_files[[i]], "attributeList.*.csv")
    alter_type <- stringr::str_replace(alter_type, "attributeList_", "")
    alter_type <- stringr::str_replace(alter_type, ".csv", "")
    alters$alter_type <- alter_type
  } else {
    this_alter <- read.csv(paste(path, alter_files[[i]], sep = "/"), header = T)
    ### Record type of alter
    alter_type <- stringr::str_extract(alter_files[[i]], "attributeList.*.csv")
    alter_type <- stringr::str_replace(alter_type, "attributeList_", "")
    alter_type <- stringr::str_replace(alter_type, ".csv", "")
    this_alter$alter_type <- alter_type
    alters <- dplyr::bind_rows(alters, this_alter)
  }
}

# Compile Alter-Alter Edgelists

edge_files <- just_csv[stringr::str_detect(just_csv, "edgeList")]

for (i in 1:length(edge_files)) {
  if (i == 1) {
    el <- read.csv(paste(path, edge_files[[i]], sep = "/"), header = T)
    ### Record type of edge
    edge_type <- stringr::str_extract(edge_files[[i]], "edgeList.*.csv")
    edge_type <- stringr::str_replace(edge_type, "edgeList_", "")
    edge_type <- stringr::str_replace(edge_type, ".csv", "")
    el$edge_type <- edge_type
  } else {
    this_el <- read.csv(paste(path, edge_files[[i]], sep = "/"), header = T)
    ### Record type of edge
    edge_type <- stringr::str_extract(edge_files[[i]], "edgeList.*.csv")
    edge_type <- stringr::str_replace(edge_type, "edgeList_", "")
    edge_type <- stringr::str_replace(edge_type, ".csv", "")
    this_el$edge_type <- edge_type
    el <- dplyr::bind_rows(el, this_el)
  }
}

# Apply catToFactor, if desired
if (cat.to.factor == TRUE) {
  egos <- apply_catToFactor(egos)
  alters <- apply_catToFactor(alters)
  el <- apply_catToFactor(el)
}

# Converting boolean columns to R logical vectors
egos <- dplyr::mutate_all(egos, to_logical)
alters <- dplyr::mutate_all(alters, to_logical)
el <- dplyr::mutate_all(el, to_logical)

# Convert date columns to POSIXct objects
egos <- dplyr::mutate_all(egos, to_date)
alters <- dplyr::mutate_all(alters, to_date)
el <- dplyr::mutate_all(el, to_date)


# Convert session info to POSIXct
egos$sessionStart <- to_posix(egos$sessionStart)
egos$sessionFinish <- to_posix(egos$sessionFinish)
egos$sessionExported <- to_posix(egos$sessionExported)

# Easier ego ID numbers
egos <- egos %>%
  dplyr::group_by(networkCanvasEgoUUID) %>%
  dplyr::mutate(ego_id = dplyr::cur_group_id()) %>%
  dplyr::ungroup() %>%
  dplyr::select(ego_id, dplyr::everything())

# Get the basics to merge into other dataframes
ego_basic <- egos %>%
  dplyr::select(ego_id, networkCanvasEgoUUID)

alters <- alters %>%
  dplyr::left_join(ego_basic, by = "networkCanvasEgoUUID") %>%
  dplyr::select(ego_id, alter_id = nodeID, dplyr::everything())

el <- el %>%
  dplyr::left_join(ego_basic, by = "networkCanvasEgoUUID") %>%
  dplyr::select(ego_id, edge_id = edgeID, from, to, edge_type, dplyr::everything())


if (ideanet == FALSE) {
  # We'll want to extract the protocol name from the filenames for naming
  # the objects we output to the global environment
  protocol_name <- egos$networkCanvasProtocolName[[1]]

  assign(x = paste(protocol_name, "_egos", sep = ""), value = egos, .GlobalEnv)
  assign(x = paste(protocol_name, "_alters", sep = ""), value = alters, .GlobalEnv)
  assign(x = paste(protocol_name, "_alter_edgelists", sep = ""), value = el, .GlobalEnv)
} else {
  nc_list <- list(egos = egos,
                  alters = alters,
                  alter_edgelists = el)
  return(nc_list)
}

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
  } else if (sum(apply(dataframe[,catVariables], 1, function(x) sum(x %in% "true")>1))>0) {
  #  stop(paste0("Your variable -",variableName,"  - appears to take multiple values.")) }
    warning(paste0("Your variable -",variableName,"  - appears to take multiple values. This variable will not be recoded."))
    return(c(NA))
} else {
  catValues <- sub(paste0('.*',fullVariableName), '', catVariables)
  factorVariable <- c()
  for(i in 1:length(catVariables)){
    factorVariable[dataframe[catVariables[i]]=="true"] <- catValues[i]
  }
  return(factor(factorVariable,levels=catValues))
}
}

# Identifying which variables to which we apply `catToFactor`
apply_catToFactor <- function(df){
### 1. Remove `_num` suffix
    ##### First create vector of column names
    df_names <- colnames(df)
    prefixes <- stringr::str_remove_all(df_names, "_\\d$")
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
      if (sum(!(these_vals %in% c("true", "false", NA))) == 0) {
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
        df_names <- colnames(df)
        df[,(ncol(df)+1)] <- this_var
        colnames(df) <- c(df_names, final_prefixes[i])
      }
    }
    return(df)
} else {
    return(df)
}
}

# Function for turning NC booleans into R logicals
to_logical <- function(x) {
  if (sum(!(x %in% c("true", "false", NA))) == 0) {
    x <- as.logical(x)
    return(x)
  } else {
    return(x)
  }
}

# Function for converting dates
to_date <- function(x) {

  # Need to handle situation where NAs might be mixed in with NAs
  string_check <- stringr::str_detect(x, "^\\d\\d\\d\\d-\\d\\d-\\d\\d$")
  string_check2 <- stringr::str_detect(x, "^\\d\\d\\d\\d-\\d\\d$")
  # Remove NAs since they mess up the validation process
  check_na <- string_check[!is.na(string_check)]
  check_na2 <- string_check2[!is.na(string_check2)]

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

# POSIXct converter for session info

to_posix <- function(x) {
x <- stringr::str_replace_all(x, "T", " ")
x <- stringr::str_replace_all(x, "Z", "")
x <- as.POSIXct(x, format = "%Y-%m-%d %H:%M:%OS")
}

###############################################################################
# TESTING
###############################################################################

# # Not working with ideanet, just want dataframes exported to Global Environ.
# nc_read(path = "~/Desktop/network_canvas/nc_test",
#         cat.to.factor = TRUE,
#         ideanet = FALSE)
#
# # Working within ideanet egonet workflow
# nc_list <- nc_read(path = "~/Desktop/network_canvas/nc_test",
#             cat.to.factor = TRUE,
#             ideanet = TRUE)


