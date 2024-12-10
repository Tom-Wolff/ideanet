#' Merging Network Canvas CSV Files (\code{nc_merge})
#'
#' @description The \code{nc_merge} function combines CSV files exported from \href{https://networkcanvas.com/}{Network Canvas}, a popular tool for egocentric data capture. It is designed to address issues that may be encountered by \code{nc_read} when Network Canvas exports separate CSV files for individual responses.
#'
#' @param path A character value indicating the directory in which Network Canvas CSVs are located. \code{nc_read} will read in all CSV files located in this directory and process them.
#' @param export_path A character value indicating the directory to which merged CSV files should be exported. This should not be the same directory as \code{path}, and this function will return an error if it detects that \code{path} and \code{export_path} are equivalent.
#'
#' @return \code{nc_merge} always writes two CSV files to the directory specified in \code{export_path}: an ego list and an alters list. If CSV files containing alter-alter ties are detected, it also writes a third merged CSV of these ties.
#'
#' @export
#'
#' @importFrom rlang .data


nc_merge <- function(
    # ARGUMENTS:

  # Path to folder containing exported NC .csv files
  path,

  # Path to directory where aggregated NC .csv files should be exported
  # this should NOT be the same directory as `path`, and this function will
  # return an error if it detects these values are equivalent
  export_path

) {

  # Creating fake objects to sidestep global binding issue
  level_name <- NULL
  var_type <- NULL

  # browser()

  # If user puts a forward slash at the end of the directory path they define,
  # it will cause problems. Check if this happens and correct
  if (stringr::str_detect(path, "/$")) {
    path <- stringr::str_replace(path, "/$", "")
  }

  if (stringr::str_detect(export_path, "/$")) {
    export_path <- stringr::str_replace(export_path, "/$", "")
  }

  if (path == export_path) {
    stop("`path` and `export_path` cannot point to the same directory.")
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
      egos <- egos %>% dplyr::mutate_all(as.character)
      egos$networkCanvasCaseID <- as.character(egos$networkCanvasCaseID)
    } else {
      this_ego <- utils::read.csv(paste(path, ego_files[[i]], sep = "/"), header = TRUE)
      this_ego <- this_ego %>% dplyr::mutate_all(as.character)
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
      alters <- utils::read.csv(paste(path, alter_files[[i]], sep = "/"), header = TRUE)
      # Need to handle the case in which ego 1 is an isolate
      if (nrow(alters) == 0) {
        alters[1,] <- NA
      } else {
        alters <- alters %>% dplyr::mutate_all(as.character)
      }

      ### Record type of alter
      node_type <- stringr::str_extract(alter_files[[i]], "attributeList.*.csv")
      node_type <- stringr::str_replace(node_type, "attributeList_", "")
      node_type <- stringr::str_replace(node_type, ".csv", "")
      alters$node_type <- node_type
      alters$data_file <- paste(path, alter_files[[i]], sep = "/")
    } else {
      this_alter <- utils::read.csv(paste(path, alter_files[[i]], sep = "/"), header = TRUE)
      # Only need to do the rest if there are actually alter nominated by ego,
      # otherwise can skip
      if (nrow(this_alter) > 0) {
        this_alter <- this_alter %>% dplyr::mutate_all(as.character)
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
      el <- utils::read.csv(paste(path, edge_files[[i]], sep = "/"), header = TRUE)
      # Handling if first ego is an isolate
      if (nrow(el) == 0) {
        el[1,] <- NA
      } else {
        el <- el %>% dplyr::mutate_all(as.character)
      }

      ### Record type of edge
      edge_type <- stringr::str_extract(edge_files[[i]], "edgeList.*.csv")
      edge_type <- stringr::str_replace(edge_type, "edgeList_", "")
      edge_type <- stringr::str_replace(edge_type, ".csv", "")
      el$edge_type <- edge_type
      el$data_file <- paste(path, edge_files[[i]], sep = "/")
    } else {
      this_el <- utils::read.csv(paste(path, edge_files[[i]], sep = "/"), header = TRUE)

      if (nrow(this_el) == 0) {
        next
      } else {
        this_el <- this_el %>% dplyr::mutate_all(as.character)
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


  # Exporting merged CSVs
  utils::write.csv(egos,
            paste(export_path, "merged_ego.csv", sep = "/"))
  utils::write.csv(alters,
            paste(export_path, "attributeList_merged.csv", sep = "/"))

  if (no_el == FALSE) {
    utils::write.csv(el,
              paste(export_path, "edgeList_merged.csv", sep = "/"))
  }




}
