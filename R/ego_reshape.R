#' Reshaping Egocentric Data (\code{ego_reshape})
#'
#' @description The \code{ego_reshape} function reshapes egocentric network data stored in a single wide dataset into three dataframes optimized for use with \code{\link{ego_netwrite}}.
#'
#' @param data A data frame containing egocentric network data in a wide format.
#' @param ego_id A character value indicating the name of the column in \code{data} containing ego identifiers, or a numeric value indicating the position of the column containing ego identifiers.
#' @param ego_vars A character vector indicating the names of the columns in \code{data} containing ego-level measures, or a numeric vector indicating the positions of the columns containing ego-level measures.
#' @param alters A character vector indicating the names of the columns in \code{data} containing ego-alter ties, or a numeric vector indicating the positions of the columns containing ego-alter ties.
#' @param alter_vars A character vector indicating the names of the columns in \code{data} containing alter-level measures, or a numeric vector indicating the positions of the columns containing alter-level measures. Variables are assumed to be ordered such that each consecutive set of columns represent a single alter-level variable, and that columns within this set are ordered such that the first column represents the value associated with the alter represented by the first column specified in \code{alters}, the second column in the set is associated with the alter represented by the second column in \code{alters}, and so on. If a certain variable was collected for only the first \code{n} alters in a survey instrument, we recommend creating placeholder columns in which all values are coded \code{NA}.
#' @param alter_alter A character vector indicating the names of the columns in \code{data} that indicate the presence of alter-alter ties, or a numeric vector indicating the positions of the columns indicating alter-alter ties. These columns should be ordered such that their values appear as they would were one to move left-to-right, top-to-bottom in an adjacency matrix. For example, the value of column 1 should usually indicate whether a tie exists between alter 1 and alter 2, the value of column 2 should indicate the presence of a tie between alter 1 and alter 3, the value of column 3 should indicate the presence of a tie between alter 1 and alter 4, and so on. The number of columns needed to represent the full set of possible ties between alters may vary depending on a) whether ties in the network are directed or undirected, and b) whether it is possible for alters to be tied to themselves. If users do not specify these conditions using the \code{directed} and \code{loops} arguments, respectively, This function uses the number of columns specified in \code{alters} to detect the presenece of directed ties and self-loops in the network.
#' @param aa_vars A character vector indicating the names of the columns in \code{data} representing edge-level characteristics of alter-alter ties. Columns should be ordered in a similar fashion as with \code{alter_vars} where consecutive sets of \code{n} columns represent a single variable and columns within these sets are ordered in the same way as their corresponding edge indicator columns are ordered in \code{alter_alter}.
#' @param directed A logical value indicating whether alter-alter ties are directed or undirected.
#' @param loops A logical value indicating whether alter-alter ties contain self-loops (alters can be tied to themselves).
#' @param missing_code A numeric value indicating "missing" values in the alter-alter edgelist.
#' @param output_name A character value indicating the name or prefix that should be given to output objects.
#'
#' @return A list containing three data frames: an ego list, an ego-alter edgelist, and an alter-alter edgelist. These dataframes are optimized for use with \code{\link{ego_netwrite}}.
#'
#' @export
#'
#' @importFrom rlang .data


ego_reshape <- function(data,
                        ego_id,
                        ego_vars,
                        alters,
                        alter_vars,
                        alter_alter,
                        aa_vars,
                        directed = NULL,
                        loops = NULL,
                        missing_code = 99999,
                        output_name = "ego_long") {

  # Create output list
  output_list <- list()

  ##### TEXT CONVERSION ##########################################################
  # Taking strings entered as arguments and converting them to their numeric index
  if (methods::is(ego_id, "character")) {
    ego_id <- which(names(data) %in% ego_id)
  }

  if (methods::is(ego_vars, "character")) {
    ego_vars <- which(names(data) %in% ego_vars)
  }

  if (methods::is(alters, "character")) {
    alters <- which(names(data) %in% alters)
  }

  if (methods::is(alter_vars, "character")) {
    alter_vars <- which(names(data) %in% alter_vars)
  }

  if (methods::is(alter_alter, "character")) {
    alter_alter <- which(names(data) %in% alter_alter)
  }

  if (methods::is(aa_vars, "character")) {
    aa_vars <- which(names(data) %in% aa_vars)
  }

  ##### INITIAL CHECKS ###########################################################
  # How many alters could an ego name?
  max_alters <- length(alters)
  # Is the length of `alter_vars` a multiple of `max_alters`?
  if (length(alter_vars) %% max_alters != 0) {
    stop(paste("Number of columns specified in alter_vars (",
               length(alter_vars),
               ") is not a multiple of the number alters specified (",
               max_alters,
               "). Be sure you have selected all needed columns and that they are ordered correctly.",
               sep = ""))
  }

  # How many alter-alter tie columns should we be looking for?
  # If we haven't yet figured out what the target number of alter-alter edges is,
  # we need to infer it from the number of alter-alter edges specified
  if (!is.null(alter_alter)) {
    if (is.null(directed) & is.null(loops)) {

      target_conditions <- data.frame(condition = c("undirected_noloop", "undirected_loop", "directed_noloop", "directed_loop"),
                                      value =     c((max_alters*(max_alters-1))/2, (max_alters*(max_alters-1))/2 + max_alters, max_alters*(max_alters-1), max_alters*(max_alters-1) + max_alters),
                                      message =   c("undirected with no self-loops", "undirected with self-loops", "directed with no self-loops", "directed with self-loops"),
                                      directed =  c(FALSE, FALSE, TRUE, TRUE),
                                      loops =     c(FALSE, TRUE, FALSE, TRUE))

      if (length(alter_alter) %in% target_conditions$value) {

        aa_target <- target_conditions[which(length(alter_alter) == target_conditions$value), 'value']
        aa_message <- target_conditions[which(length(alter_alter) == target_conditions$value), 'message']
        directed <- target_conditions[which(length(alter_alter) == target_conditions$value), 'directed']
        loops <- target_conditions[which(length(alter_alter) == target_conditions$value), 'loops']
        base::message(paste("Number of columns specified in alter_alter (",
                            length(alter_alter),
                            ") suggests that the ego networks in this dataset are ",
                            aa_message,
                            ". Data will be processed under this assumption.",
                            sep = ""))

      } else {
        stop(paste("Number of columns specified in alter_alter (",
                   length(alter_alter),
                   ") does not match any expected number of possible alter-alter ties. Be sure you have selected all needed columns and that they are ordered correctly.",
                   sep = ""))

      }

    } else {
      # Determine baseline based on `directed` argument
      aa_target <- NA
      if (directed == TRUE) {
        aa_target <- max_alters*(max_alters-1)
      } else if (directed == FALSE) {
        aa_target <- max_alters*(max_alters-1)
      } else {
        aa_target <- aa_target
      }
      # Adjust depending on presence of self-loops
      if (loops == TRUE) {
        aa_target <- aa_target + max_alters
      }
    }
  }
  # Now that we definitely have a value for `aa_target`, we need to see if the
  # number of columns in `aa_vars` is a multiple of `aa_target`
  if (!is.null(aa_vars)) {
    if (length(aa_vars) %% aa_target != 0) {
      stop(paste("Number of columns specified in aa_vars (",
                 length(aa_vars),
                 ") is not a multiple of the expected number of possible alter-alter ties (",
                 aa_target,
                 "). Be sure you have selected all needed columns and that they are ordered correctly.",
                 sep = ""))
    }
  }

  ##### EGO LIST #################################################################
  # Extract ego-level variables and set aside
  ego_df <- as.data.frame(data[,c(ego_id, ego_vars)])
  # Rename ego_id column here to make things easier later
  colnames(ego_df)[[1]] <- "ego_id"


  ##### ALTER LIST ###############################################################
  # Extract ego-alter edge variables
  alter_df <- data[,c(ego_id, alters)]
  # Rename columns here to make things easier
  colnames(alter_df) <- paste("alter", 0:(ncol(alter_df)-1), sep = "_")
  colnames(alter_df)[[1]] <- "ego_id"

  alter_df <- alter_df %>%
    tidyr::pivot_longer(cols = -.data$ego_id,
                        names_to = "var",
                        values_to = "alter") %>%
    dplyr::group_by(.data$ego_id) %>%
    dplyr::mutate(alter_id = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$alter != 99999) %>%
    dplyr::select(.data$ego_id, .data$alter_id, .data$alter)

  # Now we're dealing with the alter-level variables
  # Extract alter-level variables and keep `ego_id` for merging purposes
  alter_vars_df <- data[,c(ego_id, alter_vars)]
  colnames(alter_vars_df)[[1]] <- "ego_id"
  # Index for extracting individual variables
  alter_vars_index <- c(seq(2, ncol(alter_vars_df), max_alters), ncol(alter_vars_df)+1)

  for (i in 1:(length(alter_vars_index)-1)) {

    # Which columns are we pulling?
    extracted_cols <- alter_vars_index[[i]]:(alter_vars_index[[i+1]]-1)
    # What should we call this variable?
    this_name <- colnames(alter_vars_df)[[alter_vars_index[[i]]]]

    this_alter_var <- alter_vars_df[, c(1, extracted_cols)] %>%
      tidyr::pivot_longer(cols = -.data$ego_id,
                          names_to = "var",
                          values_to = this_name) %>%
      dplyr::group_by(.data$ego_id) %>%
      dplyr::mutate(alter_id = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::select(-.data$var)
    # Merge into `alter_df`
    alter_df <- alter_df %>%
      dplyr::left_join(this_alter_var, by = c("ego_id", "alter_id"))
  }

  ##### ALTER-ALTER EDGELIST #####################################################
  if (!is.null(alter_alter)) {

    # Creating a matrix populated by dyad labels
    cell_labels <- matrix(paste(rep(1:max_alters, each = max_alters),
                                rep(1:max_alters, max_alters),
                                sep = "_"),
                          nrow = max_alters,
                          ncol = max_alters,
                          byrow = TRUE)

    if (directed == FALSE) {
      cell_labels <- sort(cell_labels[upper.tri(cell_labels, diag = loops)])
    } else {
      if (loops == TRUE) {
        cell_labels <- sort(as.vector(cell_labels))
      } else {
        diag(cell_labels) <- NA
        cell_labels <- sort(as.vector(cell_labels))
      }
    }

    aa_tie_df <- data[,c(ego_id, alter_alter)]
    colnames(aa_tie_df) <- c("ego_id", cell_labels)
    aa_tie_df <- aa_tie_df %>%
      tidyr::pivot_longer(cols = -.data$ego_id,
                          names_to = "dyad",
                          values_to = "val") %>%
      dplyr::mutate(alter1 = as.numeric(stringr::str_replace(.data$dyad, "_\\d*$", "")),
                    alter2 = as.numeric(stringr::str_replace(.data$dyad, "^\\d*_", ""))) %>%
      dplyr::filter(.data$val != 0) %>%
      dplyr::select(.data$ego_id, .data$alter1, .data$alter2)


    # Now we're dealing with the alter-level variables
    # Extract alter-level variables and keep `ego_id` for merging purposes
    if (!is.null(aa_vars)) {
      aa_vars_df <- data[,c(ego_id, aa_vars)]
      colnames(aa_vars_df)[[1]] <- "ego_id"
      # Index for extracting individual variables
      aa_vars_index <- c(seq(2, ncol(aa_vars_df), aa_target), ncol(aa_vars_df)+1)

      for (i in 1:(length(aa_vars_index)-1)) {

        # Which columns are we pulling?
        extracted_cols <- aa_vars_index[[i]]:(aa_vars_index[[i+1]]-1)
        # What should we call this variable?
        this_name <- colnames(aa_vars_df)[[aa_vars_index[[i]]]]

        this_aa_var <- aa_vars_df[, c(ego_id, extracted_cols)]
        colnames(this_aa_var) <- c("ego_id", cell_labels)

        this_aa_var <- this_aa_var %>%
          tidyr::pivot_longer(cols = -.data$ego_id,
                              names_to = "dyad",
                              values_to = this_name) %>%
          dplyr::mutate(alter1 = as.numeric(stringr::str_replace(.data$dyad, "_\\d*$", "")),
                        alter2 = as.numeric(stringr::str_replace(.data$dyad, "^\\d*_", ""))) %>%
          dplyr::select(-.data$dyad)
        # Merge into `alter_df`
        aa_tie_df <- aa_tie_df %>%
          dplyr::left_join(this_aa_var, by = c("ego_id", "alter1", "alter2"))
      }
    }
  }

  ##### SAVING OUTPUT ############################################################

  output_list$egos <- ego_df
  # assign(paste(output_name, "egos", sep = "_"), ego_df, .GlobalEnv)
  output_list$alters <- alter_df
  # assign(paste(output_name, "alters", sep = "_"), alter_df, .GlobalEnv)

  if (!is.null(alter_alter)) {
    output_list$alter_edgelist <- aa_tie_df
    # assign(paste(output_name, "alter_edgelist", sep = "_"), aa_tie_df, .GlobalEnv)
  }

  return(output_list)

}
#
# # Directed Ties, No Self-Loops
# ego_wide <- readxl::read_xlsx("~/Desktop/netwrite_debug/egonets/ego_wide.xlsx")
#
# ego_reshape(data = ego_wide,
#             ego_id = "ego_id",
#             ego_vars = c("ego_sex", "ego_race"),
#             alters = paste("alter", 1:5, sep = ""),
#             alter_vars = c(paste("alter", 1:5, "_sex", sep = ""),
#                            paste("alter", 1:5, "_race", sep = "")),
#             alter_alter = names(ego_wide)[stringr::str_detect(names(ego_wide), "^tie")],
#             aa_vars = names(ego_wide)[stringr::str_detect(names(ego_wide), "^var")],
#             directed = NULL,
#             loops = NULL,
#             missing_code = 99999,
#             name = "dir_noself")
#
# # Directed Ties, Self-Loops
# ego_wide <- readxl::read_xlsx("~/Desktop/netwrite_debug/egonets/ego_wide_selfloops.xlsx")
#
# ego_reshape(data = ego_wide,
#             ego_id = "ego_id",
#             ego_vars = c("ego_sex", "ego_race"),
#             alters = paste("alter", 1:5, sep = ""),
#             alter_vars = c(paste("alter", 1:5, "_sex", sep = ""),
#                            paste("alter", 1:5, "_race", sep = "")),
#             alter_alter = names(ego_wide)[stringr::str_detect(names(ego_wide), "^tie")],
#             aa_vars = names(ego_wide)[stringr::str_detect(names(ego_wide), "^var")],
#             directed = NULL,
#             loops = NULL,
#             missing_code = 99999,
#             name = "dir_self")
#
# # Undirected Ties, No Self-Loops
# ego_wide <- readxl::read_xlsx("~/Desktop/netwrite_debug/egonets/ego_wide_undirected.xlsx")
#
# ego_reshape(data = ego_wide,
#             ego_id = "ego_id",
#             ego_vars = c("ego_sex", "ego_race"),
#             alters = paste("alter", 1:5, sep = ""),
#             alter_vars = c(paste("alter", 1:5, "_sex", sep = ""),
#                            paste("alter", 1:5, "_race", sep = "")),
#             alter_alter = names(ego_wide)[stringr::str_detect(names(ego_wide), "^tie")],
#             aa_vars = names(ego_wide)[stringr::str_detect(names(ego_wide), "^var")],
#             directed = NULL,
#             loops = NULL,
#             missing_code = 99999,
#             name = "undir_noself")
#
# # Undirected Ties, Self-Loops
# ego_wide <- readxl::read_xlsx("~/Desktop/netwrite_debug/egonets/ego_wide_selfloops.xlsx")
#
# ego_reshape(data = ego_wide,
#             ego_id = "ego_id",
#             ego_vars = c("ego_sex", "ego_race"),
#             alters = paste("alter", 1:5, sep = ""),
#             alter_vars = c(paste("alter", 1:5, "_sex", sep = ""),
#                            paste("alter", 1:5, "_race", sep = "")),
#             alter_alter = names(ego_wide)[stringr::str_detect(names(ego_wide), "^tie")],
#             aa_vars = names(ego_wide)[stringr::str_detect(names(ego_wide), "^var")],
#             directed = NULL,
#             loops = NULL,
#             missing_code = 99999,
#             name = "undir_self")
