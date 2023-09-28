#' Additional Measure Calculation for Ego Networks (`ego_alter_measures`)
#'
#' @description Common measures of ego network properties can differ in whether they require information about ego's attributes. Some measures, like the h-index, only require information about alter attributes, while others, such as Pearson's phi, require information about both ego and their alters. `ego_netwrite` calculates measures of the former sort by default. `ego_alter_measures`, by contrast, allows users to calculate measures that require both ego- and alter-level data.
#'
#' @param ego_df A data frame containing measures of ego attributes. If using data objects created by `ego_netwrite`, this should be the data frame entitled `egos`.
#' @param alter_df A data frame containing measures of alter attributes. If using data objects created by `ego_netwrite`, this should be the data frame entitled `alters`.
#' @param ego_prefix A character value indicating a common prefix for variables contained in `ego_df`. These prefixes are removed in order to match variables with their counterparts in `alter_df`.
#' @param ego_suffix A character value indicating a common suffix for variables contained in `ego_df`. These suffixes are removed in order to match variables with their counterparts in `alter_df`.
#' @param alter_prefix A character value indicating a common prefix for variables contained in `alter_df`. These prefixes are removed in order to match variables with their counterparts in `ego_df`.
#' @param alter_suffix A character value indicating a common suffix for variables contained in `alter_df`. These suffixes are removed in order to match variables with their counterparts in `ego_df`.
#' @param vars A character vector indicating which variables appearing in both `ego_df` and `alter_df` should be matched and used for calculating measures. If the user does not manually specify variables in this argument, `ego_alter_measures` will automatically search for matching variables across `ego_df` and `alter_df`.
#' @param measures A character vector indicating which measures should be calculated. Measures include diversity, number and proportion of homophilous alters, E-I index, Pearson's phi, and Euclidean distance. If the user does not manually specify measures in this argument, `ego_alter_measures` will calculate all applicable measures to each variable.
#' @param na.rm A logical value indicating whether `NA` values should be excluded when calculating continuous measures.
#'
#' @return `ego_alter_measures` returns a data frame containing the measures specified by the user for each individual ego network. This data frame is complementary to the `summaries` data frame created by `ego_netwrite`, and the two can easily be merged.
#'
#' @export


ego_alter_measures <- function(ego_df,
                               alter_df,
                               ego_prefix = NULL,
                               ego_suffix = NULL,
                               alter_prefix = NULL,
                               alter_suffix = NULL,
                               # If variables aren't defined, just search for matches
                               vars = NULL,
                               measures = c("diversity", "number_homophilous", "proportion_homophilous",
                                            "ei_index", "pearson_phi", "euclidean_distance"),
                               na.rm = FALSE) {

  # 1. Remove ego and alter prefixes and suffixes, if applicable
  if (!is.null(ego_prefix) == TRUE) {
        ##### Store original colnames
        orig_egonames <- colnames(ego_df)
        colnames(ego_df) <- stringr::str_remove_all(colnames(ego_df), paste("^", ego_prefix, sep = ""))
        ##### Make sure `ego_id` remains `ego_id` though
        colnames(ego_df)[[which(orig_egonames == "ego_id")]] <- "ego_id"
  }

  if (!is.null(alter_prefix) == TRUE) {
    ##### Store original colnames
    orig_alternames <- colnames(alter_df)
    colnames(alter_df) <- stringr::str_remove_all(colnames(alter_df), paste("^", alter_prefix, sep = ""))
    ##### Make sure `alter_id` remains `alter_id` though
    colnames(alter_df)[[which(orig_alternames == "alter_id")]] <- "alter_id"
  }

  if (!is.null(ego_suffix) == TRUE) {
    ##### Store original colnames
    orig_egonames <- colnames(ego_df)
    colnames(ego_df) <- stringr::str_remove_all(colnames(ego_df), paste(ego_suffix, "$", sep = ""))
  }

  if (!is.null(alter_suffix) == TRUE) {
    ##### Store original colnames
    orig_alternames <- colnames(alter_df)
    colnames(alter_df) <- stringr::str_remove_all(colnames(alter_df), paste(alter_suffix, "$", sep = ""))
  }

  # 2. If variables used to derive measures are defined, extract only these from `ego_df`
  # and `alter_df`

  if (!is.null(vars) == TRUE) {

    # Ensure that variables are in both ego and alter dataframes
    not_in_ego <- vars[!(vars %in% colnames(ego_df))]
    not_in_alter <- vars[!(vars %in% colnames(alter_df))]
    bad_vars <- c(not_in_ego, not_in_alter)

    # If 1+ variables do not appear in both dataframes, warn the user and exclude these variables
    # from calculations
    if (length(bad_vars == 1)) {
      base::warning(paste("Variable ", bad_vars, " does not appear in both ego and alter data frames. Measures related to ", bad_vars, " will not be calculated.", sep = ""))
      vars <- vars[!(vars %in% bad_vars)]
    } else if (length(bad_vars > 1)) {
      bad_var_string1 <- paste(bad_vars[1:(length(bad_vars)-1)], collapse = ", ")
      bad_var_string2 <- paste(bad_var_string1, bad_vars[length(bad_vars)], sep = ", and ")
      base::warning(paste("Variables ", bad_var_string2, " do not appear in both ego and alter data frames. Measures related to these variables will not be calculated.", sep = ""))
      vars <- vars[!(vars %in% bad_vars)]
    }

    # Extract variables being compared from `ego_df` and `alter_df`
    ego_df <- ego_df[,c("ego_id", vars)]
    alter_df <- alter_df[, c("ego_id", "alter_id", vars)]
  } else {
    ##### Otherwise extract variables that have matching names
    name_matches <- colnames(alter_df)[colnames(alter_df) %in% colnames(ego_df)]
    ##### Remove `ego_id` for next step
    vars <- name_matches[which(name_matches != "ego_id")]
    ##### Now subset `ego_df` and `alter_df`
    ego_df <- ego_df[,c("ego_id", vars)]
    alter_df <- alter_df[, c("ego_id", "alter_id", vars)]
  }





  # 4.5. Make list of just information pertaining to a particular variable
  var_list <- list()
  for (i in 1:length(vars)) {
    ego_info <- ego_df[,c("ego_id", vars[[i]])]
    alter_info <- alter_df[,c("ego_id", vars[[i]])]

    # 4. Check that classes match across variables
    ego_class <- class(ego_info[,2])
    alter_class <- class(alter_info[,2])

    # ego_class_label <- character()
    # alter_class_label <- character()
    #
    # # Determine ego class label
    # if (detect_integer_cat(ego_info[,2]) | detect_char_cat(ego_info[,2]) | ("factor" %in% ego_class & !("ordered" %in% ego_class))) {
    #   ego_class_label <- c(ego_class_label, "categorical")
    # }
    #
    # if (("logical" %in% ego_class) | detect_integer_binary(ego_info[,2]) | detect_char_binary(ego_info[,2])) {
    #   ego_class_label <- c(ego_class_label, "binary")
    # }
    #
    # if (detect_numeric(ego_info[,2]) | detect_integer(ego_info[,2])) {
    #   ego_class_label <- c(ego_class_label, "continuous")
    # }
    #
    # # Determine alter class label
    # if (detect_integer_cat(alter_info[,2]) | detect_char_cat(alter_info[,2]) | ("factor" %in% alter_class & !("ordered" %in% alter_class))) {
    #   alter_class_label <- c(alter_class_label, "categorical")
    # }
    #
    # if (("logical" %in% alter_class) | detect_integer_binary(alter_info[,2]) | detect_char_binary(alter_info[,2])) {
    #   alter_class_label <- c(alter_class_label, "binary")
    # }
    #
    # if (detect_numeric(alter_info[,2]) | detect_integer(alter_info[,2])) {
    #   alter_class_label <- c(alter_class_label, "continuous")
    # }

    # New approach: combine columns into a single vector then run check
    combined_var <- c(ego_info[,2], alter_info[,2])
    combined_class <- character()

    # Determine class label
    if (detect_integer_cat(combined_var) | detect_char_cat(combined_var) | ("factor" %in% class(combined_var) & !("ordered" %in% class(combined_var)))) {
      combined_class <- c(combined_class, "categorical")
    }

    if (("logical" %in% class(combined_var)) | detect_integer_binary(combined_var) | detect_char_binary(combined_var)) {
      combined_class <- c(combined_class, "binary")
    }

    if (detect_numeric(combined_var) | detect_integer(combined_var)) {
      combined_class <- c(combined_class, "continuous")
    }

    if ("POSIXct" %in% class(combined_var)) {
      combined_class <- c(combined_class, "date")
    }


    #if (sum(ego_class_label %in% alter_class_label) == 0) {
    if (length(combined_class) == 0) {
      warning(paste("Ego and alter measures for ", vars[[i]], " do not appear to be comparable. Measures will not be generated for this variable.",
                    sep = ""))

      var_list[[i]] <- list(class = NA,
                            var_df = NA,
                            name = vars[[i]])
    } else {

    var_df <- alter_info %>%
      dplyr::left_join(ego_info, by = "ego_id")
    colnames(var_df) <- c("ego_id", "alter_val", "ego_val")


    # Add a warning indicating if unique values don't overlap for both columns
    # (if categorical)
    if ("categorical" %in% combined_class) {

      # Get unique values of `ego_val` column that aren't NAs
      unique_ego_val <- unique(var_df$ego_val)
      unique_ego_val <- unique_ego_val[!is.na(unique_ego_val)]
      # Same for `alter_val` column
      unique_alter_val <- unique(var_df$alter_val)
      unique_alter_val <- unique_alter_val[!is.na(unique_alter_val)]
      # Determine if there's any overlap
      val_overlaps <- sum(unique_ego_val %in% unique_alter_val)
      # Display warning message if `val_overlaps == 0`
      if (val_overlaps == 0) {
        warning(paste("No values in variable ", vars[[i]], " appear for both egos and alters. You may want to ensure that this variable is similarly coded for both egos and alters.",
                      sep = ""))
      }
    }


    # both_class_label <- ego_class_label[which(ego_class_label %in% alter_class_label)]

    var_list[[i]] <- list(#class = both_class_label,
                          class = combined_class,
                          var_df = var_df,
                          name = vars[[i]])
    }

  }

  names(var_list) <- vars

  # 5. Check that values are similarly coded

  # 6. For each class of variable, generate measures for selected variables

var_measures <- lapply(var_list, get_measures)

# Set up DF for final output
final_df <- ego_df %>%
  dplyr::select(ego_id)

# Loop over elements of `var_measures` and merge into `final_df` if there's anything
# in there
for (i in 1:length(var_measures)) {
  if (!is.null(var_measures[[i]]) == TRUE) {
    final_df <- final_df %>%
      dplyr::left_join(var_measures[[i]], by = "ego_id")
  }
}


# If user specified only a specific set of measures to calculate, return only
# those values in `final_df`

if (!("diversity" %in% measures)) {
    final_df <- final_df[,!stringr::str_detect(colnames(final_df), "diversity")]
}

if (!("number_homophilous" %in% measures)) {
  final_df <- final_df[,!stringr::str_detect(colnames(final_df), "num_sim")]
}

if (!("proportion_homophilous" %in% measures)) {
  final_df <- final_df[,!stringr::str_detect(colnames(final_df), "prop_sim")]
}

if (!("ei_index" %in% measures)) {
  final_df <- final_df[,!stringr::str_detect(colnames(final_df), "ei_index")]
}

if (!("pearson_phi" %in% measures)) {
  final_df <- final_df[,!stringr::str_detect(colnames(final_df), "p_phi")]
}

if (!("euclidean_distance" %in% measures)) {
  final_df <- final_df[,!stringr::str_detect(colnames(final_df), "euclidean_distance")]
}


# Return output
return(final_df)

# End main function
}




################################################################################
# Support Functions (Will relocate to their own R script later)
################################################################################

get_measures <- function(x) {

  if (NA %in% x$class) {

    return(NULL)

  } else {

  df_made <- FALSE

  if ("categorical" %in% x$class) {



    cat_vars <- x$var_df %>%
      dplyr::group_by(ego_id) %>%
      dplyr::summarize(length = dplyr::n(),
                       diversity = length(unique(alter_val)),
                       num_sim = sum(alter_val == ego_val, na.rm = T),
                       prop_sim = num_sim/length,
                       num_diff = sum(alter_val != ego_val, na.rm = T),
                       prop_diff = num_diff/length,
                       ei_index = (prop_diff - prop_sim)/length) %>%
      dplyr::ungroup() %>%
      dplyr::select(-length, -num_diff, -prop_diff)

    p_phi <- pearson_phi(x$var_df)

    ego_df <- cat_vars %>%
      dplyr::left_join(p_phi, by = "ego_id")

    df_made = TRUE

  }

  if ("continuous" %in% x$class) {

    cont_vars <- x$var_df %>%
      # Setup for eucidean distance
      dplyr::mutate(diff = (alter_val - ego_val)^2) %>%
      # Summarize
      dplyr::group_by(ego_id) %>%
      dplyr::summarize(mean_diff = mean(ego_val - alter_val, na.rm = T),
                       mean_abs_diff = mean(abs(ego_val - alter_val), na.rm = T),
                       length = dplyr::n(),
                       euc_num = sqrt(sum(diff, na.rm = T))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(euclidean_distance = euc_num/length) %>%
      dplyr::select(-euc_num, -length)

    if (df_made == TRUE) {
      ego_df <- ego_df %>%
        dplyr::left_join(cont_vars, by = "ego_id")
    } else {
      ego_df <- cont_vars
    }
  }

  if ("binary" %in% x$class) {

    bin_vars <- x$var_df %>%
      dplyr::group_by(ego_id) %>%
      dplyr::summarize(length = dplyr::n(),
                       num_sim = sum(alter_val == ego_val, na.rm = T),
                       prop_sim = num_sim/length) %>%
      dplyr::ungroup() %>%
      dplyr::select(-length)

    if (df_made == TRUE) {
    ego_df <- ego_df %>%
      dplyr::left_join(bin_vars, by = "ego_id")
    } else {
      ego_df <- bin_vars
    }

  }

  if ("date" %in% x$class) {
    date_vars <- x$var_df %>%
      dplyr::mutate(diff = as.numeric(difftime(ego_val, alter_val, units = "days")),
                    abs_diff = abs(diff)) %>%
      dplyr::group_by(ego_id) %>%
      dplyr::summarize(mean_diff = mean(diff, na.rm = TRUE),
                       mean_abs_diff = mean(abs_diff, na.rm = TRUE)) %>%
      dplyr::ungroup()

    if (df_made == TRUE) {
      ego_df <- ego_df %>%
        dplyr::left_join(date_vars, by = "ego_id")
    } else {
      ego_df <- date_vars
    }

  }

colnames(ego_df) <- paste(colnames(ego_df), x$name, sep = "_")
colnames(ego_df)[[1]] <- "ego_id"

return(ego_df)

}

# End `get_measures`
}



pearson_phi <- function(x) {

  alter_tot <- data.frame(alter_val = x$alter_val) %>% dplyr::count(alter_val) %>%
    dplyr::mutate(n_tot = sum(n),
                  n_prop = n/n_tot) %>%
    dplyr::rename(n_alter_tot = n)

  alter_obs <- x %>% dplyr::group_by(ego_id) %>%
    count(alter_val) %>%
    dplyr::rename(n_alter_obs = n)

  alter_tots <- alter_tot %>% dplyr::left_join(alter_obs, by = "alter_val") %>%
    dplyr::select(ego_id, dplyr::everything()) %>%
    mutate(n_alter_obs = tidyr::replace_na(n_alter_obs, 0)) %>%
    dplyr::arrange(ego_id)

  p_phi <- alter_tots %>%
    dplyr::group_by(ego_id) %>%
    dplyr::mutate(exp_val = n_prop * sum(n_alter_obs),
                  diff = (n_alter_obs - exp_val)^2 / exp_val) %>%
    dplyr::summarize(chisq = sum(diff),
                     length = sum(n_alter_obs)) %>%
    dplyr::mutate(p_phi = sqrt(chisq/length)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-chisq, -length)

  return(p_phi)

}

