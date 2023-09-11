ego_df <- egonet_egos
alter_df <- egonet_alters
ego_prefix = "ego_"
alter_prefix = "alter_"
vars = c("race", "year", "major1")

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

  # 3. Check that variables are in both dataframes



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

