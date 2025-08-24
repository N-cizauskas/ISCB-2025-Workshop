# function for smd calculation:
process_data <- function(df1, df2, var_names, table_list_name, smd_list_prefix, i) {
  # merge df
  df1$Key <- 1:nrow(df1)
  df2$Key <- 1:nrow(df2)
  merged_df <- merge(df1, df2, by = "Key", all = TRUE)
  merged_df <- merged_df[, -which(names(merged_df) == "Key")]

  # make a single df
  combined_df <- data.frame(
    lapply(var_names, function(var) {
      c(merged_df[[paste0(var, ".x")]], merged_df[[paste0(var, ".y")]])
    })
  )
  colnames(combined_df) <- var_names

  # save table
  temp <- get(table_list_name, envir = .GlobalEnv)
  temp[[i]] <- CreateTableOne(vars = var_names, data = combined_df, test = FALSE)
  assign(table_list_name, temp, envir = .GlobalEnv)

  # calculate smd
  for (var in var_names) {
    x <- merged_df[[paste0(var, ".x")]]
    y <- merged_df[[paste0(var, ".y")]]

    pooled_sd <- sqrt((sd(x, na.rm = TRUE)^2 + sd(y, na.rm = TRUE)^2) / 2)
    mean_diff <- mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)
    smd_value <- mean_diff / pooled_sd

    smd_list_name <- paste0(smd_list_prefix, "_", var, "_smd_all")
    smd_list <- get(smd_list_name, envir = .GlobalEnv)
    smd_list[[i]] <- smd_value
    assign(smd_list_name, smd_list, envir = .GlobalEnv)
  }
}



# function for making empty table lists
create_table_lists <- function(names, n) {
  for (name in names) {
    assign(name, vector("list", length = n), envir = .GlobalEnv)
  }
}
