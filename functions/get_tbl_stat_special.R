get_tbl_stat_special <- function(df,
                                 vars,
                                 lst_setting,
                                 sample_type) {
  rst <- psbc_risk_template$tbl_stat_special
  for (col in vars) {
    rst <- rst %>%
      add_row(var = col)
  }
  
  if (is.null(df) ||
      nrow(df) == 0) {
    return(rst)
  }
  
  n_row <- nrow(df)
  dt <- as.character(Sys.time())
  rst <- lapply(vars, function(var) {
    special_n <- if (isTRUE(lst_setting[[var]]$special_forceNULL)) {
      NA_integer_
    } else
      (sum(df[[var]] %in% lst_setting[[var]]$special_values))
    data.frame(
      sample_type = sample_type,
      var = var,
      special_n = special_n,
      stringsAsFactors = FALSE
    )
  }) %>%
    rbindlist() %>%
    mutate(special_p = special_n / n_row,
           dt_special = dt)
  
  return(rst)
}


# testing -----------------------------------------------------------------



# df <- data.frame(x = 1, y = "a", z = 3)
# vars <- c("x", "y", "z")
# lst_special_values <-
#   list(
#     default_num = list(type = "numeric", values = c(1, 2, 3)),
#     default_char = list(type = "character", values =
#                           c("_NULL_"))
#   )
# lst_vars <-
#   list(
#     z = list(type = "numeric", special_values = list(type="numeric",values=c(3, 4, 5))),
#     a = list(type = "character",
#              special_values =
#                list(forceNULL = TRUE))
#   )
# sample_type <- "train"
#
# get_tbl_stat_special(df,
#                      vars,
#                      lst_special_values,
#                      lst_vars,
#                      sample_type)