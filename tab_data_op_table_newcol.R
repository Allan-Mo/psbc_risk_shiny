observeEvent(ra_tbl$tbl_train,{
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_table)",
    "newcol train "
  )
  req(ra_tbl$tbl_train)
  lst_d <- colnames(ra_tbl$tbl_train)
  lst_o <- ra_project$lst_var_train_col
  d_cols <- names(lst_d)
  o_cols <- names(lst_o)
  new_col <- sapply(1:length(lst_d),function(i){
    col <- d_cols[i]
    if ((!col %in% o_cols)) {
      return(col)
    } else if (lst_d[[col]] != lst_o[[col]]) {
      return(col)
    } else {
      return(NULL)
    }
  }) %>% unlist() %>% as.vector()
  if (length(new_col)>0) {
    ra_project$lst_var_train_newcol <- c(ra_project$lst_var_train_newcol,new_col)
  }
})

observeEvent(ra_tbl$tbl_test,{
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_table)",
    "newcol test "
  )
  req(ra_tbl$tbl_test)
  lst_d <- colnames(ra_tbl$tbl_test)
  lst_o <- ra_project$lst_var_test_col
  d_cols <- names(lst_d)
  o_cols <- names(lst_o)
  new_col <- sapply(1:length(lst_d),function(i){
    col <- d_cols[i]
    if ((!col %in% o_cols)) {
      return(col)
    } else if (lst_d[[col]] != lst_o[[col]]) {
      return(col)
    } else {
      return(NULL)
    }
  }) %>% unlist() %>% as.vector()
  if (length(new_col)>0) {
    ra_project$lst_var_test_newcol <- c(ra_project$lst_var_test_newcol,new_col)
  }
})

observeEvent(ra_tbl$tbl_validate,{
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_table)",
    "newcol validate "
  )
  req(ra_tbl$tbl_validate)
  lst_d <- colnames(ra_tbl$tbl_validate)
  lst_o <- ra_project$lst_var_validate_col
  d_cols <- names(lst_d)
  o_cols <- names(lst_o)
  new_col <- sapply(1:length(lst_d),function(i){
    col <- d_cols[i]
    if ((!col %in% o_cols)) {
      return(col)
    } else if (lst_d[[col]] != lst_o[[col]]) {
      return(col)
    } else {
      return(NULL)
    }
  }) %>% unlist() %>% as.vector()
  if (length(new_col)>0) {
    ra_project$lst_var_validate_newcol <- c(ra_project$lst_var_validate_newcol,new_col)
  }
})