train_col_types <- list()
test_col_types <- list()
validate_col_types <- list()
all_col_types <- list()
s_train <- NULL
s_test <- NULL
s_validate <- NULL
if (ra_data$has_train_tbl) {
  train_col_types <- lapply(ra_tbl$tbl_train, class)
  s_train <- ra_tbl$tbl_train %>% head(1)
}
if (ra_data$has_test_tbl) {
  test_col_types <- lapply(ra_tbl$tbl_test, class)
  s_test <- ra_tbl$tbl_test %>% head(1)
}
if (ra_data$has_test_tbl) {
  validate_col_types <- lapply(ra_tbl$tbl_validate, class)
  s_validate <- ra_tbl$tbl_validate %>% head(1)
}
if (ra_data$n_data > 0) {
  all_col_types <- lapply(bind_rows(s_train, s_test, s_validate), class)
}

all_vars <- names(all_col_types)
# 字段一致性检查
if (ra_data$n_data > 1) {
  df_train_col <-
    if (length(train_col_types) == 0) {
      data.frame(var = character(0), train = character(0))
    } else {
      train_col_types %>% stack() %>% "names<-"(c("train", "var"))
    }
  df_test_col <-
    if (length(test_col_types) == 0) {
      data.frame(var = character(0), test = character(0))
    } else {
      test_col_types %>% stack() %>% "names<-"(c("test", "var"))
    }
  df_validate_col <-
    if (length(validate_col_types) == 0) {
      data.frame(var = character(0), validate = character(0))
    } else {
      validate_col_types %>% stack() %>% "names<-"(c("validate", "var"))
    }
  tbl_setting_chk <- data.frame(var = all_vars) %>%
    left_join(df_train_col,by="var") %>%
    left_join(df_test_col,by="var") %>%
    left_join(df_validate_col,by="var") %>%
    mutate(
      level = "warning",
      sample_type = NA_character_,
      train = ifelse(is.na(train), '_NA_', train),
      test = ifelse(is.na(test), '_NA_', test),
      validate = ifelse(is.na(validate), '_NA_', validate)
    ) %>% 
    mutate(chk = ifelse(
      train == test & train == validate,
      NA_character_,
      ifelse(
        (train == '_NA_' &
           test == validate) + (test = '_NA_' &
                                  train == validate) + (validate = '_NA_' &
                                                          train == test) > 0,
        NA_character_,
        sprintf(
          '字段类型不一致，train为%s,test为%s,validate为%s',
          train,
          test,
          validate
        )
      )
    )) %>% 
    filter(!is.na(chk)) %>%
    mutate(chk=as.character(chk)) %>% 
    select(all_of(colnames(tbl_setting_chk))) %>%
    bind_rows(tbl_setting_chk, .)
}