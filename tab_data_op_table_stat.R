

observeEvent(ra_tbl$tbl_train, {
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_table)",
    "stat train "
  )
  req(ra_tbl$tbl_train)
  req(nrow(ra_tbl$tbl_train) > 0)
  tbl_stat <- ra_tbl$tbl_train %>%
    sapply(class) %>%
    stack() %>%
    summarise(
      n = n()
      ,
      factor = sum(values %in% c("factor"))
      ,
      int = sum(values %in% c("integer", "integer64"))
      ,
      num = sum(values %in% c("numeric"))
      ,
      str = sum(values %in% c("character"))
    )
  tbl_stat <- ra_tbl$tbl_train %>%
    ungroup() %>%
    summarise(
      n = n(),
      label_miss = sum(is.na(label), na.rm = T)
      ,
      pos = sum(label, na.rm = T)
    ) %>%
    mutate(rate = pos / n) %>%
    mutate(
      sample_type = "train",
      pct = NA_real_,
      ncol = tbl_stat$n,
      factor = tbl_stat$factor,
      int = tbl_stat$int,
      num = tbl_stat$num,
      str = tbl_stat$str,
      dt = as.character(Sys.time())
    ) %>%
    select(dt, sample_type, n, pct, pos, rate, ncol, factor, int, num, str)
  
  ra_project$tbl_stat <- ra_project$tbl_stat %>%
    filter(sample_type != "train") %>%
    bind_rows(tbl_stat) %>%
    ungroup() %>%
    mutate(tot = sum(ifelse(
      sample_type %in% c("train", "test", "validate"), n, 0
    ))) %>%
    mutate(pct = ifelse(sample_type %in% c("train", "test", "validate"), n /
                          tot, NA)) %>%
    select(dt,
           sample_type,
           n,
           pct,
           pos,
           rate,
           ncol,
           factor,
           int,
           num,
           str)
  
})



observeEvent(ra_tbl$tbl_test, {
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_table)",
    "stat test "
  )
  req(ra_tbl$tbl_test)
  req(nrow(ra_tbl$tbl_test) > 0)
  tbl_stat <- ra_tbl$tbl_test %>%
    sapply(class) %>%
    stack() %>%
    summarise(
      n = n()
      ,
      factor = sum(values %in% c("factor"))
      ,
      int = sum(values %in% c("integer", "integer64"))
      ,
      num = sum(values %in% c("numeric"))
      ,
      str = sum(values %in% c("character"))
    )
  tbl_stat <- ra_tbl$tbl_test %>%
    ungroup() %>%
    summarise(
      n = n(),
      label_miss = sum(is.na(label), na.rm = T)
      ,
      pos = sum(label, na.rm = T)
    ) %>%
    mutate(rate = pos / n) %>%
    mutate(
      sample_type = "test",
      pct = NA_real_,
      ncol = tbl_stat$n,
      factor = tbl_stat$factor,
      int = tbl_stat$int,
      num = tbl_stat$num,
      str = tbl_stat$str,
      dt = as.character(Sys.time())
    ) %>%
    select(dt, sample_type, n, pct, pos, rate, ncol, factor, int, num, str)
  ra_project$tbl_stat <- ra_project$tbl_stat %>%
    filter(sample_type != "test") %>%
    bind_rows(tbl_stat) %>%
    ungroup() %>%
    mutate(tot = sum(ifelse(
      sample_type %in% c("train", "test", "validate"), n, 0
    ))) %>%
    mutate(pct = ifelse(sample_type %in% c("train", "test", "validate"), n /
                          tot, NA)) %>%
    select(dt,
           sample_type,
           n,
           pct,
           pos,
           rate,
           ncol,
           factor,
           int,
           num,
           str)
  
})


observeEvent(ra_tbl$tbl_validate, {
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_table)",
    "stat validate "
  )
  req(ra_tbl$tbl_validate)
  req(nrow(ra_tbl$tbl_validate) > 0)
  
  tbl_stat <- ra_tbl$tbl_validate %>%
    sapply(class) %>%
    stack() %>%
    summarise(
      n = n()
      ,
      factor = sum(values %in% c("factor"))
      ,
      int = sum(values %in% c("integer", "integer64"))
      ,
      num = sum(values %in% c("numeric"))
      ,
      str = sum(values %in% c("character"))
    )
  tbl_stat <- ra_tbl$tbl_validate %>%
    ungroup() %>%
    summarise(
      n = n(),
      label_miss = sum(is.na(label), na.rm = T)
      ,
      pos = sum(label, na.rm = T)
    ) %>%
    mutate(rate = pos / n) %>%
    mutate(
      sample_type = "validate",
      pct = NA_real_,
      ncol = tbl_stat$n,
      factor = tbl_stat$factor,
      int = tbl_stat$int,
      num = tbl_stat$num,
      str = tbl_stat$str,
      dt = as.character(Sys.time())
    ) %>%
    select(dt, sample_type, n, pct, pos, rate, ncol, factor, int, num, str)
  
  ra_project$tbl_stat <- ra_project$tbl_stat %>%
    filter(sample_type != "validate") %>%
    bind_rows(tbl_stat) %>%
    ungroup() %>%
    mutate(tot = sum(ifelse(
      sample_type %in% c("train", "test", "validate"), n, 0
    ))) %>%
    mutate(pct = ifelse(sample_type %in% c("train", "test", "validate"), n /
                          tot, NA)) %>%
    select(dt,
           sample_type,
           n,
           pct,
           pos,
           rate,
           ncol,
           factor,
           int,
           num,
           str)
  
})
