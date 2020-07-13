observeEvent({
  input$tab_data_btn_load_allfile
  input$tab_data_btn_load_setting
  1
}, {
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_table)",
    "button click - loading setting file=",ra_project$file_setting
  )
  req(ra_project$file_setting)
  my_log("DEBUG",
         ra_config$debug_level,
         "(tab_data_table)",
         "loading setting file - start")
  ok <- TRUE
  file <-
    path(ra_config$select_project$path, ra_project$file_setting)
  base_name <- basename(file)
  my_log("DEBUG",
         ra_config$debug_level,
         "(tab_data_table)",
         "file=",
         file)
  tryCatch({
    sheets <- excel_sheets(file)
  }
  , error = function(e) {
    my_log(
      "DEBUG",
      ra_config$debug_level,
      "(tab_data_table)",
      "loading setting file - excel_sheets error"
    )
    showModal(modalDialog(
      title = "无法读取文件",
      "请检查文件是否存在、被占用(excel打开)；",
      file,
      footer = tagList(modalButton("返回修改"))
    ))
    ok <<- FALSE
  })
  req(ok)
  chk <- c()
  if (!"special_values" %in% sheets) {
    chk <- c(chk, "sheet不存在：special_values；")
  }
  if (!"vars" %in% sheets) {
    chk <- c(chk, "sheet不存在：vars；")
  }
  if (length(chk) > 0) {
    showModal(modalDialog(
      title = "参数文件sheet错误",
      paste0(chk, collapse = ""),
      footer = tagList(modalButton("返回修改"))
    ))
    ok <- FALSE
  }
  req(ok)
  # 读取错误
  tbl_list <- list()
  s <- tryCatch({
    tbl_list$special_values <-
      read_excel(file, sheet = "special_values")
    tbl_list$vars <- read_excel(file, sheet = "vars")
    
  }, error = function(e) {
    showModal(modalDialog(title = "参数文件读取错误",
                          file,
                          footer = tagList(modalButton("返回修改"))))
    ok <<- FALSE
  })
  req(ok)
  
  chk <- character(0)
  col_diff <-
    setdiff(colnames(ra_project$tbl_special_values),
            colnames(tbl_list$special_values))
  if (length(col_diff) > 0) {
    chk <- col_diff %>%
      paste0(collapse = ",") %>%
      sprintf("special vaues的sheet中不存着这些列：%s；", .) %>%
      c(chk, .)
  }
  col_diff <-
    setdiff(colnames(ra_project$tbl_vars), colnames(tbl_list$vars))
  if (length(col_diff) > 0) {
    chk <- col_diff %>%
      paste0(collapse = ",") %>%
      sprintf("special vaues的sheet中不存着这些列：%s；", .) %>%
      c(chk, .)
  }
  if (length(chk) > 0) {
    showModal(modalDialog(
      title = "参数文件的列不全",
      paste0(chk, collapse = ""),
      footer = tagList(modalButton("返回修改"))
    ))
    ok <- FALSE
  }
  req(ok)
  
  showModal(modalDialog(
    title = "参数文件将被覆盖",
    "请确认",
    footer = tagList(
      actionButton("tab_data_table_tbn_confirm_setting", "确认覆盖"),
      modalButton("取消")
    )
  ))
  
  
})




observeEvent(input$tab_data_table_tbn_confirm_setting, {
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_stat)",
    "confirm setting replace "
  )
  file <-
    path(ra_config$select_project$path, ra_project$file_setting)
  ra_project$tbl_special_values <- psbc_risk_template$tbl_special_values  #------------------------触发
  ra_project$tbl_vars <- psbc_risk_template$tbl_vars
  ra_project$tbl_special_values <- "special_values" %>%
    read_excel(file, sheet = .) %>%
    select(all_of(colnames(ra_project$tbl_special_values)))
  ra_project$tbl_vars <- "vars" %>%
    read_excel(file, sheet = .) %>%
    select(all_of(colnames(ra_project$tbl_vars)))
  ra_data$file_setting_time <- as.character(Sys.time())
  ra_data$setting_special_status <- "view"
  ra_data$setting_vars <- "view"
  removeModal()
  my_log("INFO",
         ra_config$debug_level,
         "(tab_data_table)",
         "loading setting file - success")
})
