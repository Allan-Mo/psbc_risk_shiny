
observeEvent({
  input$tab_data_btn_load_alltable
  input$tab_data_btn_load_allfile
  1
}, {
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_table)",
    "button click - loading single file"
  )
  req(ra_project$file_single)
  req(input$tab_data_rdo_tbltype == 'single')
  my_log("DEBUG",
         ra_config$debug_level,
         "(tab_data_table)",
         "loading single table - start")
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Load single data file", value = 0)
  
  ok <- TRUE
  file <-
    path(ra_config$select_project$path, ra_project$file_single)
  base_name <- basename(file)
  obj_name <- gsub("(\\.\\w+)$", "", base_name)
  
  tryCatch({
    progress$inc(amount=0.1,detail="loading")
    load(file)
  }, error = function(e) {
    showModal(modalDialog(title = "single数据无法load()",
                          file,
                          footer = tagList(modalButton("返回修改"))))
    ok <<- FALSE
  })
  req(ok)
  
  chk <- character(0)
  if (!(obj_name %in% ls())) {
    chk <- c(chk, "文件未包含同名object")
  } else if (!("sample_type" %in% colnames(get(obj_name)))) {
    chk <- c(chk, "data.frame无sample_type字段")
  } else if (!("label" %in% colnames(get(obj_name)))) {
    chk <- c(chk, "data.frame无label字段")
  }
  if (length(chk) > 0) {
    showModal(
      modalDialog(
        title = "single数据格式不标准",
        "要求：1.文件包含同名object；\n2.data.frame包含sample_type和label字段；\n3.sample_type的取值包括train/test/validate",
        paste(chk, collapse = "\n"),
        footer = tagList(modalButton("返回修改"))
      )
    )
    ok <- FALSE
  }
  req(ok)
  
  ra_tbl$tbl_train <- NULL   #应该是比较了数据值，如果值不变，不触发重新统计
  ra_tbl$tbl_test <- NULL
  ra_tbl$tbl_validate <- NULL
  progress$inc(amount=0.1,detail="get train")
  ra_tbl$tbl_train <- get(obj_name) %>%
    filter(sample_type == "train")
  progress$inc(amount=0.1,detail="get test")
  ra_tbl$tbl_test <- get(obj_name) %>%
    filter(sample_type == "test")
  progress$inc(amount=0.05,detail="get validate")
  ra_tbl$tbl_validate <- get(obj_name) %>%
    filter(sample_type == "validate")
  omit_nrow <- get(obj_name) %>%
    filter(!(sample_type %in% c("train", "test", "validate"))) %>%
    nrow()
  if (omit_nrow > 0) {
    showModal(modalDialog(
      title = "sample_type包含其他取值",
      sprintf(
        "sample_type包括%d行train/test/validate之外的取值，这些行将被忽略！",
        omit_nrow
      ),
      footer = tagList(modalButton("确定"))
    ))
  }
  rm(list = c(obj_name))
  gc()
  my_log("INFO",
         ra_config$debug_level,
         "(tab_data_table)",
         "loading single table - success")
})