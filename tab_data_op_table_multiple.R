observeEvent({
  input$tab_data_btn_load_alltable
  input$tab_data_btn_load_allfile
  1
}, {
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_table)",
    "button click - loading train table"
  )
  ok <- TRUE
  req(ra_project$file_train)
  req(input$tab_data_rdo_tbltype == 'split')
  
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "loading train table", value = 0)
  
  my_log("DEBUG",
         ra_config$debug_level,
         "(tab_data_table)",
         "loading train table - start")
  file <-
    path(ra_config$select_project$path, ra_project$file_train)
  base_name <- basename(file)
  obj_name <- gsub("(\\.\\w+)$", "", base_name)
  
  tryCatch({
    load(file)
  }, error = function(e) {
    showModal(modalDialog(title = "train数据无法load()",
                          file,
                          footer = tagList(modalButton("返回修改"))))
    ok <<- FALSE
  })
  req(ok)
  
  chk <- character(0)
  if (!(obj_name %in% ls())) {
    chk <- c(chk, "文件未包含同名object")
  } else if (!("label" %in% colnames(get(obj_name)))) {
    chk <- c(chk, "data.frame无label字段")
  }
  if (length(chk) > 0) {
    showModal(
      modalDialog(
        title = "train数据格式不标准",
        "要求：1.文件包含同名object；\n2.data.frame包含label字段；",
        paste(chk, collapse = "\n"),
        footer = tagList(modalButton("返回修改"))
      )
    )
    ok <- FALSE
  }
  req(ok)
  
  ra_tbl$tbl_train <- NULL
  ra_tbl$tbl_train <- get(obj_name)
  rm(list = c(obj_name))
  gc()
  my_log("INFO",
         ra_config$debug_level,
         "(tab_data_table)",
         "loading traing table - success")
  
})


# 测试文件 & 全部导入
observeEvent({
  input$tab_data_btn_load_alltable
  input$tab_data_btn_load_allfile
  1
}, {
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_table)",
    "button click - loading test table"
  )
  
  req(ra_project$file_test)
  req(input$tab_data_rdo_tbltype == 'split')
  
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "loading test table", value = 0)
  
  my_log("DEBUG",
         ra_config$debug_level,
         "(tab_data_table)",
         "loading test table - start")
  ok <- TRUE
  file <-
    path(ra_config$select_project$path, ra_project$file_test)
  base_name <- basename(file)
  obj_name <- gsub("(\\.\\w+)$", "", base_name)
  
  tryCatch({
    load(file)
  }, error = function(e) {
    showModal(modalDialog(title = "test数据无法load()",
                          file,
                          footer = tagList(modalButton("返回修改"))))
    ok <<- FALSE
  })
  req(ok)
  chk <- character(0)
  if (!(obj_name %in% ls())) {
    chk <- c(chk, "文件未包含同名object")
  } else if (!("label" %in% colnames(get(obj_name)))) {
    chk <- c(chk, "data.frame无label字段")
  }
  if (length(chk) > 0) {
    showModal(
      modalDialog(
        title = "test数据格式不标准",
        "要求：1.文件包含同名object；\n2.data.frame包含label字段；",
        paste(chk, collapse = "\n"),
        footer = tagList(modalButton("返回修改"))
      )
    )
    ok <- FALSE
  }
  req(ok)
  ra_tbl$tbl_test <- NULL
  ra_tbl$tbl_test <- get(obj_name)
  rm(list = c(obj_name))
  gc()
  my_log("INFO",
         ra_config$debug_level,
         "(tab_data_table)",
         "loading test table - success")
})

# 验证文件 & 全部导入
observeEvent({
  input$tab_data_btn_load_alltable
  input$tab_data_btn_load_allfile
  1
}, {
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_table)",
    "button click - loading validate table"
  )
  req(ra_project$file_validate)
  req(input$tab_data_rdo_tbltype == 'split')
  
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "loading validate table", value = 0)
  
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_table)",
    "loading validate table - start"
  )
  ok <- TRUE
  file <-
    path(ra_config$select_project$path, ra_project$file_validate)
  base_name <- basename(file)
  obj_name <- gsub("(\\.\\w+)$", "", base_name)
  tryCatch({
    load(file)
  }, error = function(e) {
    showModal(modalDialog(title = "validate数据无法load()",
                          file,
                          footer = tagList(modalButton("返回修改"))))
    ok <<- FALSE
  })
  req(ok)
  
  chk <- character(0)
  if (!(obj_name %in% ls())) {
    chk <- c(chk, "文件未包含同名object")
  } else if (!("label" %in% colnames(get(obj_name)))) {
    chk <- c(chk, "data.frame无label字段")
  }
  if (length(chk) > 0) {
    showModal(
      modalDialog(
        title = "validate数据格式不标准",
        "要求：1.文件包含同名object；\n2.data.frame包含label字段；",
        paste(chk, collapse = "\n"),
        footer = tagList(modalButton("返回修改"))
      )
    )
    ok <- FALSE
  }
  req(ok)
  ra_tbl$tbl_validate <- NULL
  ra_tbl$tbl_validate <- get(obj_name)
  rm(list = c(obj_name))
  gc()
  my_log(
    "INFO",
    ra_config$debug_level,
    "(tab_data_table)",
    "loading validate table - success"
  )
  
})