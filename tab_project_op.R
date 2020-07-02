

# 项目表 -----------------------------------------------------------------------

output$tab_project_table <- DT::renderDT({
  ra_config$df_project %>%
    DT::datatable(
      selection = "single",
      rownames = FALSE,
      options = list(lengthChange = TRUE)
    )
},
server = TRUE)

# 响应从表中选择行 --------------------------------------------------------------------
ra_project_select_record <- reactive({
  ra_config$df_project[input$tab_project_table_rows_selected, ]
})

output$tab_project_text_select <- renderPrint({
  s = ra_project_select_record()
  if (nrow(s) > 0) {
    cat(sprintf('是否选择第%s个项目:%s', s$id[1], s$name_cn[1]))
    enable("tab_project_btn_select")
  } else {
    cat("从下面列表中选择项目")
    disable("tab_project_btn_select")
  }
})


# 响应选择项目 ------------------------------------------------------------------

observeEvent(input$tab_project_btn_select, {
  s <- ra_project_select_record()
  if (nrow(s) > 0) {
    s <- as.list(unlist(s))
    # 写入config
    ra_config$select_project <- s
    # 加载project数据
    file <- path(s$path, "project.Rdata")
    if (file.exists(file)) {
      load(file)
      for (name in names(project)) {
        ra_project[[name]] <- project[[name]]
      }
    }
    # 空表结构
    if (is.null(ra_project$tbl_stat)) {
      ra_project$tbl_stat <- psbc_risk_template$tbl_stat
    }
    
    if (is.null(ra_project$tbl_mutate)) {
      ra_project$tbl_mutate <- psbc_risk_template$mutate
    }
    if (is.null(ra_project$tbl_varstat)) {
      ra_project$tbl_varstat <- psbc_risk_template$varstat
    }
    # 模板文件
    if (is.null(ra_project$file_special)) {
      relative_file <- "/特殊值_模板.txt"
      file <- path(ra_config$select_project$path, relative_file)
      psbc_risk_template$special %>% 
        write.table(file,row.names=FALSE,quote=FALSE,col.names = FALSE)
      ra_project$special_values <- list(values=psbc_risk_template$special,dt=Sys.time())
      ra_project$file_special <- relative_file
    if (is.null(ra_project$file_setting)) {
      relative_file <- "/参数文件_模板.xlsx"
      file <- path(ra_config$select_project$path, relative_file)
      
    }
      
        
      
      
    }
    
  }
  
})

# 显示选择项目
output$tab_project_text_current <- renderPrint({
  if (!is.null(ra_config$select_project)) {
    s <- ra_config$select_project
    cat(sprintf("第%s个项目:%s", s$id, s$name_cn))
  }
})

# 显示tab
observeEvent(input$tab_project_btn_select, {
  show(selector = '#navbar li a[data-value="data"]')
  show(selector = '#navbar li a[data-value="bin"]')
  show(selector = '#navbar li a[data-value="scorecard"]')
})




# 添加项目 (proxy的方法没搞定)--------------------------------------------------------------------

# tab_project_op_proxy <-  dataTableProxy('tab_project_table')
# observeEvent(input$tab_project_btn_add, {
#   tab_project_op_proxy %>%
#     addRow(
#       list(
#         id = nrow(config$df_project) + 1,
#         name = input$tab_project_add_name,
#         name_cn = input$tab_project_add_name_cn,
#         owner = "莫运政",
#         create_time = Sys.time(),
#         modify_time = Sys.time(),
#         path = input$tab_project_add_path
#       )
#     )
# })

# 添加项目 --------------------------------------------------------------------
volumes <- c(getVolumes()())
shinyDirChoose(input, 'tab_project_add_path', roots = volumes)
ra_tab_project_path <- reactive({
  if (is.integer(input$tab_project_add_path)) {
    return("")
  } else {
    return(parseDirPath(volumes, input$tab_project_add_path))
  }
})
output$tab_project_add_path_text <- renderPrint({
  ra_tab_project_path()
})

observeEvent(input$tab_project_btn_add, {
  path <- as.character(ra_tab_project_path())
  if (!isTruthy(input$tab_project_add_name)
      | !isTruthy(input$tab_project_add_name_cn)
      | !isTruthy(input$tab_project_add_owner)
      | !isTruthy(path)) {
    showModal(modalDialog(
      title = "无法创建项目",
      "信息填写不完整",
      footer = tagList(modalButton("返回修改"))
    ))
  } else {
    ra_config$df_project <- data.frame(
      id = nrow(ra_config$df_project) + 1,
      name = input$tab_project_add_name,
      name_cn = input$tab_project_add_name_cn,
      owner = input$tab_project_add_owner,
      create_time = Sys.time(),
      modify_time = Sys.time(),
      path = path
    ) %>%
      bind_rows(ra_config$df_project)
  }
  
  
})
