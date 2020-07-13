

# 项目表 -----------------------------------------------------------------------

output$tab_project_table <- DT::renderDT({
  ra_config$tbl_project %>%
    DT::datatable(
      selection = "single",
      rownames = FALSE,
      options = list(lengthChange = TRUE,orderClasses = TRUE)
    )
},
server = TRUE)

# 响应从表中选择行 --------------------------------------------------------------------
ra_project_select_record <- reactive({
  if (!is.null(input$tab_project_table_rows_selected)) {
    my_log("DEBUG",ra_config$debug_level,"select row",input$tab_project_table_rows_selected)
  }
  
  ra_config$tbl_project[input$tab_project_table_rows_selected, ]
})

output$tab_project_text_select <- renderPrint({
  if (is.null(ra_project_select_record())) {
    return()
  }
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
  my_log("INFO",ra_config$debug_level,"button click - select project")
  # 显示tab
  show(selector = '#navbar li a[data-value="data"]')
  show(selector = '#navbar li a[data-value="setting"]')
  show(selector = '#navbar li a[data-value="bin"]')
  show(selector = '#navbar li a[data-value="var"]')
  show(selector = '#navbar li a[data-value="psi"]')
  show(selector = '#navbar li a[data-value="scorecard"]')
  show(selector = '#navbar li a[data-value="ml"]')
  show(selector = '#navbar li a[data-value="report"]')
  s <- ra_project_select_record()
  if (nrow(s) > 0) {
    s <- as.list(unlist(s))
    # 写入config
    ra_config$select_project <- s
    # project数据初始化
    my_log("DEBUG",ra_config$debug_level,"project initiating")
    source("tab_project_op_init.R",local=TRUE,encoding = "UTF-8")
    
  }
  
})

# 显示选择项目
output$tab_project_text_current <- renderPrint({
  if (!is.null(ra_config$select_project)) {
    s <- ra_config$select_project
    cat(sprintf("第%s个项目:%s", s$id, s$name_cn))
  }
})





# 添加项目 (proxy的方法没搞定)--------------------------------------------------------------------

# tab_project_op_proxy <-  dataTableProxy('tab_project_table')
# observeEvent(input$tab_project_btn_add, {
#   tab_project_op_proxy %>%
#     addRow(
#       list(
#         id = nrow(config$tbl_project) + 1,
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
    ra_config$tbl_project <- data.frame(
      id = nrow(ra_config$tbl_project) + 1,
      name = input$tab_project_add_name,
      name_cn = input$tab_project_add_name_cn,
      owner = input$tab_project_add_owner,
      create_time = Sys.time(),
      modify_time = Sys.time(),
      path = path
    ) %>%
      bind_rows(ra_config$tbl_project)
  }
  
  
})


# log ---------------------------------------------------------------------

observeEvent(input$tab_project_select_debug,{
  ra_config$debug_level <- input$tab_project_select_debug
})

