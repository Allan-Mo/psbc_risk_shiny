




# table -------------------------------------------------------------------

output$tab_setting_special_table <- DT::renderDT({
  req(ra_project$tbl_stat)
  ra_project$tbl_special_values %>%
    datatable(
      selection = "single",
      rownames = FALSE,
      options = list(
        lengthChange = TRUE,
        searching = FALSE,
        paging = FALSE,
        scrollX = FALSE,
        scrolly = TRUE
      )
    )
  
}
,
server = TRUE)


# status ------------------------------------------------------------------


output$setting_special_status <- reactive({
  ra_data$setting_special_status
})

outputOptions(output, "setting_special_status", suspendWhenHidden = FALSE)

observeEvent(ra_data$setting_special_status, {
  if (ra_data$setting_special_status %in% c("select", "add", "edit")) {
    disable("tab_setting_special_table")
  } else {
    enable("tab_setting_special_table")
  }
})


# table select ------------------------------------------------------------
observe({
  if (!is.null(input$tab_setting_special_table_rows_selected)) {
    ra_data$setting_special_status <- "tblselect"
  }
})



ra_setting_special_select_record <- reactive({
  if (is.null(input$tab_setting_special_table_rows_selected)) {
    return(NULL)
  } else {
    return(ra_project$tbl_special_values[input$tab_setting_special_table_rows_selected,] %>%
             unlist() %>%
             as.list())
  }
})

# select ------------------------------------------------------------------

observeEvent(input$tab_setting_special_btn_select, {
  req(input$tab_setting_special_table_rows_selected)
  ra_data$setting_special_status <- "select"
})


# add name ----------------------------------------------------------------

observeEvent(input$tab_setting_special_btn_addname, {
  ra_data$setting_special_status <- "add"
})


# add value ---------------------------------------------------------------

observeEvent(input$tab_setting_special_btn_addvalue, {
  req(ra_setting_special_select_record())
  disable("tab_setting_special_input_name")
  disable("tab_setting_special_input_type")
  s <- ra_setting_special_select_record()
  updateTextInput(session, "tab_setting_special_input_name", value = s$name)
  updateSelectInput(session,
                    "tab_setting_special_input_type",
                    selected = s$type)
  ra_data$setting_special_status <- "add"
})



# confirm add -------------------------------------------------------------

observeEvent(input$tab_setting_special_btn_add_confirm, {
  ok <- TRUE
  s_id <- nrow(ra_project$tbl_special_values) + 1
  s_name <- input$tab_setting_special_input_name
  s_type <- input$tab_setting_special_input_type
  s_values <- input$tab_setting_special_input_values
  s_notes <- input$tab_setting_special_input_notes
  s_value_type <- input$tab_setting_special_input_value_type
  if (is.null(s_name) | str_trim(s_name) == ""
      | is.null(s_values) | str_trim(s_values) == "")
  {
    showModal(modalDialog(title = "信息不全，请填写备注和代码",
                          footer = tagList(modalButton("返回"))))
    ok <- FALSE
  }
  req(ok)
  
  s_tbl <- ra_project$tbl_special_values %>%
    filter(name == s_name)
  if (nrow(s_tbl) > 0) {
    if (s_type != s_tbl[1, "type"])
    {
      showModal(modalDialog(
        title = "错误：类型不符",
        sprintf("name=%s：添加的类型%s与已有的类型%s 不同。", s_name, s_type, s_tbl[1, "type"]),
        footer = tagList(modalButton("返回"))
      ))
      ok <- FALSE
    }
    req(ok)
    # 插入时更换id
    s_id <- max(as.integer(s_tbl[["id"]])) + 1
    s_seq <- max(as.integer(s_tbl[["seq"]])) + 1
  } else {
    s_seq <- 1L
  }
  n_values <-
    ifelse(s_value_type == "single", 1L, length(str_split(s_values, ",")[[1]]))
  if (n_values == 1L) {
    new_tbl <- data.frame(
      id = s_id,
      name = s_name,
      type = s_type,
      seq = s_seq,
      value_num = ifelse(s_type == "numeric", as.numeric(s_values), NA_real_),
      value_char = ifelse(s_type == "charecter", s_values, NA_character_),
      note = s_notes,
      stringsAsFactors = FALSE
    )
  }  else {
    n_notes <- length(str_split(s_notes, ",")[[1]])
    if (n_values != n_notes)
    {
      showModal(modalDialog(
        title = "错误：values和notes的数目不同",
        sprintf("values有%d个,notes有%d个。", n_values, n_notes),
        footer = tagList(modalButton("返回"))
      ))
      ok <- FALSE
    }
    req(ok)
    
    new_tbl <- data.frame(
      id = s_id + 0:(n_values - 1),
      name = rep(s_name, n_values),
      type = rep(s_type, n_values),
      seq = s_seq + 0:(n_values - 1),
      value_num = if (s_type == "numeric") {
        as.numeric(str_split(s_values, ",")[[1]])
      } else {
        rep(NA_real_, n_values)
      },
      value_char = if (s_type == "character") {
        str_split(s_values, ",")[[1]]
      } else {
        rep(NA_character_, n_values)
      },
      note = str_split(s_notes, ",")[[1]],
      stringsAsFactors = FALSE
    )
  }
  
  ra_project$tbl_special_values <-
    ra_project$tbl_special_values %>%
    filter(id < s_id) %>%
    bind_rows(new_tbl) %>%
    bind_rows(ra_project$tbl_special_values %>%
                filter(id >= s_id) %>%
                mutate(id = id + n_values))
  enable("tab_setting_special_input_name")
  enable("tab_setting_special_input_type")
  ra_data$setting_special_status <- "view"
  
})


# add cancel --------------------------------------------------------------

observeEvent(input$tab_setting_special_btn_add_cancel,
             {
               enable("tab_setting_special_input_name")
               enable("tab_setting_special_input_type")
               ra_data$setting_special_status <- "view"
             })


# edit --------------------------------------------------------------------
observeEvent(input$tab_setting_special_btn_edit, {
  s <- ra_setting_special_select_record()
  disable("tab_setting_special_input_name")
  updateTextInput(session, "tab_setting_special_input_name", value = s$name)
  disable("tab_setting_special_input_type")
  updateSelectInput(session, "tab_setting_special_input_type", selected = s$type)
  disable("tab_setting_special_input_value_type")
  updateSelectInput(session, "tab_setting_special_input_value_type", selected = "single")
  updateTextInput(
    session,
    "tab_setting_special_input_values",
    value = ifelse(s$type == "numeric", s$value_num, s$value_char)
  )
  updateTextInput(session,"tab_setting_special_input_notes", value = s$note)
  ra_data$setting_special_status <- "edit"
})


# confirm edit ------------------------------------------------------------

observeEvent(input$tab_setting_special_btn_edit_confirm, {
  s <- ra_setting_special_select_record()
  
  ra_project$tbl_special_values[s$id, "value_num"] <-
    ifelse(
      s$type == "numeric",
      as.integer(input$tab_setting_special_input_values),
      NA_real_
    )
  ra_project$tbl_special_values[s$id, "value_char"] <-
    ifelse(s$type == "character",
           input$tab_setting_special_input_values,
           NA_character_)
  ra_project$tbl_special_values[s$id, "note"] <-
    input$tab_setting_special_input_notes
  enable("tab_setting_special_input_name")
  updateTextInput(session,"tab_setting_special_input_name", value = "")
  enable("tab_setting_special_input_type")
  updateSelectInput(session,"tab_setting_special_input_type", selected = "")
  enable("tab_setting_special_input_value_type")
  updateTextInput(session,"tab_setting_special_input_values", value = "")
  updateTextInput(session,"tab_setting_special_input_notes", value = "")
  ra_data$setting_special_status <- "view"
})


# edit cancel -------------------------------------------------------------
observeEvent(input$tab_setting_special_btn_edit_cancel, {
  enable("tab_setting_special_input_name")
  updateTextInput(session,"tab_setting_special_input_name", value = "")
  enable("tab_setting_special_input_type")
  updateSelectInput(session,"tab_setting_special_input_type", selected = "")
  enable("tab_setting_special_input_value_type")
  updateTextInput(session,"tab_setting_special_input_values", value = "")
  updateTextInput(session,"tab_setting_special_input_notes", value = "")
  ra_data$setting_special_status <- "view"
})



# delete value ------------------------------------------------------------
observeEvent(input$tab_setting_special_btn_delete, {
  s <- ra_setting_special_select_record()
  s_id <- as.integer(s$id)
  s_name <- s$name
  
  ra_project$tbl_special_values <-
    ra_project$tbl_special_values %>%
    filter(id < s_id) %>%
    bind_rows(
      ra_project$tbl_special_values %>%
        filter(id > s_id) %>%
        mutate(id = id - 1, seq = ifelse(name == s_name, seq - 1, seq))
    )
  ra_data$setting_special_status <- "view"
})
