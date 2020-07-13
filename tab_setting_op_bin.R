




# table -------------------------------------------------------------------

output$tab_setting_bin_table <- DT::renderDT({
  req(ra_project$tbl_bin_groups)
  ra_project$tbl_bin_groups %>%
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


output$setting_bin_status <- reactive({
  ra_data$setting_bin_status
})

outputOptions(output, "setting_bin_status", suspendWhenHidden = FALSE)

observeEvent(ra_data$setting_bin_status, {
  if (ra_data$setting_bin_status %in% c("select", "add", "edit")) {
    disable("tab_setting_bin_table")
  } else {
    enable("tab_setting_bin_table")
  }
})


# table select ------------------------------------------------------------
observe({
  if (!is.null(input$tab_setting_bin_table_rows_selected)) {
    ra_data$setting_bin_status <- "tblselect"
  }
})



ra_setting_bin_select_record <- reactive({
  if (is.null(input$tab_setting_bin_table_rows_selected)) {
    return(NULL)
  } else {
    return(ra_project$tbl_bin_groups[input$tab_setting_bin_table_rows_selected,] %>%
             unlist() %>%
             as.list())
  }
})

# select ------------------------------------------------------------------

observeEvent(input$tab_setting_bin_btn_select, {
  req(input$tab_setting_bin_table_rows_selected)
  ra_data$setting_bin_status <- "select"
})



# add value ---------------------------------------------------------------

observeEvent(input$tab_setting_bin_btn_addvalue, {
  ra_data$setting_bin_status <- "add"
})



# confirm add -------------------------------------------------------------

observeEvent(input$tab_setting_bin_btn_add_confirm, {
  ok <- TRUE
  s_id <- nrow(ra_project$tbl_bin_groups) + 1
  s_name <- input$tab_setting_bin_input_name
  s_bin_num_limit <- input$tab_setting_bin_input_bin_num_limit
  s_stop_limit <- input$tab_setting_bin_input_stop_limit
  s_notes <- input$tab_setting_bin_input_notes
  s_count_distr_limit <- input$tab_setting_bin_input_count_distr_limit
  if (is.null(s_name) | str_trim(s_name) == "")
  {
    showModal(modalDialog(title = "信息不全，必须填写name",
                          footer = tagList(modalButton("返回"))))
    ok <- FALSE
  }
  req(ok)
  
  if (s_name %in% ra_project$tbl_bin_groups$name) {
    showModal(modalDialog(
      title = "错误：name已经存在符",
      sprintf("name=%s：已经存在。", s_name),
      footer = tagList(modalButton("返回"))
    ))
    ok <- FALSE
  }
  req(ok)
  
  new_tbl <- data.frame(
      id = as.integer(s_id),
      name = s_name,
      bin_num_limit = as.integer(s_bin_num_limit),
      stop_limit = as.numeric(s_stop_limit),
      value_char = as.numeric(count_distr_limit),
      note = s_notes,
      stringsAsFactors = FALSE
    )
 
  
  ra_project$tbl_bin_groups <-
    bind_rows(new_tbl) 
  ra_data$setting_bin_status <- "view"
  
})


# add cancel --------------------------------------------------------------

observeEvent(input$tab_setting_bin_btn_add_cancel,
             {
               ra_data$setting_bin_status <- "view"
             })


# edit --------------------------------------------------------------------
observeEvent(input$tab_setting_bin_btn_edit, {
  s <- ra_setting_bin_select_record()
  disable("tab_setting_bin_input_name")
  updateTextInput(session, "tab_setting_bin_input_name", value = s$name)
  updateSliderInput(session, "tab_setting_bin_input_bin_num_limit", value = as.integer(s$bin_num_limit))
  updateSliderInput(session, "tab_setting_bin_input_count_distr_limit", value = as.numeric(s$count_distr_limit))
  updateSliderInput(
    session,
    "tab_setting_bin_input_stop_limit",
    value = as.numeric(s$stop_limit)
  )
  updateTextInput(session,"tab_setting_bin_input_notes", value = s$note)
  ra_data$setting_bin_status <- "edit"
})


# confirm edit ------------------------------------------------------------

observeEvent(input$tab_setting_bin_btn_edit_confirm, {
  s <- ra_setting_bin_select_record()
  ra_project$tbl_bin_groups[s$id, "bin_num_limit"] <- as.integer(input$tab_setting_bin_input_bin_num_limit)
  ra_project$tbl_bin_groups[s$id, "count_distr_limit"] <- as.integer(input$tab_setting_bin_input_count_distr_limit)
  ra_project$tbl_bin_groups[s$id, "stop_limit"] <- as.numeric(input$tab_setting_bin_input_stop_limit)
  ra_project$tbl_bin_groups[s$id, "note"] <- input$tab_setting_bin_input_notes
  
  enable("tab_setting_bin_input_name")
  updateTextInput(session,"tab_setting_bin_input_name", value = "")
  ra_data$setting_bin_status <- "view"
})


# edit cancel -------------------------------------------------------------
observeEvent(input$tab_setting_bin_btn_edit_cancel, {
  enable("tab_setting_bin_input_name")
  updateTextInput(session,"tab_setting_bin_input_name", value = "")
  ra_data$setting_bin_status <- "view"
})



# delete value ------------------------------------------------------------
observeEvent(input$tab_setting_bin_btn_delete, {
  s <- ra_setting_bin_select_record()
  s_id <- as.integer(s$id)
  
  ra_project$tbl_bin_groups <-
    ra_project$tbl_bin_groups %>%
    filter(id < s_id) %>%
    bind_rows(
      ra_project$tbl_bin_groups %>%
        filter(id > s_id) %>%
        mutate(id = id - 1)
    )
  ra_data$setting_bin_status <- "view"
})
