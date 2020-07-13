output$tab_bin_text_setting <- renderPrint({
  req(ra_data$onevar_var)
  if (input$tab_bin_rdo_binsrc == "train") {
    print(ra_project$lst_bin_train_for_train$lst_setting[[ra_data$onevar_var]])
  } else {
    print(ra_project$lst_bin_all_for_train$lst_setting[[ra_data$onevar_var]])
  }
})

observeEvent(input$tab_bin_rdo_binsrc,{
  df <- if (input$tab_bin_rdo_binsrc == "train") {
    ra_project$lst_bin_train_for_train$lst_bin[[ra_data$onevar_var]]$tree
  } else {
    ra_project$lst_bin_all_for_train$lst_bin[[ra_data$onevar_var]]$tree
  }
  req(df)
  output$tab_bin_plot_train_tree <- renderPlot({
    woebin_plot(df)
  })
  output$tab_bin_table_train_tree <- DT::renderDT({
    df %>%
      datatable(
        selection = "none",
        rownames = FALSE,
        options = list(
          orderClasses = TRUE,
          lengthChange = FALSE,
          searching = FALSE,
          paging = FALSE,
          scrollX = FALSE
        )
      ) %>%
      formatPercentage(c("count_distr", "badprob"), 1) %>%
      formatRound(c("woe", "bin_iv", "total_iv"))
    
  }
  ,
  server = TRUE)
  
})


observeEvent(input$tab_bin_rdo_binsrc,{
  df <- if (input$tab_bin_rdo_binsrc == "train") {
    ra_project$lst_bin_train_for_test$lst_bin[[ra_data$onevar_var]]$tree
  } else {
    ra_project$lst_bin_all_for_test$lst_bin[[ra_data$onevar_var]]$tree
  }
  req(df)
  output$tab_bin_plot_test_tree <- renderPlot({
    woebin_plot(df)
  })
  output$tab_bin_table_test_tree <- DT::renderDT({
    df %>%
      datatable(
        selection = "none",
        rownames = FALSE,
        options = list(
          orderClasses = TRUE,
          lengthChange = FALSE,
          searching = FALSE,
          paging = FALSE,
          scrollX = FALSE
        )
      ) %>%
      formatPercentage(c("count_distr", "badprob"), 1) %>%
      formatRound(c("woe", "bin_iv", "total_iv"))
    
  }
  ,
  server = TRUE)
  
})

observeEvent(input$tab_bin_rdo_binsrc,{
  df <- if (input$tab_bin_rdo_binsrc == "train") {
    ra_project$lst_bin_train_for_validate$lst_bin[[ra_data$onevar_var]]$tree
  } else {
    ra_project$lst_bin_all_for_validate$lst_bin[[ra_data$onevar_var]]$tree
  }
  req(df)
  output$tab_bin_plot_validate_tree <- renderPlot({
    woebin_plot(df)
  })
  output$tab_bin_table_validate_tree <- DT::renderDT({
    df %>%
      datatable(
        selection = "none",
        rownames = FALSE,
        options = list(
          orderClasses = TRUE,
          lengthChange = FALSE,
          searching = FALSE,
          paging = FALSE,
          scrollX = FALSE
        )
      ) %>%
      formatPercentage(c("count_distr", "badprob"), 1) %>%
      formatRound(c("woe", "bin_iv", "total_iv"))
    
  }
  ,
  server = TRUE)
  
})


observeEvent(input$tab_bin_rdo_binsrc,{
  df <- if (input$tab_bin_rdo_binsrc == "train") {
    ra_project$lst_bin_train_for_all$lst_bin[[ra_data$onevar_var]]$tree
  } else {
    ra_project$lst_bin_all_for_all$lst_bin[[ra_data$onevar_var]]$tree
  }
  req(df)
  output$tab_bin_plot_all_tree <- renderPlot({
    woebin_plot(df)
  })
  output$tab_bin_table_all_tree <- DT::renderDT({
    df %>%
      datatable(
        selection = "none",
        rownames = FALSE,
        options = list(
          orderClasses = TRUE,
          lengthChange = FALSE,
          searching = FALSE,
          paging = FALSE,
          scrollX = FALSE
        )
      ) %>%
      formatPercentage(c("count_distr", "badprob"), 1) %>%
      formatRound(c("woe", "bin_iv", "total_iv"))
    
  }
  ,
  server = TRUE)
  
})