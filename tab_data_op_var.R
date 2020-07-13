
# local reactive values ---------------------------------------------------
ra_tab_data_var <-
  reactiveValues(
    sample_type = c("all", "train", "test", "validate")
    ,
    bin_src = c("all"),
    col = c("sample_type", "cat_1", "cat_2", "type", "var", "desc")
  )


# join train/test/validate/all --------------------------------------------


observeEvent({
  ra_project$tbl_stat_general_train
  ra_project$tbl_stat_general_test
  ra_project$tbl_stat_general_validate
  ra_project$tbl_stat_general_all
  1
}, {
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_var)",
    "tbl_stat_general change "
  )
  ra_project$tbl_stat_general <-
    bind_rows(
      ra_project$tbl_stat_general_train,
      ra_project$tbl_stat_general_test,
      ra_project$tbl_stat_general_validate,
      ra_project$tbl_stat_general_all
    )
  
})


observeEvent({
  ra_project$tbl_stat_special_train
  ra_project$tbl_stat_special_test
  ra_project$tbl_stat_special_validate
  ra_project$tbl_stat_special_all
  1
}, {
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_var)",
    "tbl_stat_special change "
  )
  ra_project$tbl_stat_special <-
    bind_rows(
      ra_project$tbl_stat_special_train,
      ra_project$tbl_stat_special_test,
      ra_project$tbl_stat_special_validate,
      ra_project$tbl_stat_special_all
    )
})
# tbl_varstat ----------------------------------------------------------------
observeEvent({
  ra_project$tbl_stat_general
  ra_project$tbl_stat_special
  ra_project$tbl_stat_cat
  ra_project$tbl_stat_bin
  1
}, {
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_var)",
    "tbl_varstat change "
  )
  
  ra_project$tbl_varstat <- ra_project$tbl_stat_general %>%
    left_join(ra_project$tbl_stat_cat, by = "var", copy = TRUE) %>%
    left_join(
      ra_project$tbl_stat_special,
      by = c("sample_type", "var"),
      copy = TRUE
    ) %>%
    left_join(ra_project$tbl_stat_bin,
              by = c("sample_type", "var"),
              copy = TRUE) %>%
    select(all_of(colnames(psbc_risk_template$tbl_varstat)))
  
  # showNotification("统计/分箱完成")
})




# tab_data_var_tbl --------------------------------------------------------
observeEvent(input$tbl_data_btn_chkgroup_tables, {
  ra_tab_data_var$sample_type <-
    if (is.null(input$tbl_data_var_chkgroup_tables)) {
      c("all", "train", "test", "validate")
    } else {
      input$tbl_data_var_chkgroup_tables
    }
})


observeEvent(input$tbl_data_btn_chkgroup_binsrc, {
  ra_tab_data_var$bin_src <- input$tbl_data_var_chkgroup_binsrc
})

observeEvent(input$tbl_data_btn_chkgroup_cols, {
  ra_tab_data_var$col <- input$tbl_data_var_chkgroup_cols %>%
    sapply(function(x) {
      switch(
        x,
        "miss" = c("miss_n", "miss_p"),
        "special" = c("special_n", "special_p"),
        "unique" = c("unique_n", "unique_p"),
        "rank" = c(
          "r1",
          "r1_n",
          "r1_p",
          "r2",
          "r2_n",
          "r2_p",
          "r3",
          "r3_n",
          "r3_p"
        ),
        "mean" = c("mean", "std"),
        "quantile" = c("min", "p5", "p10", "p25", "p50", "p75", "p90", "p95", "max"),
        "quantile_detail" = c(
          "min",
          "p5",
          "p10",
          "p15",
          "p20",
          "p25",
          "p30",
          "p35",
          "p40",
          "p45",
          "p50",
          "p55",
          "p60",
          "p65",
          "p70",
          "p75",
          "p80",
          "p85",
          "p90",
          "p95",
          "max"
        ),
        "psi" = c("bin_src","psi_tree", "psi_chi", "psi_freq", "psi_manual"),
        "bin" = c(
          "bin_src",
          "bin_tree",
          "bin_chi",
          "bin_freq",
          "bin_manual",
          "iv_tree",
          "iv_chi",
          "iv_freq",
          "iv_manual"
        ),
        "dt" = c("dt_general", "dt_special", "dt_bin")
      )
    }) %>%
    unlist() %>%
    as.vector() %>%
    c(c("sample_type", "cat_1", "cat_2", "type", "var", "desc"), .) %>%
    unique()
})


observe({
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_var)",
    "trigger tab_data_var_table rendering"
  )
  req(ra_project$tbl_varstat)
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_var)",
    "tab_data_var_table rendering - start"
  )
  # filter row and column
  ra_project$tbl_varstat_filtered <- ra_project$tbl_varstat %>%
    filter(
      sample_type %in% ra_tab_data_var$sample_type &
        bin_src %in% ra_tab_data_var$bin_src
    ) %>%
    select(all_of(ra_tab_data_var$col))
  
})



# render data table -------------------------------------------------------




output$tab_data_var_tbl <- renderDT({
  rdt <- ra_project$tbl_varstat_filtered
  cols <- colnames(rdt)
  # get js code
  sample_type <- unique(rdt$sample_type)
  cat_1 <- unique(rdt$cat_1)
  cat_2 <- unique(rdt$cat_2)
  type <- unique(rdt$type)
  colors <-
    rainbow(length(sample_type) + length(cat_1) + length(cat_2) + length(type))
  sample_type <- sample_type %>%
    tbl_bgcolor_css_code(0, ., c(0), colors)
  colors <- colors[length(sample_type) + 1:length(colors)]
  cat_1 <- cat_1 %>%
    tbl_bgcolor_css_code(1, ., c(1), colors)
  colors <- colors[length(cat_1) + 1:length(colors)]
  cat_2 <- cat_2 %>%
    tbl_bgcolor_css_code(2, ., c(2), colors)
  colors <- colors[length(cat_2) + 1:length(colors)]
  type <- type %>%
    tbl_bgcolor_css_code(3, ., c(3, 4, 5), colors)
  js_code <- paste0(sample_type, cat_1, cat_2, type, sep = "\n")
  # cat(js_code)
  
  # apply js code
  rdt <- rdt %>%
    datatable(
      selection = "single",
      filter = "top",
      rownames = FALSE,
      options = list(
        orderClasses = TRUE,
        lengthChange = TRUE,
        searching = TRUE,
        paging = TRUE,
        scrollX = TRUE,
        scrolly = "200px",
        lengthMenu = list(
          c(30, 50, 100, 150, 200,-1),
          c('30', '50', "100", "150", "200", 'All')
        ),
        pageLength = 50
        ,
        rowCallback = DT::JS(sprintf('function(row, data) {
            %s

          }', js_code))
      )
    )
  # formatting
  col_pct <-
    c("miss_p", "unique_p", "special_p", "r1_p", "r2_p", "r3_p")
  col_pct <- col_pct[col_pct %in% cols]
  if (length(col_pct) > 0) {
    rdt <- rdt %>%
      formatPercentage(col_pct, 1)
  }
  col_int <-
    c(
      "miss_n",
      "unique_n",
      "special_n",
      "r1_n",
      "r2_n",
      "r3_n",
      "bin_tree",
      "bin_chi",
      "bin_freq",
      "bin_manual"
    )
  col_int <- col_int[col_int %in% cols]
  if (length(col_int) > 0) {
    rdt <- rdt %>%
      formatRound(col_int, 0)
  }
  col_num <-
    c(
      "mean",
      "std",
      "min",
      "p5",
      "p10",
      "p15",
      "p20",
      "p25",
      "p30",
      "p35",
      "p40",
      "p45",
      "p50",
      "p55",
      "p60",
      "p65",
      "p70",
      "p75",
      "p80",
      "p85",
      "p90",
      "p95",
      "max",
      "iv_tree",
      "iv_chi",
      "iv_freq",
      "iv_manual",
      "psi_tree",
      "psi_chi",
      "psi_freq",
      "psi_manual"
    )
  col_num <- col_num[col_num %in% cols]
  if (length(col_num) > 0) {
    rdt <- rdt %>%
      formatRound(col_num, 3)
  }
  return(rdt)
}
, server = TRUE)



# observe row select ------------------------------------------------------



observeEvent(input$tab_data_var_tbl_rows_selected, {
  my_log("DEBUG",
         ra_config$debug_level,
         "(tab_data_var)",
         "rows selected trigger")
  req(input$tab_data_var_tbl_rows_selected)
  
  var <-
    ra_project$tbl_varstat_filtered$var[input$tab_data_var_tbl_rows_selected]
  
  ra_data$onevar_var <- var
  if (ra_project$tbl_varstat_filtered$type[input$tab_data_var_tbl_rows_selected] %in%
      c("integer", "numeric", "integer64")) {
    ra_data$onevar_type <- "numeric"
  } else {
    ra_data$onevar_type <- "character"
  }
  my_log("DEBUG",
         ra_config$debug_level,
         "(tab_data_var)",
         "rows selected change var=",
         var)
})

observeEvent({
  ra_data$onevar_var
  1
}, {
  req(ra_data$onevar_var)
  req(ra_data$onevar_var != "label")
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "plot data preparing", value = 0)
  
  var <- ra_data$onevar_var
  progress$inc(amount = 0.2, detail = " train")
  df_train <- if (ra_data$has_train_tbl)
  {
    ra_tbl$tbl_train %>%
      select(all_of(c("label", var))) %>%
      "colnames<-"(c("label", "x")) %>%
      mutate(sample_type = "train")
  }
  else
  {
    NULL
  }
  progress$inc(amount = 0.2, detail = " test")
  df_test <- if (ra_data$has_test_tbl)
  {
    ra_tbl$tbl_test %>%
      select(all_of(c("label", var))) %>%
      "colnames<-"(c("label", "x")) %>%
      mutate(sample_type = "test")
  }
  else
  {
    NULL
  }
  progress$inc(amount = 0.2, detail = " validate")
  df_validate <- if (ra_data$has_validate_tbl)
  {
    ra_tbl$tbl_validate %>%
      select(all_of(c("label", var))) %>%
      "colnames<-"(c("label", "x")) %>%
      mutate(sample_type = "validate")
  }
  else
  {
    NULL
  }
  progress$inc(amount = 0.2, detail = " all")
  df <- bind_rows(df_train, df_test, df_validate)
  if (ra_data$n_data > 1) {
    df <- df %>%
      mutate(sample_type = "all") %>%
      bind_rows(df)
  } 
  ra_tbl$tbl_onevar <- df
})


observeEvent({
  ra_data$onevar_var
  1
}, {
  req(ra_data$onevar_var)
  ra_tbl$tbl_onevar_stat <- ra_project$tbl_varstat %>%
    filter(var == ra_data$onevar_var)
})