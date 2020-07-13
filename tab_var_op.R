

# ra_data$tbl_onevar

# selected var ------------------------------------------------------------
output$tab_var_text_var <- renderText({
  req(ra_data$onevar_var)
  sprintf("当前变量 : %s",ra_data$onevar_var)
})



# time --------------------------------------------------------------------


output$tab_var_tbl_stat_p1 <- renderDT({
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_table_p1)",
    "trigger "
  )
  req(ra_tbl$tbl_onevar_stat)
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_table_p1)",
    "start "
  )
  ra_tbl$tbl_onevar_stat %>% 
    select(sample_type, cat_1, cat_2, type, var, desc,bin_src,dt_general,dt_special,dt_bin) %>% 
    datatable(
      selection = "none",
      rownames = FALSE,
      options = list(
        orderClasses = TRUE,
        lengthChange = TRUE,
        searching = FALSE,
        paging = FALSE,
        scrollX = TRUE
      )
    ) 
},server = TRUE)


# general stat ------------------------------------------------------------


output$tab_var_tbl_stat_p2 <- renderDT({
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_table_p2)",
    "trigger "
  )
  req(ra_tbl$tbl_onevar_stat)
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_table_p2)",
    "start "
  )
  ra_tbl$tbl_onevar_stat %>% 
    select(sample_type, cat_1, cat_2, type, var, desc,miss_n,miss_p,unique_n,unique_p,special_n,special_p) %>% 
    distinct() %>% 
    datatable(
      selection = "none",
      rownames = FALSE,
      options = list(
        orderClasses = TRUE,
        lengthChange = TRUE,
        searching = FALSE,
        paging = FALSE,
        scrollX = TRUE
      )
    ) %>% 
    formatPercentage(c("miss_p", "unique_p", "special_p"),1) %>% 
    formatRound(c("miss_n","unique_n","special_n"), 0)
},server = TRUE)



output$tab_var_plot_miss <- renderPlotly({
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_plot_miss)",
    "trigger "
  )
  req(ra_tbl$tbl_onevar_stat)
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_plot_miss)",
    "start "
  )
  ra_tbl$tbl_onevar_stat %>% 
    select(sample_type,miss_n,miss_p) %>% 
    distinct() %>% 
    plot_ly(x = ~ sample_type) %>%
    add_bars(y = ~ miss_n,name="missing") %>%
    add_lines(y = ~ miss_p, yaxis = "y2",name="pct") %>%
    add_markers(y = ~ miss_p, yaxis = "y2") %>%
    layout(
      title="缺失值",
      showlegend = FALSE,
      yaxis2 = list(
        overlaying = "y",
        side = "right",
        tickformat = "%",
        title = ""
      ),
      yaxis = list(
        showgrid = FALSE,
        tickformat = ",",
        title = ""
      ),
      xaxis = list(showgrid = FALSE, title = ""),
      margin=list(l = 50,r = 50,b = 50,t = 50,pad = 4)
    )
})



output$tab_var_plot_unique <- renderPlotly({
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_plot_unique)",
    "trigger "
  )
  req(ra_tbl$tbl_onevar_stat)
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_plot_unique)",
    "start "
  )
  ra_tbl$tbl_onevar_stat %>% 
    select(sample_type,unique_n,unique_p) %>% 
    distinct() %>% 
    plot_ly(x = ~ sample_type) %>%
    add_bars(y = ~ unique_n,name="unique") %>%
    add_lines(y = ~ unique_p, yaxis = "y2",name="pct") %>%
    add_markers(y = ~ unique_p, yaxis = "y2") %>%
    layout(
      title="唯一值",
      showlegend = FALSE,
      yaxis2 = list(
        overlaying = "y",
        side = "right",
        tickformat = "%",
        title = ""
      ),
      yaxis = list(
        showgrid = FALSE,
        tickformat = ",",
        title = ""
      ),
      xaxis = list(showgrid = FALSE, title = ""),
      margin=list(l = 50,r = 50,b = 50,t = 50,pad = 4)
    )
})


output$tab_var_plot_special <- renderPlotly({
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_plot_special)",
    "trigger "
  )
  req(ra_tbl$tbl_onevar_stat)
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_plot_special)",
    "start "
  )
  ra_tbl$tbl_onevar_stat %>% 
    select(sample_type,special_n,special_p) %>% 
    distinct() %>% 
    plot_ly(x = ~ sample_type) %>%
    add_bars(y = ~ special_n,name="special") %>%
    add_lines(y = ~ special_p, yaxis = "y2",name="pct") %>%
    add_markers(y = ~ special_p, yaxis = "y2") %>%
    layout(
      title="特殊值",
      showlegend = FALSE,
      yaxis2 = list(
        overlaying = "y",
        side = "right",
        tickformat = "%",
        title = ""
      ),
      yaxis = list(
        showgrid = FALSE,
        tickformat = ",",
        title = ""
      ),
      xaxis = list(showgrid = FALSE, title = ""),
      margin=list(l = 50,r = 50,b = 50,t = 50,pad = 4)
    )
})

# ranking stat ------------------------------------------------------------

output$tab_var_tbl_stat_p3 <- renderDT({
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_table_p3)",
    "trigger "
  )
  req(ra_tbl$tbl_onevar_stat)
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_table_p3)",
    "start "
  )
  ra_tbl$tbl_onevar_stat %>% 
    select(sample_type, cat_1, cat_2, type, var, desc,r1,r1_n,r1_p,r2,r2_n,r2_p,r3,r3_n,r3_p) %>% 
    distinct() %>% 
    datatable(
      selection = "none",
      rownames = FALSE,
      options = list(
        orderClasses = TRUE,
        lengthChange = TRUE,
        searching = FALSE,
        paging = FALSE,
        scrollX = TRUE
      )
    ) %>% 
    formatRound(c("r1_n", "r2_n", "r3_n"),0) %>% 
    formatPercentage(c("r1_p","r2_p","r3_p"), 1)
},server = TRUE)


output$tab_var_plot_rank <- renderPlotly({
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_plot_rank)",
    "trigger "
  )
  req(ra_tbl$tbl_onevar)
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_plot_rank)",
    "start "
  )
  
  df <- ra_tbl$tbl_onevar %>% 
    group_by(sample_type,x) %>% 
    tally() %>% 
    group_by(sample_type) %>% 
    arrange(desc(n)) %>% 
    mutate(id=row_number(),
           tot=sum(n),
           cum=cumsum(n)) %>% 
    filter(id<=input$tab_var_input_rank_num) %>% 
    mutate(cum=cum/tot) %>% 
    select(sample_type,id,cum)
  plot_ly(df, x = ~id, y = ~cum, color=~sample_type, type = 'scatter', mode = 'lines+markers') %>% 
    layout(
      title="TOP值累积占比",
      showlegend = TRUE,
      legend=list(x=0,y=1),
      yaxis = list(
        showgrid = FALSE,
        tickformat = "%",
        title = ""
      ),
      xaxis = list(showgrid = FALSE, title = ""),
      margin=list(l = 50,r = 50,b = 50,t = 50,pad = 4)
    )
  
})



# missing and unique ------------------------------------------------------

output$tab_var_tbl_stat_p4 <- renderDT({
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_table_p4)",
    "trigger "
  )
  req(ra_tbl$tbl_onevar_stat)
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_table_p4)",
    "start "
  )
  ra_tbl$tbl_onevar_stat %>% 
    select(sample_type, cat_1, cat_2, type, var, desc,mean,std,min, p5, p10, p25, p50, p75, p90, p95, max) %>% 
    distinct() %>% 
    datatable(
      selection = "none",
      rownames = FALSE,
      options = list(
        orderClasses = TRUE,
        lengthChange = TRUE,
        searching = FALSE,
        paging = FALSE,
        scrollX = TRUE
      )
    ) %>% 
    formatRound(c("mean","std","min", "p5", "p10", "p25", "p50", "p75", "p90", "p95", "max"), 3)
},server = TRUE)


# percentile --------------------------------------------------------------

output$tab_var_tbl_stat_p5 <- renderDT({
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_table_p5)",
    "trigger "
  )
  req(ra_tbl$tbl_onevar_stat)
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_table_p5)",
    "start "
  )
  ra_tbl$tbl_onevar_stat %>% 
    select(sample_type, cat_1, cat_2, type, var, desc,min,p5,p10,p15,p20,p25,p30,p35,p40,p45,p50,p55,p60,p65,p70,p75,p80,p85,p90,p95,max) %>% 
    distinct() %>% 
    datatable(
      selection = "none",
      rownames = FALSE,
      options = list(
        orderClasses = TRUE,
        lengthChange = TRUE,
        searching = FALSE,
        paging = FALSE,
        scrollX = TRUE
      )
    ) %>% 
    formatRound(c("min","p5","p10","p15","p20","p25","p30","p35","p40","p45","p50","p55","p60","p65","p70","p75","p80","p85","p90","p95","max"), 3)
},server = TRUE)

output$tab_var_plot_quantile <- renderPlotly({
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_plot_quantile)",
    "trigger "
  )
  req(nrow(ra_tbl$tbl_onevar_stat)>0)
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_plot_quantile)",
    "start "
  )
  
  ra_tbl$tbl_onevar_stat %>% 
    rename(p0=min,p100=max) %>% 
    select(sample_type,matches("^p\\d+$")) %>% 
    gather(key="x",value="value",-sample_type) %>% 
    mutate(x=as.integer(gsub("p","",x))) %>% 
    plot_ly(x = ~x, y = ~value, color=~sample_type, type = 'scatter', mode = 'lines+markers') %>% 
    layout(
      title="百分位",
      showlegend = TRUE,
      legend=list(x=0,y=1),
      yaxis = list(
        showgrid = FALSE,
        title = ""
      ),
      xaxis = list(showgrid = TRUE, title = ""),
      margin=list(l = 50,r = 50,b = 50,t = 50,pad = 4)
    )
  
})


output$tab_var_tbl_stat_p6 <- renderDT({
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_table_p6)",
    "trigger "
  )
  req(ra_tbl$tbl_onevar_stat)
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_table_p6)",
    "start "
  )
  ra_tbl$tbl_onevar_stat %>% 
    select(sample_type, cat_1, cat_2, type, var, desc,bin_src,psi_tree, psi_chi, psi_freq, psi_manual) %>% 
    datatable(
      selection = "none",
      rownames = FALSE,
      options = list(
        orderClasses = TRUE,
        lengthChange = TRUE,
        searching = FALSE,
        paging = FALSE,
        scrollX = TRUE
      )
    ) %>% 
    formatRound(c("psi_tree", "psi_chi", "psi_freq", "psi_manual"), 3)
},server = TRUE)

output$tab_var_tbl_stat_p7 <- renderDT({
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_table_p7)",
    "trigger "
  )
  req(ra_tbl$tbl_onevar_stat)
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_table_p7)",
    "start "
  )
  ra_tbl$tbl_onevar_stat %>% 
    select(sample_type, cat_1, cat_2, type, var, desc,bin_src,bin_tree,bin_chi,bin_freq,bin_manual,iv_tree,iv_chi,iv_freq,iv_manual) %>% 
    datatable(
      selection = "none",
      rownames = FALSE,
      options = list(
        orderClasses = TRUE,
        lengthChange = TRUE,
        searching = FALSE,
        paging = FALSE,
        scrollX = TRUE
      )
    ) %>% 
    formatRound(c("bin_tree","bin_chi","bin_freq","bin_manual"), 0) %>% 
    formatRound(c("iv_tree","iv_chi","iv_freq","iv_manual"), 3)
},server = TRUE)



output$tab_var_plot_bin_iv_train <- renderPlotly({
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_plot_iv_train)",
    "trigger "
  )
  req(nrow(ra_tbl$tbl_onevar_stat)>0)
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_plot_iv_train)",
    "start "
  )
  
  
  ra_tbl$tbl_onevar_stat %>% 
    filter(bin_src=="train") %>% 
    select(sample_type,matches("iv")) %>% 
    gather(key="method",value ="iv",-sample_type) %>% 
    mutate(method=gsub("iv_","",method)) %>% 
    plot_ly(x = ~ sample_type,y=~iv,color=~method,type="bar") %>%
    layout(
      title="IV bin_src=all",
      showlegend = TRUE,
      yaxis2 = list(
        overlaying = "y",
        side = "right",
        tickformat = "%",
        title = ""
      ),
      yaxis = list(
        showgrid = FALSE,
        tickformat = ",",
        title = ""
      ),
      xaxis = list(showgrid = FALSE, title = ""),
      margin=list(l = 50,r = 50,b = 50,t = 50,pad = 4)
    )
  
})



output$tab_var_plot_bin_iv_all <- renderPlotly({
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_plot_iv_all)",
    "trigger "
  )
  req(nrow(ra_tbl$tbl_onevar_stat)>0)
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_plot_iv_all)",
    "start "
  )
  
  
  ra_tbl$tbl_onevar_stat %>% 
    filter(bin_src=="all") %>% 
    select(sample_type,matches("iv")) %>% 
    gather(key="method",value ="iv",-sample_type) %>% 
    mutate(method=gsub("iv_","",method)) %>% 
    plot_ly(x = ~ sample_type,y=~iv,color=~method,type="bar") %>%
    layout(
      title="IV bin_src=all",
      showlegend = TRUE,
      yaxis2 = list(
        overlaying = "y",
        side = "right",
        tickformat = "%",
        title = ""
      ),
      yaxis = list(
        showgrid = FALSE,
        tickformat = ",",
        title = ""
      ),
      xaxis = list(showgrid = FALSE, title = ""),
      margin=list(l = 50,r = 50,b = 50,t = 50,pad = 4)
    )
  
})


output$tab_var_plot_bin_psi_train <- renderPlotly({
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_plot_psi_train)",
    "trigger "
  )
  req(nrow(ra_tbl$tbl_onevar_stat)>0)
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_plot_psi_train)",
    "start "
  )
  
  
  ra_tbl$tbl_onevar_stat %>% 
    filter(bin_src=="train") %>% 
    select(sample_type,matches("psi")) %>% 
    gather(key="method",value ="psi",-sample_type) %>% 
    mutate(method=gsub("psi_","",method)) %>% 
    plot_ly(x = ~ sample_type,y=~psi,color=~method,type="bar") %>%
    layout(
      title="PSI bin_src=train",
      showlegend = TRUE,
      yaxis2 = list(
        overlaying = "y",
        side = "right",
        tickformat = "%",
        title = ""
      ),
      yaxis = list(
        showgrid = FALSE,
        tickformat = ",",
        title = ""
      ),
      xaxis = list(showgrid = FALSE, title = ""),
      margin=list(l = 50,r = 50,b = 50,t = 50,pad = 4)
    )
  
})



output$tab_var_plot_bin_psi_all <- renderPlotly({
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_plot_psi_all)",
    "trigger "
  )
  req(nrow(ra_tbl$tbl_onevar_stat)>0)
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_plot_psi_all)",
    "start "
  )
  
  
  ra_tbl$tbl_onevar_stat %>% 
    filter(bin_src=="all") %>% 
    select(sample_type,matches("psi")) %>% 
    gather(key="method",value ="psi",-sample_type) %>% 
    mutate(method=gsub("psi_","",method)) %>% 
    plot_ly(x = ~ sample_type,y=~psi,color=~method,type="bar") %>%
    layout(
      title="PSI bin_src=all",
      showlegend = TRUE,
      yaxis2 = list(
        overlaying = "y",
        side = "right",
        tickformat = "%",
        title = ""
      ),
      yaxis = list(
        showgrid = FALSE,
        tickformat = ",",
        title = ""
      ),
      xaxis = list(showgrid = FALSE, title = ""),
      margin=list(l = 50,r = 50,b = 50,t = 50,pad = 4)
    )
  
})

# boxplot -----------------------------------------------------------------

output$tab_var_plot_box <- renderPlotly({
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_plot_box)",
    "trigger "
  )
  req(ra_tbl$tbl_onevar)
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_plot_box)",
    "start "
  )
  ra_tbl$tbl_onevar %>%
    plot_ly(y = ~ x,
            color = ~ sample_type,
            type = 'box')
})

# violin plot -------------------------------------------------------------
output$tab_var_plot_violin <- renderPlotly({
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_plot_violin)",
    "trigger "
  )
  req(nrow(ra_tbl$tbl_onevar)>0)
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_plot_violin)",
    "start "
  )
  ra_tbl$tbl_onevar %>%
    plot_ly(x=~sample_type
            ,y = ~ x,
            type = 'violin')
  
})


# density plot ------------------------------------------------------------
output$tab_var_plot_density <- renderPlotly({
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_plot_density)",
    "trigger "
  )
  req(nrow(ra_tbl$tbl_onevar)>0)
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_var_plot_density)",
    "start "
  )
  p <- ggplot(ra_tbl$tbl_onevar, aes(x, fill = sample_type)) + 
    geom_density(alpha = 0.2) +
    theme_bw()
  ggplotly(p)
})


# histogram ---------------------------------------------------------------

# output$tab_var_plot_hist <- renderPlotly({
#   my_log(
#     "DEBUG",
#     ra_config$debug_level,
#     "(tab_var_plot_hist)",
#     "trigger "
#   )
#   req(nrow(ra_tbl$tbl_onevar)>0)
#   my_log(
#     "DEBUG",
#     ra_config$debug_level,
#     "(tab_var_plot_histogram)",
#     "start "
#   )
#   df <- ra_tbl$tbl_onevar
#   
#   fig <- plot_ly(alpha = 0.3,nbinsx=as.integer(input$tab_var_plot_hist_bins))
#   if ("train" %in% df$sample_type) {
#     train <- df %>% 
#       filter(sample_type=="train")
#     fig <- fig %>% add_histogram(x = ~train)
#   }
#   if ("test" %in% df$sample_type) {
#     test <- df %>% 
#       filter(sample_type=="test")
#     fig <- fig %>% add_histogram(x = ~test)
#   }
#   if ("validate" %in% df$sample_type) {
#     validate <- df %>% 
#       filter(sample_type=="validate")
#     fig <- fig %>% add_histogram(x = ~validate)
#   }
#   if ("train" %in% df$sample_type) {
#     all <- df %>% 
#       filter(sample_type=="all")
#     fig <- fig %>% add_histogram(x = ~all)
#   }
#   fig
# })

ra_tab_var_scatter <- reactiveValues(
  train=NULL,
  train_f=NULL,
  test=NULL,
  test_f=NULL,
  validate=NULL,
  validate_f=NULL,
  all=NULL,
  all_f=NULL,
  brush=NULL
)

observeEvent(ra_tbl$tbl_onevar,
             {
               req(ra_data$onevar_type %in% c("integer","numeric","integer64"))
               df <- ra_tbl$tbl_onevar %>% 
                 mutate(rd=rnorm(nrow(.)))
               ra_tab_var_scatter$train <- df %>% 
                 filter(sample_type=="train")
               ra_tab_var_scatter$test <- df %>% 
                 filter(sample_type=="test")
               ra_tab_var_scatter$validate <- df %>% 
                 filter(sample_type=="validate")
               ra_tab_var_scatter$all <- df %>% 
                 filter(sample_type=="all")
             })
# brush
observeEvent(input$tab_var_s_train_brush,{
  req(input$tab_var_s_train_brush)
  print("input$tab_var_s_train_brush==")
  print(input$tab_var_s_train_brush)
  ra_tab_var_scatter$brush <- input$tab_var_s_train_brush
})
observeEvent(input$tab_var_s_test_brush,{
  req(input$tab_var_s_test_brush)
  ra_tab_var_scatter$brush <- input$tab_var_s_test_brush
})
observeEvent(input$tab_var_s_validate_brush,{
  req(input$tab_var_s_validate_brush)
  ra_tab_var_scatter$brush <- input$tab_var_s_validate_brush
})
observeEvent(input$tab_var_s_all_brush,{
  req(input$tab_var_s_all_brush)
  ra_tab_var_scatter$brush <- input$tab_var_s_all_brush
})

# brush
observeEvent(ra_tab_var_scatter$brush,{
  req(ra_tab_var_scatter$brush)
  brush <- ra_tab_var_scatter$brush
  ra_tab_var_scatter$train_f <- ra_tab_var_scatter$train %>% 
    filter(x>=brush$xmin & x<=brush$xmax & rd>=brush$ymin & rd<=brush$ymax)
  ra_tab_var_scatter$test_f <- ra_tab_var_scatter$test %>% 
    filter(x>=brush$xmin & x<=brush$xmax & rd>=brush$ymin & rd<=brush$ymax)
  ra_tab_var_scatter$validate_f <- ra_tab_var_scatter$validate %>% 
    filter(x>=brush$xmin & x<=brush$xmax & rd>=brush$ymin & rd<=brush$ymax)
  ra_tab_var_scatter$all_f <- ra_tab_var_scatter$all %>% 
    filter(x>=brush$xmin & x<=brush$xmax & rd>=brush$ymin & rd<=brush$ymax)
})

# left
output$tab_var_plot_hist_train_l <- renderPlot({
  req(nrow(ra_tab_var_scatter$train)>0)
  qplot(ra_tab_var_scatter$train$rd, ra_tab_var_scatter$train$x)
})

output$tab_var_plot_hist_test_l <- renderPlot({
  req(nrow(ra_tab_var_scatter$test)>0)
  qplot(ra_tab_var_scatter$test$rd, ra_tab_var_scatter$test$x)
})
output$tab_var_plot_hist_validate_l <- renderPlot({
  req(nrow(ra_tab_var_scatter$validate)>0)
  qplot(ra_tab_var_scatter$validate$rd, ra_tab_var_scatter$validate$x)
})
output$tab_var_plot_hist_all_l <- renderPlot({
  req(nrow(ra_tab_var_scatter$all)>0)
  qplot(ra_tab_var_scatter$all$rd, ra_tab_var_scatter$all$x)
})

# output$tab_var_plot_hist_train_r <- renderPlot({
#   req(nrow(ra_tab_var_scatter$train_f)>0)
#   hist(ra_tab_var_scatter$train_f$x)
# })
# 
# output$tab_var_plot_hist_test_r <- renderPlot({
#   req(nrow(ra_tab_var_scatter$test_f)>0)
#   hist(ra_tab_var_scatter$test_f$x)
# })
# output$tab_var_plot_hist_validate_r <- renderPlot({
#   req(nrow(ra_tab_var_scatter$validate_f)>0)
#   hist(ra_tab_var_scatter$validate_f$x)
# })
# output$tab_var_plot_hist_all_r <- renderPlot({
#   req(nrow(ra_tab_var_scatter$all_f)>0)
#   hist(ra_tab_var_scatter$all_f$x)
# })