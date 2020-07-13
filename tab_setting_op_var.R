
# table -------------------------------------------------------------------



output$tab_setting_var_table <- DT::renderDT({
  my_log("DEBUG",ra_config$debug_level,"(tab_setting_var)","trigger tab_setting_var_table rendering")
  req(ra_project$tbl_vars)
  my_log("DEBUG",ra_config$debug_level,"(tab_setting_var)","tab_setting_var_table rendering - start")
  cat_1 <- ra_project$tbl_vars$cat_1 %>%  
    unique()
  cat_2 <- ra_project$tbl_vars$cat_2 %>% 
    unique()
  type <- ra_project$tbl_vars$type %>%  
    unique()
  special_value <- ra_project$tbl_vars$special_value_ref %>%  
    unique()
  colors <- rainbow(length(cat_1)+length(cat_2)+length(type)+length(special_value))
  cat_1 <- cat_1 %>% 
    tbl_bgcolor_css_code(1,.,c(1),colors)
  colors <- colors[length(cat_1)+1:length(colors)]
  cat_2 <- cat_2 %>% 
    tbl_bgcolor_css_code(2,.,c(2),colors)
  colors <- colors[length(cat_2)+1:length(colors)]
  type <- type %>% 
    tbl_bgcolor_css_code(4,.,c(3,4,5),colors)
  colors <- colors[length(type)+1:length(colors)]
  special_value <- special_value %>% 
    tbl_bgcolor_css_code(6,.,c(6),colors)
  js_code <- paste0(cat_1,cat_2,type,special_value,sep="\n")
  # cat(js_code)
  ra_project$tbl_vars %>%
    datatable(
      selection = "single",
      filter = "top",
      rownames = FALSE,
      options = list(
        orderClasses = TRUE,
        lengthChange = TRUE,
        searching = TRUE,
        paging = TRUE,
        scrollX = FALSE,
        scrolly = TRUE,
        lengthMenu = list(c(30, 50,100,150,200, -1), c('30', '50',"100","150","200", 'All')),
        pageLength = 50,
        rowCallback = DT::JS(
          sprintf('function(row, data) {
            %s

          }',js_code)
        )
      )
    )
  
}
,
server = TRUE)


# change onevar_var -------------------------------------------------------
observeEvent(input$tab_setting_var_table_rows_selected,{
  req(input$tab_setting_var_table_rows_selected)
  req(ra_project$tbl_varstat)
  req(nrow(ra_project$tbl_varstat)>0)
  col <- ra_project$tbl_vars$var[input$tab_setting_var_table_rows_selected]
  req(col %in% ra_project$tbl_varstat$var)
  type <- ra_project$tbl_varstat %>% 
    filter(var==col) %>% 
    "[["("type") %>% 
    "["(1)
  
  ra_data$onevar_var <- col
  if (type %in% c("integer","integer64","numeric")) {
    ra_data$onevar_type <- "numeric"
  } else {
    ra_data$onevar_type <- "character"
  }
  
})




