output$tab_sc_table_candidate <- DT::renderDT({
  req(ra_project$tbl_varstat)
  ra_project$tbl_varstat %>%
    filter(sample_type=="all" & bin_src=="all") %>% 
    select(cat_1,cat_2,var,desc,type,matches("^(iv)|(psi)")) %>% 
    arrange(desc(iv_manual)) %>% 
    head(20) %>% 
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
