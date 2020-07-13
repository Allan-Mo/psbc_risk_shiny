get_tbl_stat_bin_psi <- function(rst, exp_rst, method, var) {
  if ((!is.null(rst$lst_bin[[var]][[method]])) &&
      (!is.null(exp_rst$lst_bin[[var]][[method]]))) {
    tbl_exp <- exp_rst$lst_bin[[var]][[method]] %>%
      ungroup() %>%
      mutate(n = sum(count)) %>%
      mutate(exp_p = count / n) %>%
      select(bin, exp_p)
    tbl_real <- rst$lst_bin[[var]][[method]] %>%
      ungroup() %>%
      mutate(n = sum(count)) %>%
      mutate(real_p = count / n) %>%
      select(bin, real_p)
    rst <- tbl_exp %>%
      full_join(tbl_real,by="bin") %>%
      mutate(exp_p = ifelse(is.na(exp_p), 0, exp_p),
             real_p = ifelse(is.na(real_p), 0, real_p)) %>%
      mutate(exp_p=ifelse(exp_p==0,0.0000001,exp_p)) %>% 
      mutate(psi = (real_p - exp_p) * log(real_p / exp_p)) %>% 
      summarise(psi=sum(psi))
    
    return(rst$psi)
  } else {
    return(NA_real_)
  }
}


get_tbl_stat_bin <-
  function(vars,rst, bin_src, sample_type, exp_rst = NULL) {
    dt <- as.character(Sys.time())
    
    lapply(vars, function(var) {
      lst <- rst$lst_bin[[var]]
      stat <- data.frame(
        bin_src = bin_src,
        sample_type = sample_type,
        var = var,
        bin_tree = ifelse(is.null(lst$tree), NA_integer_, nrow(lst$tree)),
        bin_chi = ifelse(is.null(lst$chimerge), NA_integer_, nrow(lst$chimerge)),
        bin_freq = ifelse(is.null(lst$freq), NA_integer_, nrow(lst$freq)),
        bin_manual = ifelse(is.null(lst$manual), NA_integer_, nrow(lst$manual)),
        iv_tree = ifelse(is.null(lst$tree), NA_real_, max(lst$tree$total_iv)),
        iv_chi = ifelse(is.null(lst$chimerge), NA_real_, max(lst$chimerge$total_iv)),
        iv_freq = ifelse(is.null(lst$freq), NA_real_, max(lst$freq$total_iv)),
        iv_manual = ifelse(is.null(lst$manual), NA_real_, max(lst$manual$total_iv)),
        psi_tree = get_tbl_stat_bin_psi(rst, exp_rst, "tree", var),
        psi_chi = get_tbl_stat_bin_psi(rst, exp_rst, "chimerge", var),
        psi_freq = get_tbl_stat_bin_psi(rst, exp_rst, "freq", var),
        psi_manual = get_tbl_stat_bin_psi(rst, exp_rst, "manual", var)
      )
    }) %>%
      rbindlist() %>%
      mutate(dt_bin = dt)
  }
