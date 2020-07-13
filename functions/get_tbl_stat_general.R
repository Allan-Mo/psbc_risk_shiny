get_tbl_stat_general <- function(df=NULL, vars=NULL, sample_type=NULL,lst_setting=NULL) {
  if (is.null(df) || nrow(df) == 0) {
    return(psbc_risk_template$tbl_stat_general)
  }
  
  n_row <- nrow(df)
  dt <- as.character(Sys.time())
  rst <- lapply(vars, function(var) {
    col <- df[[var]]
    sv <- lst_setting[[var]]$special_values
    has_sv <- length(sv)>0
    if (has_sv) {
      col_sv <- col[!col %in% sv]
      nrow_sv <- length(col_sv)
    } else {
      col_sv <- col
      col_sv <- n_row
    }
    df_rank <- data.frame(x = col) %>%
      count(x) %>%
      arrange(desc(n)) %>%
      head(3) %>%
      as.data.frame()
    n_rank <- nrow(df_rank)
    is_num <- is.numeric(col)
    qts_seq <- seq(0.05, 0.95, 0.05)
    if (is_num) {
      qts <- stats::quantile(col, probs = qts_seq)
    } else {
      qts <- rep.int(NA_real_, times = length(qts_seq))
    }
    qts <- qts %>%
      "names<-"(as.character(qts_seq))
    rst <- data.frame(
      sample_type = sample_type,
      var = var,
      dt_general = dt,
      type = class(col),
      miss_n = sum(is.na(col)),
      unique_n = length(unique(col)),
      r1 = as.character(df_rank[[1, "x"]]),
      r1_n = df_rank[[1, "n"]],
      r2 = ifelse(n_rank >= 2, as.character(df_rank[[2, "x"]]), NA_character_),
      r2_n = ifelse(n_rank >= 2, df_rank[[2, "n"]], NA_integer_),
      r3 = ifelse(n_rank >= 3, as.character(df_rank[[3, "x"]]), NA_character_),
      r3_n = ifelse(n_rank >= 3, df_rank[[3, "n"]], NA_integer_),
      mean = ifelse(is_num, mean(col_sv, na.rm = T), NA_real_),
      std = ifelse(is_num, sd(col_sv, na.rm = T), NA_real_),
      min = ifelse(is_num, min(col_sv, na.rm = T), NA_real_),
      p5 = qts["0.05"],
      p10 = qts["0.1"],
      p15 = qts["0.15"],
      p20 = qts["0.2"],
      p25 = qts["0.25"],
      p30 = qts["0.3"],
      p35 = qts["0.35"],
      p40 = qts["0.4"],
      p45 = qts["0.45"],
      p50 = qts["0.5"],
      p55 = qts["0.55"],
      p60 = qts["0.6"],
      p65 = qts["0.65"],
      p70 = qts["0.7"],
      p75 = qts["0.75"],
      p80 = qts["0.8"],
      p85 = qts["0.85"],
      p90 = qts["0.9"],
      p95 = qts["0.95"],
      max = ifelse(is_num, max(col_sv, na.rm = T), NA_real_),
      
      stringsAsFactors = FALSE
    ) 
    return(rst)
  }) %>%
    rbindlist() %>% 
    mutate(
      miss_p = miss_n / n_row,
      unique_p = unique_n / n_row,
      r1_p = r1_n / n_row,
      r2_p = r2_n / n_row,
      r3_p = r3_n / n_row
    )
  
  return(rst)
  
}

# df <- data.frame(x = 1, y = "haha", z = as.factor("e"))
# vars <- colnames(df)
# sample_type = "train"
# get_tbl_stat_general(df = df,
#                      vars = vars,
#                      sample_type = "train")