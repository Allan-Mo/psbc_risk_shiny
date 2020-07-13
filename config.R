# template --------------------------------------------------------------------
psbc_risk_template <- list(
  tbl_project = data.frame(
    id = c(1),
    name = c("example"),
    name_cn = c("example"),
    owner = c("myz"),
    create_time = c(Sys.time()),
    modify_time = c(Sys.time()),
    path = c(
      "D:/OneDrive/R_packages/psbc_risk_shiny/examples/example_project"
    ),
    stringsAsFactors = FALSE
  ),
  tbl_stat = data.frame(
    dt = character(0),
    sample_type = character(0),
    n = integer(0),
    pct = numeric(0),
    pos = integer(0),
    rate = numeric(0),
    ncol = integer(0),
    factor = integer(0),
    int = integer(0),
    num = integer(0),
    str = integer(0),
    stringsAsFactors = FALSE
  )
  
  ,
  
  tbl_mutate = data.frame(
    id = integer(0),
    type = character(0),
    note = character(0),
    code = character(0),
    stringsAsFactors = FALSE
  ),
  tbl_special_values =
    data.frame(
      id = integer(0),
      name = character(0),
      type = character(0),
      seq = integer(0),
      value_num = integer(0),
      value_char = character(0),
      note = character(0),
      stringsAsFactors = FALSE
    ),
  # tbl_bin_groups = data.frame(
  #   id=c(1L,2L),
  #   name = c("default_num", "default_char"),
  #   bin_num_limit = c(8L, 8L),
  #   stop_limit = c(0.1, 0.1),
  #   count_distr_limit = c(0.05, 0.05),
  #   note = c("default value for integer and numeric columns", "default value for string and character columns"),
  #   stringsAsFactors = FALSE
  # ),
  tbl_vars = data.frame(
    id = integer(0),
    cat_1 = character(0),
    cat_2 = character(0),
    var = character(0),
    type = character(0),
    desc = character(0),
    special_value_ref = character(),
    bin_num_limit	= character(0),
    stop_limit = character(0),
    count_distr_limit = character(0),
    breaks = character(0),
    note = character(0),
    stringsAsFactors = FALSE
  ),
  tbl_stat_general = data.frame(
    sample_type = character(0),
    var = character(0),
    dt_general = character(0),
    type = character(0),
    miss_n = integer(0),
    miss_p = numeric(0),
    unique_n = integer(0),
    unique_p = numeric(0),
    r1 = character(0),
    r1_n = integer(0),
    r1_p = numeric(0),
    r2 = character(0),
    r2_n = integer(0),
    r2_p = numeric(0),
    r3 = character(0),
    r3_n = integer(0),
    r3_p = numeric(0),
    mean = numeric(0),
    std = numeric(0),
    min = numeric(0),
    p5 = numeric(0),
    p10 = numeric(0),
    p15 = numeric(0),
    p20 = numeric(0),
    p25 = numeric(0),
    p30 = numeric(0),
    p35 = numeric(0),
    p40 = numeric(0),
    p45 = numeric(0),
    p50 = numeric(0),
    p55 = numeric(0),
    p60 = numeric(0),
    p65 = numeric(0),
    p70 = numeric(0),
    p75 = numeric(0),
    p80 = numeric(0),
    p85 = numeric(0),
    p90 = numeric(0),
    p95 = numeric(0),
    max = numeric(0),
    
    stringsAsFactors = FALSE
  ),
  tbl_stat_special = data.frame(
    sample_type = character(0),
    var = character(0),
    dt_special = character(0),
    
    special_n = integer(0),
    special_p = numeric(0),
    stringsAsFactors = FALSE
  ),
  tbl_stat_bin = data.frame(
    sample_type = character(0),
    bin_src=character(0),
    var = character(0),
    dt_bin = character(0),
    bin_tree = integer(0),
    bin_chi = integer(0),
    bin_freq = integer(0),
    bin_manual = integer(0),
    iv_tree = numeric(0),
    iv_chi = numeric(0),
    iv_freq = numeric(0),
    iv_manual = numeric(0),
    psi_tree = numeric(0),
    psi_chi = numeric(0),
    psi_freq = numeric(0),
    psi_manual = numeric(0),
    stringsAsFactors = FALSE
  ),
  tbl_stat_cat = data.frame(
    var = character(0),
    cat_1 = character(0),
    cat_2 = character(0),
    desc = character(0)
  ),
  tbl_varstat = data.frame(
    sample_type = character(0),
    cat_1 = character(0),
    cat_2 = character(0),
    type = character(0),
    var = character(0),
    desc = character(0),
    
    dt_general=character(0),
    special_n = integer(0),
    special_p = numeric(0),
    dt_special=character(0),
    miss_n = integer(0),
    miss_p = numeric(0),
    unique_n = integer(0),
    unique_p = numeric(0),
    r1=character(0),
    r1_n = integer(0),
    r1_p = numeric(0),
    r2=character(0),
    r2_n = integer(0),
    r2_p = numeric(0),
    r3=character(0),
    r3_n = integer(0),
    r3_p = numeric(0),
    mean = numeric(0),
    std = numeric(0),
    min = numeric(0),
    p5 = numeric(0),
    p10 = numeric(0),
    p15 = numeric(0),
    p20 = numeric(0),
    p25 = numeric(0),
    p30 = numeric(0),
    p35 = numeric(0),
    p40 = numeric(0),
    p45 = numeric(0),
    p50 = numeric(0),
    p55 = numeric(0),
    p60 = numeric(0),
    p65 = numeric(0),
    p70 = numeric(0),
    p75 = numeric(0),
    p80 = numeric(0),
    p85 = numeric(0),
    p90 = numeric(0),
    p95 = numeric(0),
    max = numeric(0),
    dt_bin=character(0),
    bin_src=character(0),
    bin_tree = integer(0),
    bin_chi = integer(0),
    bin_freq = integer(0),
    bin_manual = integer(0),
    iv_tree = numeric(0),
    iv_chi = numeric(0),
    iv_freq = numeric(0),
    iv_manual = numeric(0),
    psi_tree = numeric(0),
    psi_chi = numeric(0),
    psi_freq = numeric(0),
    psi_manual = numeric(0),
    stringsAsFactors = FALSE
  ),
  tbl_setting_chk = data.frame(
    level=character(0),
    sample_type = character(0),
    var = character(0),
    chk = character(0),
    stringsAsFactors = FALSE
  )
)



# loading config file ------------------------------------------------------------------



if (file.exists("config.Rdata")) {
  load("config.Rdata")
  
} else {
  config <- list(
    tbl_project = psbc_risk_template$tbl_project ,
    select_project = NULL,
    log_level = "DEBUG"
  )
}
