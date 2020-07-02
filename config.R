# 模板文件 --------------------------------------------------------------------
psbc_risk_template <- list(
  project <- data.frame(
    id = c(1),
    name = c("zdt"),
    name_cn = c("政贷通"),
    owner = c("莫运政"),
    create_time = c(Sys.time()),
    modify_time = c(Sys.time()),
    path = c("D:/OneDrive/R_packages/psbc_risk_shiny/examples/zdt"),
    stringsAsFactors = FALSE
  ),
  tbl_stat = data.frame(
    dt = character(0)
    ,
    sample_type = character(0),
    n = integer(0),
    pct = numeric(0)
    ,
    pos = integer(0),
    rate = numeric(0),
    ncol = integer(0),
    factor = integer(0),
    int = integer(0),
    num = integer(0),
    str = integer(0)
  )
  
  ,
  special = c(-999999,-9999999,-99999999)
  
  ,
  mutate = data.frame(
    id = integer(0),
    type = character(0),
    note = character(0),
    code = character(0)
    ,
    stringsAsFactors = FALSE
  ),
  varstat = data.frame(
    sample_type = character(0),
    cat1 = character(0),
    cat2 = character(0),
    type = character(0),
    col = character(0),
    desc = character(0),
    miss_n = integer(0),
    miss_p = numeric(0),
    special_n = integer(0),
    special_p = numeric(0),
    unique_n = integer(0),
    unique_p = numeric(0),
    r1_n = integer(0),
    r1_p = numeric(0),
    r2_n = integer(0),
    r2_p = numeric(0),
    r3_n = integer(0),
    r3_p = numeric(0),
    mean = numeric(0),
    sdt = numeric(0),
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
    psi_manual = numeric(0)
  )
)



# 载入配置文件 ------------------------------------------------------------------



if (file.exists("config.Rdata")) {
  load("config.Rdata")
  
} else {
  config <- list(
    df_project = psbc_risk_template$project ,
    select_project = NULL,
    log_level = "DEBUG"
  )
}
