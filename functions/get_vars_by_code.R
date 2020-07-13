get_vars_by_code <- function(df, type, code) {
  if (type == "manual") {
    p_change <- "[_a-zA-Z]\\w*+(?!\\()"
    change <- str_extract_all(code, p_change)[[1]]
    change <- intersect(colnames(df), change)
    p_new <- "(?<=(^|\n))[_a-zA-Z]\\w*+(?!\\()"
    new <- str_extract_all(code, p_new)[[1]]
  } else if (type == "all") {
    change <- colnames(df)
    new <- colnames(df)
  } else if (type %in% c("real",
                         "character",
                         "factor",
                         "integer",
                         "numeric")) {
    cl <- sapply(df, class)
    change <- cl[cl %in%  switch(
      type,
      real = c("integer", "integer64", "numeric"),
      character = c("character"),
      factor = c("factor"),
      integer = c("integer", "integer64"),
      numeric = c("numeric")
    )] %>% names()
    new <- change
  } else if (type == "advanced") {
    line_1 <- str_split(code, "\n")[[1]][1]
    change <- sapply(str_split(line_1, ",")[[1]], function(x) {
      df %>%
        head(1) %>%
        select_(str_trim(x)) %>%
        colnames()
    }) %>%
      unlist() %>%
      as.vector()
    
    new <- change
  }
  
  # 排除列
  change <- setdiff(change,c("sample_type","label"))
  new <- setdiff(new,c("sample_type","label"))
  return(list(change = change, new = new))
}
