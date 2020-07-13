
tbl_bgcolor_css_code <- function(idx,values,pos,colors) {
  n <- length(values)
  if (n<=0) {return(NULL)}
  code <- lapply(1:n,function(i) {
    pre <- ifelse(i==1,"if ","else if ")
    cond <- sprintf(' (data[%d]=="%s") ',idx,values[i])
    body <- if (length(pos)==1) {
      sprintf(' $("td:eq(%d)", row).css({"background-color": "%s80"}); ',pos,colors[i])
    } else {
      sapply(1:length(pos),function(m){
        sprintf(' $("td:eq(%d)", row).css({"background-color": "%s80"}); ',pos[m],colors[i])
      }) %>% 
        paste0(collapse="\n")
    }
    code <- sprintf(' %s %s {
                %s
            }',pre,cond,body)
    return(code)
  }) %>% 
    paste0(collapse = "\n")
  return(code)
}