#' dpg_s
#'
#' This function deploy autoDisc in r.
#'
#' @param var_name var name.
#' @param data_name data name.
#' @param summary summary.
#' @param mean.na mean of NAs.
#' @param file txt  to deploy.
dpg_r = function(var_name, summary, mean.na,data_name = "data", file) {
  n_bds = nrow(summary) 
  
  # print(summary)
  # print(n_bds)
  cat('##################### \n\n',file = file)
  
  
  if (n_bds == 1) cat(data_name,'$DC_',var_name, ' = "', summary$band[1],'"\n\n',sep = "",file = file)
  
  if (n_bds != 1) {
    cat(data_name,'$DC_',var_name,' = ','as.factor(ifelse(is.na(',data_name,'$',var_name,'),"',summary$band[summary$na],'",
ifelse(',data_name,'$',var_name , ' %in% c("', paste0(t(list2DF(summary$names[1])) ,collapse = '","'),'"),"', summary$band[1],'",\n',sep = "",file = file)
    
    for (j in 2:n_bds) {
      cat('ifelse(',data_name,'$',var_name ,' %in% c("', paste0(t(list2DF(summary$names[j])) ,collapse = '","'),'"),"',summary$band[j],
          '",\n',sep = "",file = file)
    }
    cat('"',summary$band[summary$na],'"',rep(')', n_bds + 1),")",
        '\n\n',sep = "",file = file)
    
    if (!is.na(mean.na)) {
      cat("#NA found\n",
          "#Mean NA = ",mean.na,"\n\n",sep = '',file = file)}
    
    if (is.na(mean.na)) {
      cat("#NA not found",
          "\n\n",sep = '',file = file)}
  }
}
