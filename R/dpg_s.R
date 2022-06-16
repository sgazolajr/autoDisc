#' dpg_s
#'
#' This function deploy autoDisc in SQL.
#'
#' @param var_name var name.
#' @param data_name data name.
#' @param summary summary.
#' @param mean.na mean of NAs.
#' @param file txt  to deploy.
dpg_s = function(var_name, summary, mean.na,data_name = "data", file) {
  n_bds = nrow(summary) 
  
  cat("##################### \n\n",file = file)
  
  if (n_bds == 0) cat("SELECT [DC_",var_name,"] = '",summary$band[1]," FROM [dbo].[",data_name,"];\n\n", sep = '',file = file)
  
  if (n_bds != 0) {
    
    cat("DECLARE @DC_",var_name," VARCHAR(10) \n",sep = '',file = file)
    cat("\tSET @DC_",var_name," = ( \n",sep = '',file = file)
    
    cat("\t\tSELECT CASE WHEN @",var_name," IS NULL\tTHEN '", summary$band[summary$na],"'\n",
        "\t\t\t\t\tWHEN @",var_name , " in ('", paste0(t(list2DF(summary$names[1])) ,collapse = "','"),"')\tTHEN '", summary$band[1],"' \n",
        sep = '',file = file)
    
    for (j in 2:n_bds) {
      cat("\t\t\t\t\tWHEN @",var_name ," in ('",paste0(t(list2DF(summary$names[j])) ,collapse = "','"),"')\tTHEN '",summary$band[j],"'\n",
          sep = '',file = file)
    }
    cat("\t\t\t\t\tELSE \t\t'",summary$band[summary$na],"'", "\n",sep = '',file = file)
    cat("\t\t\t\t\tEND\n",sep = '',file = file)
    cat("\t) \n",sep = '',file = file)
    
    if (!is.na(mean.na)) {
      cat("#NA found\n",
          "#Mean NA = ",mean.na,"\n\n",sep = '',file = file)}
    
    if (is.na(mean.na)) {
      cat("#NA not found",
          "\n\n",sep = '',file = file)}
    
  }
}
