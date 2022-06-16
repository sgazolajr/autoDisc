#' autoDisc
#'
#' This function auto disc your vars for regression.
#'
#' @param data R data frame object.
#' @param y Name of you target column.
#' @param train_subset Vector of 0 and 1, with the same len of data.
#' @param include List with name of variables to be analyzed by the algorithm.
#' @param exclude If you do not want to pass the name of the Data Frame variables to be analyzed by the algorithm, you also have to pass the list of data frame variables that are not analyzed, for example the key variables, and everything else will be analyzed.
#' @param bs_loc Where put baseline: "euqal", "min" or "max".
#' @param na_loc Where put NAs: "euqal", "min" or "max".
#' @param min_r min rep of the tree leaves.
#' @param cp cp of the tree.
#' @param maxdepth maxdepth of the tree.
#' @param file_name File name with the deploy.
#' @param deploy_r TRUE or FALSE.
#' @param deploy_sql TRUE or FALSE.
#' @param deploy_python TRUE or FALSE.
#' @param pct_na_allowed Pct of NA allowed in each covariable.
#' @param quantil_variability_exclude Pct of qunatil, to remove vars that not present variability before discretize.
#' @param trim Pct of trim vars, to remove outliers before discretize.
#' @param return_data A boolean indicate if function will return data frame.
#' @param subsample_rows Proportion of your data that function will run.
#' @param seed Seed to subsample rows.
#' @param debug debug.
#' @return A data_frame with the vars DC.
#' @import dplyr 
#' @import rpart
#' @importFrom stats na.omit
#' @importFrom stats aggregate
#' @importFrom stats quantile
#'@examples
#'\dontrun{
#'data_return = autoDisc(data = your_df
#'                       ,y = "y"
#'                       ,train_subset = your_df$Flag_de
#'                       ,include = include
#'                       ,deploy_r = TRUE
#'                       ,deploy_sql = TRUE)
#'}
#' @export
autoDisc = function(data,
                    y,
                    train_subset = c(rep(1,nrow(data))),
                    exclude=NULL,
                    include=NULL,
                    bs_loc = "equal",
                    na_loc = "equal",
                    min_r= 0.03,
                    cp = 0,
                    maxdepth = 3,
                    file_name = "disc",
                    deploy_r = FALSE, 
                    deploy_sql = FALSE, 
                    deploy_python = FALSE, 
                    pct_na_allowed = 0.9,
                    quantil_variability_exclude = 0.015,
                    trim = 0.01,
                    return_data = TRUE,
                    subsample_rows=1,
                    seed=NULL,
                    debug = FALSE
){

  data = as.data.frame(data)
  col_nameset = colnames(data)
  
  avg_y <- NULL

  
  if(!is.null(exclude) & !is.null(include)) stop("Just one param: 'include' or 'exclude', must be passed.")
  if(!(y %in% col_nameset)) stop(paste(y," not found in data."))
  if(!is.null(include)) if(sum(!(include %in% col_nameset))) stop("Some 'include' var not found in data.")
  
  if(is.null(include)){
    Xnames = col_nameset[!col_nameset %in% c(y,exclude)]
  }else Xnames = include
  
  if(!is.null(seed)) set.seed(seed)
  if(subsample_rows<1-0.00001) data = data[sample(nrow(data),nrow(data)*subsample_rows),]
  data_train = data[train_subset==1,]
  if(sum(is.na(data_train[,y]))) stop("NA Found in y (train subset).")
  
  qtd_min_r = min_r * nrow(data_train)
  rpart_control = rpart.control(cp = cp
                                ,maxdepth = maxdepth
                                , minsplit = trunc(2.5*qtd_min_r)
                                , minbucket = qtd_min_r)
  
  deploy = deploy_r | deploy_sql | deploy_python
  if(deploy) codigo_cat = file(paste(file_name, "_deploy.txt", sep = ""), open = "a")

  for (var in Xnames) if(!(is.numeric(as.matrix(data[,var])))){
    Xnames = Xnames[!Xnames %in% var]
  }else{
    if(quantil_variability_exclude > 0 &
       (as.numeric(quantile(as.matrix(data_train[,var]),quantil_variability_exclude,na.rm = TRUE)) == as.numeric(quantile(as.matrix(data_train[,var]),1-quantil_variability_exclude,na.rm = TRUE)))){
      Xnames = Xnames[!Xnames %in% var]
    }else{
      if(mean(is.na(data_train[,var])) > pct_na_allowed){
        Xnames = Xnames[!Xnames %in% var]
      }else if(trim > 0 & trim < 1){
        trim_inf = trim
        trim_sup = 1-trim
        q_inf = as.numeric(quantile(as.matrix(data_train[,var]),trim_inf,na.rm = TRUE))
        q_sup = as.numeric(quantile(as.matrix(data_train[,var]),trim_sup,na.rm = TRUE))
        
        data_train[,var] = as.numeric(ifelse(data_train[,var] < q_inf, q_inf,
                                             ifelse(data_train[,var] > q_sup, q_sup, data_train[,var])))
      }
    }
  }
  
  for (var in Xnames) {
    if(debug) print(var)
    
    # Ajuste Arvore de Regress?o
    tree = rpart(data_train[,y] ~ data_train[,var]
            ,control = rpart_control
            ,na.action = na.omit
            ,data = data_train
      )
    
    splits = tree$splits[,4] |> as.numeric() |> sort()
    cltr_splits = cut(data_train[!is.na(data_train[,var]),var], breaks = c(-Inf, splits, Inf), labels = paste('BD', seq_along(numeric(length(splits) + 1)), sep = ''))
    
    summary = data.frame(cut = c(-Inf, splits),
                         aggregate(list(avg_y = data_train[,y][!is.na(data_train[,var])]),
                                   by = list(Grupo = cltr_splits),
                                   FUN = function(x) {mean(x)}),
                         na = rep(F, length(splits) + 1))
    summary = summary |> arrange(cut) |> mutate(band = paste('B', seq_along(numeric(length(splits) + 1)), sep = ''))
    
    avg_tot = data_train[,y] |> mean(na.rm = TRUE)
    summary = summary |> mutate(diff_avg_tot = abs(avg_y - avg_tot))
    avg_NAs = NA
    
    # NAs
    if(na_loc == "equal"){
      qtd_NAs = sum(is.na(data_train[,var]))
      
      if (qtd_NAs > 0) {
        avg_NAs = data_train[,y][is.na(data_train[,var])] |> mean(na.rm = TRUE)
        summary = summary |> mutate(diff_avg_NA = abs(avg_y - avg_NAs))
        summary$na[which.min(summary$diff_avg_NA)] = TRUE
      }else summary$na[which.min(summary$diff_avg_tot)] = TRUE 
    }else if(na_loc == "min"){
      summary$na[which.min(summary$avg_y)] = TRUE 
    }else if(na_loc == "max"){
      summary$na[which.max(summary$avg_y)] = TRUE
    }
    
    # baseline
    if(bs_loc == "equal"){
      summary$band[which.min(summary$diff_avg_tot)] = 'B0' 
    }else if(bs_loc == "min"){
      summary$band[which.min(summary$avg_y)] = 'B0' 
    }else if(bs_loc == "max"){
      summary$band[which.max(summary$avg_y)] = 'B0'
    }
    
    if(return_data){
      data[, ncol(data) + 1] = cut(data[,var], breaks = c(summary$cut, Inf), labels = summary$band)
      data[ncol(data)][is.na(data[ncol(data)])] = summary$band[summary$na] 
      data[,ncol(data)] = as.factor(data[,ncol(data)])
      names(data)[ncol(data)] = paste0("DC_", var)
    }
    
    
    if(deploy){
      if(deploy_r) dp_r(var_name = var,summary =  summary,mean.na = avg_NAs,file = codigo_cat)
      if(deploy_sql) dp_s(var_name = var,summary =  summary,mean.na = avg_NAs,file = codigo_cat)
      if(deploy_python) dp_p(var_name = var,summary =  summary,mean.na = avg_NAs,file = codigo_cat)
    }
  }
  if(deploy) close(codigo_cat)
  if(return_data) return(data)
}