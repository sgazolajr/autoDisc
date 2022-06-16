#' autoGroup
#'
#' This function auto group your vars.
#'
#' @param data R data frame object.
#' @param y Name of you target column.
#' @param train_subset Vector of 0 and 1, with the same len of data.
#' @param include List with name of variables to be analyzed by the algorithm.
#' @param exclude If you do not want to pass the name of the Data Frame variables to be analyzed by the algorithm, you also have to pass the list of data frame variables that are not analyzed, for example the key variables, and everything else will be analyzed.
#' @param bs_loc Where put baseline: "euqal", "min" or "max".
#' @param na_replace String to replace NAs, its recomended do this, to NA be included in a band. If na_replace = NA, NAs will be include in a band with min diff of avg y. 
#' @param min_r min rep of the tree leaves.
#' @param cp cp of the tree.
#' @param maxdepth maxdepth of the tree.
#' @param file_name File name with the deploy.
#' @param deploy_r TRUE or FALSE.
#' @param deploy_sql TRUE or FALSE.
#' @param pct_na_allowed Pct of NA allowed in each covariable.
#' @param perc_max_cat Perc of nrow data that perc of number of categories of each var are allowed.
#' @param return_data A boolean indicate if function will return data frame.
#' @param warn Indicate if function will show warnings.
#' @param subsample_rows Proportion of your data that function will run.
#' @param seed Seed to subsample rows.
#' @param debug debug.
#' @return A data_frame with the vars DC.
#' @import dplyr 
#' @import rpart 
#' @import stringr 
#' @importFrom stats na.omit
#' @importFrom stats aggregate
#'@examples
#'\dontrun{
#'autoGroup(data = your_df
#'          ,y = "y"
#'          ,train_subset = your_df$Flag_dev
#'          ,include = include
#'          ,deploy_r = T
#'          ,deploy_sql = TRUE)
#'}
#' @export
autoGroup = function(data,
                     y,
                     train_subset = c(rep(1,nrow(data))),
                     exclude=NULL,
                     include=NULL,
                     bs_loc = "equal",
                     na_replace = "",
                     min_r= 0.03,
                     cp = 0,
                     maxdepth = 3,
                     file_name = "group",
                     deploy_r = FALSE,
                     deploy_sql = FALSE,
                     pct_na_allowed = 0.95,
                     perc_max_cat = 0.9,
                     return_data = TRUE,
                     warn = NULL,
                     seed=NULL,
                     subsample_rows=1,
                     debug = FALSE
){
  data = as.data.frame(data)
  col_nameset = colnames(data)
  
  avg_y <- bd <- name <- set <-NULL
  
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
  
  if(!is.null(warn)){
    default_w <- getOption("warn")
    options(warn = -1)
  }
  
  deploy = deploy_r | deploy_sql 
  if(deploy) codigo_cat = file(paste(file_name, "_deploy.txt", sep = ""), open = "a")
  
  qtd_min_r = min_r * nrow(data_train)
  rpart_control = rpart.control(cp = cp
                                ,maxdepth = maxdepth
                                , minsplit = trunc(2.5*qtd_min_r)
                                , minbucket = qtd_min_r)
  
  max_cat = trunc(perc_max_cat*nrow(data_train))
  
  for (var in Xnames) if(is.numeric(as.matrix(data[,var]))){
    Xnames = Xnames[!Xnames %in% var]
  }else{
    if(mean(is.na(data_train[,var])) > pct_na_allowed){
      Xnames = Xnames[!Xnames %in% var]
    }else if(length(unique(data_train[,var])) > max_cat) Xnames = Xnames[!Xnames %in% var]
  } 
  
  for (var in Xnames) {
    if(debug) print(var)
    data_train[,var] = data_train[,var] |> as.character()
    qtd_NAs = sum(is.na(data_train[,var]))
    data_train[is.na(data_train[,var]),var] = na_replace
    
    # Ajuste Arvore de Regress?o
    tree = rpart(data_train[,y] ~ data_train[,var]
            ,control = rpart_control
            ,na.action = na.omit
            ,data = data_train
      )
    # splits = tree$splits
    sets = data.frame(name = data_train[!is.na(data_train[,var]),var],bd = tree$where) |> group_by(bd) |> summarise(list(name))
    sets$`list(name)` = lapply(sets$`list(name)`, unique)
    cltr_splits = tree$where
    
    summary = data.frame(aggregate(list(avg_y = data_train[,y][!is.na(data_train[,var])]),
                                   by = list(set = cltr_splits),
                                   FUN = function(x) {mean(x)}),
                         na = rep(F, nrow(sets))
                         ,names = as.array(sets$`list(name)`)
    )
    summary = summary |> arrange(avg_y)  |> mutate(band = paste('B', seq_along(numeric(length(set) )), sep = ''))
    
    avg_tot = data_train[,y] |> mean(na.rm = TRUE)
    summary = summary |> mutate(diff_avg_tot = abs(avg_y - avg_tot))
    avg_NAs = NA
    
    # NAs
    if (qtd_NAs > 0) {
      if(!is.na(na_replace)){
        for(i in 1:nrow(summary)) summary[i,]$na = ifelse(sum(as.character(t(list2DF(summary[i,]$names))) == na_replace)>0,T,F)
      }else{
        avg_NAs = data_train[,y][is.na(data_train[,var])] |> mean(na.rm = TRUE)
        summary = summary |> mutate(diff_avg_NA = abs(avg_y - avg_NAs))
        summary$na[which.min(summary$diff_avg_NA)] = TRUE
      }
    }else summary$na[which.min(summary$diff_avg_tot)] = TRUE 

    # baseline
    if(bs_loc == "equal"){
      summary$band[which.min(summary$diff_avg_tot)] = 'B0' 
    }else if(bs_loc == "min"){
      summary$band[which.min(summary$avg_y)] = 'B0' 
    }else if(bs_loc == "max"){
      summary$band[which.max(summary$avg_y)] = 'B0'
    }
    
    if(debug) print(summary)
    
    if(return_data){
      data[, ncol(data) + 1] = lapply(data[,var], FUN = function(x) ifelse(is.na(x),NA,summary[grepl(as.character(x),summary$names),]$band)) |> as.character()
      data[is.na(data[ncol(data)]) | data[ncol(data)] == "NA" | data[ncol(data)] == "", ncol(data)] = summary$band[summary$na] 
      data[,ncol(data)] = as.factor(data[,ncol(data)])
      names(data)[ncol(data)] = paste0("DC_", var)
    }
    
    if(deploy){
      if(deploy_r) dpg_r(var_name = var,summary =  summary,mean.na = avg_NAs,file = codigo_cat)
      if(deploy_sql) dpg_s(var_name = var,summary =  summary,mean.na = avg_NAs,file = codigo_cat)
    }
  }
  if(deploy) close(codigo_cat)
  if(!is.null(warn)) options(warn = default_w)
  if(return_data) return(data)
}
