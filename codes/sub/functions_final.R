output_slides <- function(m_list, title, filename, dep_var_label, cov_label, keep_var, added_line){
  stargazer(
    m_list,
    #se = list(rse_fs, rse_fs_d, rse_fs_di, rse_fs_dic),
    dep.var.labels = dep_var_label,
    covariate.labels = cov_label,
    keep = keep_var,
    omit.stat = c('adj.rsq', 'ser'),
    add.lines = added_line,
    notes.label = '',
    omit.table.layout = "n",
    title = title,
    table.placement = '!t',
    label = paste0('industry_', filename),
    out = paste0('output_final/', filename, '.tex')
  )
  
}

# cluster bootstrap function
df %>% select(code_union_ess) %>% unique %>% unname %>% unlist -> list_ind
bs <- function(i, vec, df){
  out <- df %>% filter(code_union_ess != vec[i])
}
reg_b <- function(loop, df, fmla, varname){
  #print(paste("bootstrap loop", loop))
  cl_b <- sample(length(list_ind), length(list_ind), replace = T)
  df_b <- bs(1, cl_b, df)
  m_b <- felm(fmla, data = df_b, weights = df_b$sales.m_value_5_avg)
  m_b$coefficients[varname,]
  #data.frame(cl_b[1], m_b$coefficients[varname,])
}
