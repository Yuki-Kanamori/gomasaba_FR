# require(devtools)
# devtools::install_github("ichimomo/frasyr") 
# require(frasyr)
require(tidyverse)
require(gridExtra)

setwd("~/Dropbox/saVAST_egg/rvpa/re-analysis")
source("original-function1.0.r", encoding = 'UTF-8')
source("frasyr_func_vpa.r")

## 太平洋ゴマサバ 
# rvpa ----------------------------------------------------------
#---- 2019年 ----
caa = read.csv("caa_pgs2019.csv",  row.names = 1) #2019年のCAAは0を入れておく。NAはエラーになる
waa = read.csv("waa_pgs2019.csv",  row.names = 1)
maa = read.csv("maa_pgs2019.csv",  row.names = 1)
cpue = read.csv("cpue_pgs2019.csv", row.names = 1) #2018年の産卵量は使わないのではNAにする
dat2019 = data.handler(caa, waa, maa, cpue, M=0.4)

###step1
res1.pgs = vpa(dat = dat2019,
               sel.f = NULL,
               tf.year = 2014:2017,
               fc.year = 2014:2018, # Fcurrentでどの範囲を参照するか
               last.catch.zero = TRUE,
               plus.group = TRUE,
               Pope = TRUE,  # Popeの近似式を使う
               tune = FALSE,
               p.init = 1,
               plot = TRUE,
               plot.year = 2014:2018)

sel.f = res1.pgs$saa$"2018"

res1.pgs$faa$"2018" #チェック: 最近年のFの値
res1.pgs$naa$"2018" #チェック: 最近年の年齢別資源尾数

###step2
res_vpa2019 = vpa(dat            = dat2019,
                  sel.f           = sel.f,
                  fc.year         = 2014:2018, # Fcurrentでどの範囲を参照するか
                  term.F          = "max",  # 最終年の最高齢のFのみ推定
                  Pope            = TRUE,  # Popeの近似式を使う
                  last.catch.zero = TRUE,
                  abund           = c("N", "SSB"),　
                  min.age         = c(0, 2),　#2〜4歳資源量＝親魚量
                  max.age         = c(0, 4),
                  link            = c("id", "id"),
                  base            = c(NA, NA),
                  tune            = TRUE,
                  plot            = TRUE,
                  plot.year       = 2012:2018)

res_vpa2019$faa$"2018" #チェック: 最近年のFの値
res_vpa2019$naa$"2018" #チェック: 最近年の年齢別資源尾数

if (0) {
  # remove 2019 data
  res_vpa2019$input$dat$caa <- res_vpa2019$input$dat$caa[-ncol(res_vpa2019$input$dat$caa)]
  res_vpa2019$input$dat$maa <- res_vpa2019$input$dat$maa[-ncol(res_vpa2019$input$dat$maa)]
  res_vpa2019$input$dat$waa <- res_vpa2019$input$dat$waa[-ncol(res_vpa2019$input$dat$waa)]
  res_vpa2019$input$dat$M <- res_vpa2019$input$dat$M[-ncol(res_vpa2019$input$dat$M)]
  
  res_vpa2019$naa <- res_vpa2019$naa[-ncol(res_vpa2019$naa)]
  res_vpa2019$faa <- res_vpa2019$faa[-ncol(res_vpa2019$faa)]
  res_vpa2019$baa <- res_vpa2019$baa[-ncol(res_vpa2019$baa)]
  res_vpa2019$ssb <- res_vpa2019$ssb[-ncol(res_vpa2019$ssb)] 
  
  save(res_vpa2019,file="res_vpa2019.rda")
  
  # 2019年のweight at ageは将来予測で使うため、別に保存しておく
  waa2019 <- tibble("2019"=dat2019$waa$"2019")
}


### ここから西嶋検討 ### ----

# vpares = res_vpa2019

cpue0 = read.csv("cpue_pgs2019_rawegg.csv", row.names = 1)
cpue1 = read.csv("cpue_pgs2019_nominal.csv", row.names = 1)
cpue2 = read.csv("cpue_pgs2019_vast_nochub.csv", row.names = 1)
cpue3 = read.csv("cpue_pgs2019_vast_chub.csv", row.names = 1)

# #2018年までの資源量を使ってレトロ解析を行うので，2019年のデータを除去しとく
# for(i in 0:3){
#   data = get(paste0("cpue", i))
#   assign(paste0("cpue", i),
#          data[, -ncol(data)])
# }
# 
# tag = c("caa", "waa", "maa")
# for(i in tag){
#   data = get(paste0(i))
#   assign(paste0(i),
#          data[, -ncol(data)])
# }

dat0 = data.handler(caa, waa, maa, cpue0, M = 0.4)
dat1 = data.handler(caa, waa, maa, cpue1, M = 0.4)
dat2 = data.handler(caa, waa, maa, cpue2, M = 0.4)
dat3 = data.handler(caa, waa, maa, cpue3, M = 0.4)

# # 2005 & 2006 を除く
# dat0$index[2,as.character(2005:2006)] <- NA
# dat1$index[2,as.character(2005:2006)] <- NA
# dat2$index[2,as.character(2005:2006)] <- NA
# dat3$index[2,as.character(2005:2006)] <- NA

# 
# vpa_retro_2step = function(dat, retro_year = 5) {
#   res0_step1 =  res1.pgs
#   res0_step1$input$dat = dat
#   res0_step1 = do.call(vpa, res0_step1$input)
#   res0_step2 = res_vpa2019
#   res0_step2$input$dat = dat
#   res0_step2$input$sel.f = rev(res0_step1$saa)[,2]
#   res0_step2 = do.call(vpa, res0_step2$input)
#   retro_step1 = retro.est3(res0_step1,n=retro_year)
#   sel_mat = NULL
#   for ( i in 1:retro_year ) {
#     sel_mat = cbind(sel_mat,rev(retro_step1$Res[[i]]$saa)[,2])
#   }
#   retro_step2 = retro.est3(res0_step2, n = retro_year, sel.mat = sel_mat)
#   return(list(res=res0_step2, retro = retro_step2))
# }
# 
# res_dat0 = vpa_retro_2step(dat0)
# colSums(res_dat0$res$ssb)
# res_dat0$retro$mohn
# res_dat0$res$sigma
# 
# res_dat1 = vpa_retro_2step(dat1)
# colSums(res_dat1$res$ssb)
# res_dat1$retro$mohn
# res_dat1$res$sigma
# 
# res_dat2 = vpa_retro_2step(dat2)
# colSums(res_dat2$res$ssb)
# res_dat2$retro$mohn
# res_dat2$res$sigma
# 
# res_dat3 = vpa_retro_2step(dat3)
# names(res_dat3$res)
# colSums(res_dat2$res$ssb)
# res_dat3$retro$mohn
# res_dat3$res$sigma
# 
get_tbl = function(Res, tune = "2steps") {
  mohn_var = c("N2","B2","R2","F","SSB2")
  mohns = Res$retro$mohn[mohn_var]
  names(mohns) = c("mohn_N","mohn_B","mohn_R","mohn_F","mohn_SSB")
  model_tbl = tibble(tune=tune,tune_start = colnames(Res$res$input$dat$index)[!is.na(Res$res$input$dat$index[2,])][1]) %>%
    mutate(term_f = Res$res$term.f, R2018 = Res$res$naa[1,"2018"],B2018 = colSums(Res$res$baa)["2018"]/1000,
           SSB2018 = colSums(Res$res$ssb)["2018"]/1000,SSB2019 = colSums(Res$res$ssb)["2019"]/1000,sigma = mean(Res$res$sigma))
  mohn_tbl = tibble(mohn = mohns,var=names(mohn)) %>% spread(key=var,value=mohn)
  return(bind_cols(model_tbl, mohn_tbl))
}

# 
# summary_table = bind_rows(get_tbl(res_dat0),get_tbl(res_dat1),get_tbl(res_dat2),get_tbl(res_dat3)) %>%
#   mutate(Index_type = c("Egg_abundance","Nominal","VAST_noChub","VAST_Chub"))
# 
# View(summary_table)
# 
# #### 2005~2006を使う ----
# dat0 = data.handler(caa, waa, maa, cpue0, M=0.4)
# dat1 = data.handler(caa, waa, maa, cpue1, M=0.4)
# dat2 = data.handler(caa, waa, maa, cpue2, M=0.4)
# dat3 = data.handler(caa, waa, maa, cpue3, M=0.4)
# 
# res_dat0 = vpa_retro_2step(dat0)
# colSums(res_dat0$res$ssb)
# res_dat0$retro$mohn
# res_dat0$res$sigma
# 
# res_dat1 = vpa_retro_2step(dat1)
# colSums(res_dat1$res$ssb)
# res_dat1$retro$mohn
# res_dat1$res$sigma
# 
# res_dat2 = vpa_retro_2step(dat2)
# colSums(res_dat2$res$ssb)
# res_dat2$retro$mohn
# res_dat2$res$sigma
# 
# res_dat3 = vpa_retro_2step(dat3)
# colSums(res_dat2$res$ssb)
# res_dat3$retro$mohn
# res_dat3$res$sigma
# 
# summary_table2 = bind_rows(get_tbl(res_dat0),get_tbl(res_dat1),get_tbl(res_dat2),get_tbl(res_dat3)) %>%
#   mutate(Index_type = c("Egg_abundance","Nominal","VAST_noChub","VAST_Chub"))
# 
# summary_table = bind_rows(summary_table, summary_table2)
# 
# View(summary_table)

### sel.update = TRUE ----

vpa_retro_sel_update = function(dat, retro_year = 5) {
  # res0_step1 =  res1.pgs #YK added (retro.est3(res0_step1, n = retro_year) でエラー: オブジェクト 'res0_step1' がありません )
  res0_step2 = res_vpa2019
  res0_step2$input$dat = dat
  # res0_step2$input$tf.year = 2014:2017 #year
  res0_step2$input$tf.year = 2013:2017 #year
  res0_step2$input$sel.f = NULL
  res0_step2$input$sel.update = TRUE
  res0_step2 = do.call(vpa, res0_step2$input)
  # retro_step1 = retro.est3(res0_step1,n=retro_year)
  retro_step2 = retro.est3(res0_step2, n = retro_year, sel.mat = NULL)
  return(list(res=res0_step2, retro = retro_step2))
}

#raw
res_dat01 = vpa_retro_sel_update(dat0)
colSums(res_dat01$res$ssb)
res_dat01$retro$mohn
res_dat01$res$sigma

#nominal
res_dat11 = vpa_retro_sel_update(dat1)
colSums(res_dat11$res$ssb)
res_dat11$retro$mohn
res_dat11$res$sigma

#chub-
res_dat21 = vpa_retro_sel_update(dat2)
colSums(res_dat21$res$ssb)
res_dat21$retro$mohn
res_dat21$res$sigma

#chub+
res_dat31 = vpa_retro_sel_update(dat3)
colSums(res_dat31$res$ssb)
res_dat31$retro$mohn
res_dat31$res$sigma

summary_table3 = bind_rows(get_tbl(res_dat01, tune = "sel_update"),
                           get_tbl(res_dat11, tune = "sel_update"),
                           get_tbl(res_dat21, tune = "sel_update"),
                           get_tbl(res_dat31, tune = "sel_update")) %>%
  mutate(Index_type = c("Egg_abundance","Nominal","VAST_noChub","VAST_Chub"), tf_yr = "2013:2017", sel_def = "max")


# summary_table = bind_rows(summary_table, summary_table3)
# 
# summary_table = summary_table %>% mutate(
#   sel_def = "max"
# )

#参照年を変更
vpa_retro_sel_update = function(dat, retro_year = 5) {
  res0_step1 =  res1.pgs #YK added (retro.est3(res0_step1, n = retro_year) でエラー: オブジェクト 'res0_step1' がありません )
  res0_step2 = res_vpa2019
  res0_step2$input$dat = dat
  # res0_step2$input$tf.year = 2016:2017 #year これもミス?
  res0_step2$input$tf.year = 2015:2017 #year
  res0_step2$input$sel.f = NULL
  res0_step2$input$sel.update = TRUE
  res0_step2 = do.call(vpa, res0_step2$input)
  retro_step1 = retro.est3(res0_step1,n=retro_year)
  retro_step2 = retro.est3(res0_step2, n = retro_year, sel.mat = NULL)
  return(list(res=res0_step2, retro = retro_step2))
}

#raw
res_dat0 = vpa_retro_sel_update(dat0)
colSums(res_dat0$res$ssb)
res_dat0$retro$mohn
res_dat0$res$sigma

#nominal
res_dat1 = vpa_retro_sel_update(dat1)
colSums(res_dat1$res$ssb)
res_dat1$retro$mohn
res_dat1$res$sigma

#chub-
res_dat2 = vpa_retro_sel_update(dat2)
colSums(res_dat2$res$ssb)
res_dat2$retro$mohn
res_dat2$res$sigma

#chub+
res_dat3 = vpa_retro_sel_update(dat3)
colSums(res_dat2$res$ssb)
res_dat3$retro$mohn
res_dat3$res$sigma

summary_table4 = bind_rows(get_tbl(res_dat0, tune = "sel_update"),
                           get_tbl(res_dat1, tune = "sel_update"),
                           get_tbl(res_dat2, tune = "sel_update"),
                           get_tbl(res_dat3, tune = "sel_update")) %>%
  mutate(Index_type = c("Egg_abundance","Nominal","VAST_noChub","VAST_Chub"), tf_yr = "2015:2017", sel_def = "max")

summary_table = bind_rows(summary_table3, summary_table4)

# summary_table = summary_table %>% mutate(
#   sel_def = "max"
# )
write.csv(summary_table, "summary_table.csv")









# fig -----------------------------------------------------------
DF0 = NULL; DF1 = NULL; DF2 = NULL; DF3 = NULL; DF4 = NULL; DF5 = NULL; DF6 = NULL
for(j in 1:5){
  data = res_dat01
  
  df0 = data_frame(colSums(data$res$wcaa))
  colnames(df0) = "value"
  df0 = df0 %>% mutate(year = rep(1995:2019), ret_yr = 0, type = "wcaa", index = "Nominal")
  DF0 = rbind(DF0, df0)
  df0 = data_frame(colSums(data$res$naa))
  colnames(df0) = "value"
  df0 = df0 %>% mutate(year = rep(1995:2019), ret_yr = 0, type = "naa", index = "Nominal")
  DF0 = rbind(DF0, df0)
  df0 = data_frame(colSums(data$res$faa))
  colnames(df0) = "value"
  df0 = df0 %>% mutate(year = rep(1995:2019), ret_yr = 0, type = "faa", index = "Nominal")
  DF0 = rbind(DF0, df0)
  df0 = data_frame(colSums(data$res$baa))
  colnames(df0) = "value"
  df0 = df0 %>% mutate(year = rep(1995:2019), ret_yr = 0, type = "baa", index = "Nominal")
  DF0 = rbind(DF0, df0)
  df0 = data_frame(colSums(data$res$ssb))
  colnames(df0) = "value"
  df0 = df0 %>% mutate(year = rep(1995:2019), ret_yr = 0, type = "ssb", index = "Nominal")
  DF0 = rbind(DF0, df0)
  df0 = data_frame(colSums(data$res$saa))
  colnames(df0) = "value"
  df0 = df0 %>% mutate(year = rep(1995:2019), ret_yr = 0, type = "saa", index = "Nominal")
  DF0 = rbind(DF0, df0)
  
  df1 = data_frame(colSums(data$retro$Res[[j]]$wcaa))
  colnames(df1) = "value"
  df1 = df1 %>% mutate(year = rep(1995:(2019-j)), ret_yr = paste0(j), type = "wcaa", index = "Nominal")
  DF1 = rbind(DF1, df1)
  
  
  df2 = data_frame(colSums(data$retro$Res[[j]]$naa))
  colnames(df2) = "value"
  df2 = df2 %>% mutate(year = rep(1995:(2019-j)), ret_yr = paste0(j), type = "naa", index = "Nominal")
  DF2 = rbind(DF2, df2)
  
  df3 = data_frame(colSums(data$retro$Res[[j]]$faa))
  colnames(df3) = "value"
  df3 = df3 %>% mutate(year = rep(1995:(2019-j)), ret_yr = paste0(j), type = "faa", index = "Nominal")
  DF3 = rbind(DF3, df3)
  
  df4 = data_frame(colSums(data$retro$Res[[j]]$baa))
  colnames(df4) = "value"
  df4 = df4 %>% mutate(year = rep(1995:(2019-j)), ret_yr = paste0(j), type = "baa", index = "Nominal")
  DF4 = rbind(DF4, df4)
  
  df5 = data_frame(colSums(data$retro$Res[[j]]$ssb))
  colnames(df5) = "value"
  df5 = df5 %>% mutate(year = rep(1995:(2019-j)), ret_yr = paste0(j), type = "ssb", index = "Nominal")
  DF5 = rbind(DF5, df5)
  
  df6 = data_frame(colSums(data$retro$Res[[j]]$saa))
  colnames(df6) = "value"
  df6 = df6 %>% mutate(year = rep(1995:(2019-j)), ret_yr = paste0(j), type = "saa", index = "Nominal")
  DF6 = rbind(DF6, df6)
}
retro_dat0_0 = DF0
retro_wcaa_dat0 = DF1
retro_naa_dat0 = DF2
retro_faa_dat0 = DF3
retro_baa_dat0 = DF4
retro_ssb_dat0 = DF5
retro_saa_dat0 = DF6

DF0 = NULL; DF1 = NULL; DF2 = NULL; DF3 = NULL; DF4 = NULL; DF5 = NULL; DF6 = NULL
for(j in 1:5){
  data = res_dat21
  
  df0 = data_frame(colSums(data$res$wcaa))
  colnames(df0) = "value"
  df0 = df0 %>% mutate(year = rep(1995:2019), ret_yr = 0, type = "wcaa", index = "Chub-")
  DF0 = rbind(DF0, df0)
  df0 = data_frame(colSums(data$res$naa))
  colnames(df0) = "value"
  df0 = df0 %>% mutate(year = rep(1995:2019), ret_yr = 0, type = "naa", index = "Chub-")
  DF0 = rbind(DF0, df0)
  df0 = data_frame(colSums(data$res$faa))
  colnames(df0) = "value"
  df0 = df0 %>% mutate(year = rep(1995:2019), ret_yr = 0, type = "faa", index = "Chub-")
  DF0 = rbind(DF0, df0)
  df0 = data_frame(colSums(data$res$baa))
  colnames(df0) = "value"
  df0 = df0 %>% mutate(year = rep(1995:2019), ret_yr = 0, type = "baa", index = "Chub-")
  DF0 = rbind(DF0, df0)
  df0 = data_frame(colSums(data$res$ssb))
  colnames(df0) = "value"
  df0 = df0 %>% mutate(year = rep(1995:2019), ret_yr = 0, type = "ssb", index = "Chub-")
  DF0 = rbind(DF0, df0)
  df0 = data_frame(colSums(data$res$saa))
  colnames(df0) = "value"
  df0 = df0 %>% mutate(year = rep(1995:2019), ret_yr = 0, type = "saa", index = "Chub-")
  DF0 = rbind(DF0, df0)
  
  df1 = data_frame(colSums(data$retro$Res[[j]]$wcaa))
  colnames(df1) = "value"
  df1 = df1 %>% mutate(year = rep(1995:(2019-j)), ret_yr = paste0(j), type = "wcaa", index = "Chub-")
  DF1 = rbind(DF1, df1)
  
  df1 = data_frame(colSums(data$retro$Res[[j]]$wcaa))
  colnames(df1) = "value"
  df1 = df1 %>% mutate(year = rep(1995:(2019-j)), ret_yr = paste0(j), type = "wcaa", index = "Chub-")
  DF1 = rbind(DF1, df1)
  
  df2 = data_frame(colSums(data$retro$Res[[j]]$naa))
  colnames(df2) = "value"
  df2 = df2 %>% mutate(year = rep(1995:(2019-j)), ret_yr = paste0(j), type = "naa", index = "Chub-")
  DF2 = rbind(DF2, df2)
  
  df3 = data_frame(colSums(data$retro$Res[[j]]$faa))
  colnames(df3) = "value"
  df3 = df3 %>% mutate(year = rep(1995:(2019-j)), ret_yr = paste0(j), type = "faa", index = "Chub-")
  DF3 = rbind(DF3, df3)
  
  df4 = data_frame(colSums(data$retro$Res[[j]]$baa))
  colnames(df4) = "value"
  df4 = df4 %>% mutate(year = rep(1995:(2019-j)), ret_yr = paste0(j), type = "baa", index = "Chub-")
  DF4 = rbind(DF4, df4)
  
  df5 = data_frame(colSums(data$retro$Res[[j]]$ssb))
  colnames(df5) = "value"
  df5 = df5 %>% mutate(year = rep(1995:(2019-j)), ret_yr = paste0(j), type = "ssb", index = "Chub-")
  DF5 = rbind(DF5, df5)
  
  df6 = data_frame(colSums(data$retro$Res[[j]]$saa))
  colnames(df6) = "value"
  df6 = df6 %>% mutate(year = rep(1995:(2019-j)), ret_yr = paste0(j), type = "saa", index = "Chub-")
  DF6 = rbind(DF6, df6)
}
retro_dat2_0 = DF0
retro_wcaa_dat2 = DF1
retro_naa_dat2 = DF2
retro_faa_dat2 = DF3
retro_baa_dat2 = DF4
retro_ssb_dat2 = DF5
retro_saa_dat2 = DF6

DF0 = NULL; DF1 = NULL; DF2 = NULL; DF3 = NULL; DF4 = NULL; DF5 = NULL; DF6 = NULL
for(j in 1:5){
  data = res_dat31
  
  df0 = data_frame(colSums(data$res$wcaa))
  colnames(df0) = "value"
  df0 = df0 %>% mutate(year = rep(1995:2019), ret_yr = 0, type = "wcaa", index = "Chub+")
  DF0 = rbind(DF0, df0)
  df0 = data_frame(colSums(data$res$naa))
  colnames(df0) = "value"
  df0 = df0 %>% mutate(year = rep(1995:2019), ret_yr = 0, type = "naa", index = "Chub+")
  DF0 = rbind(DF0, df0)
  df0 = data_frame(colSums(data$res$faa))
  colnames(df0) = "value"
  df0 = df0 %>% mutate(year = rep(1995:2019), ret_yr = 0, type = "faa", index = "Chub+")
  DF0 = rbind(DF0, df0)
  df0 = data_frame(colSums(data$res$baa))
  colnames(df0) = "value"
  df0 = df0 %>% mutate(year = rep(1995:2019), ret_yr = 0, type = "baa", index = "Chub+")
  DF0 = rbind(DF0, df0)
  df0 = data_frame(colSums(data$res$ssb))
  colnames(df0) = "value"
  df0 = df0 %>% mutate(year = rep(1995:2019), ret_yr = 0, type = "ssb", index = "Chub+")
  DF0 = rbind(DF0, df0)
  df0 = data_frame(colSums(data$res$saa))
  colnames(df0) = "value"
  df0 = df0 %>% mutate(year = rep(1995:2019), ret_yr = 0, type = "saa", index = "Chub+")
  DF0 = rbind(DF0, df0)
  
  df1 = data_frame(colSums(data$retro$Res[[j]]$wcaa))
  colnames(df1) = "value"
  df1 = df1 %>% mutate(year = rep(1995:(2019-j)), ret_yr = paste0(j), type = "wcaa", index = "Chub+")
  DF1 = rbind(DF1, df1)
  
  df2 = data_frame(colSums(data$retro$Res[[j]]$naa))
  colnames(df2) = "value"
  df2 = df2 %>% mutate(year = rep(1995:(2019-j)), ret_yr = paste0(j), type = "naa", index = "Chub+")
  DF2 = rbind(DF2, df2)
  
  df3 = data_frame(colSums(data$retro$Res[[j]]$faa))
  colnames(df3) = "value"
  df3 = df3 %>% mutate(year = rep(1995:(2019-j)), ret_yr = paste0(j), type = "faa", index = "Chub+")
  DF3 = rbind(DF3, df3)
  
  df4 = data_frame(colSums(data$retro$Res[[j]]$baa))
  colnames(df4) = "value"
  df4 = df4 %>% mutate(year = rep(1995:(2019-j)), ret_yr = paste0(j), type = "baa", index = "Chub+")
  DF4 = rbind(DF4, df4)
  
  df5 = data_frame(colSums(data$retro$Res[[j]]$ssb))
  colnames(df5) = "value"
  df5 = df5 %>% mutate(year = rep(1995:(2019-j)), ret_yr = paste0(j), type = "ssb", index = "Chub+")
  DF5 = rbind(DF5, df5)
  
  df6 = data_frame(colSums(data$retro$Res[[j]]$saa))
  colnames(df6) = "value"
  df6 = df6 %>% mutate(year = rep(1995:(2019-j)), ret_yr = paste0(j), type = "saa", index = "Chub+")
  DF6 = rbind(DF6, df6)
}
retro_dat3_0 = DF0
retro_wcaa_dat3 = DF1
retro_naa_dat3 = DF2
retro_faa_dat3 = DF3
retro_baa_dat3 = DF4
retro_ssb_dat3 = DF5
retro_saa_dat3 = DF6

retro = rbind(retro_wcaa_dat0, retro_wcaa_dat2, retro_wcaa_dat3,
              retro_naa_dat0, retro_naa_dat2, retro_naa_dat3,
              retro_faa_dat0, retro_faa_dat2, retro_faa_dat3,
              retro_baa_dat0, retro_baa_dat2, retro_baa_dat3,
              retro_ssb_dat0, retro_ssb_dat2, retro_ssb_dat3,
              retro_saa_dat0, retro_saa_dat2, retro_saa_dat3,
              retro_dat0_0, retro_dat2_0, retro_dat3_0)
summary(retro)
unique(retro$ret_yr)
retro = retro %>% filter(value != 0)
head(retro)

retro$index = factor(retro$index, levels = c("Nominal", "Chub-", "Chub+"))
retro$type = ifelse(retro$type == "wcaa", "WCAA", ifelse(retro$type == "naa", "Numbers", ifelse(retro$type == "faa", "FAA", ifelse(retro$type == "baa", "Biomass", ifelse(retro$type == "ssb", "SSB", "SAA")))))
write.csv(retro, "retro_13-17.csv")

# なぜか2019年が入ってしまう
select = c("Biomass", "Numbers", "SSB")
fig_retro = retro %>% filter(type %in% select) 

fig_retro = retro %>% filter(type != "WCAA") %>% filter(type != "FAA") %>% filter(type != "SAA")

unique(fig_retro$type)
levels(fig_retro$type)
fig_retro$type = factor(fig_retro$type, levels = c("Numbers", "Biomass", "SSB"))
summary(fig_retro)

g = ggplot(fig_retro, aes(x = year, y = value, colour = as.factor(ret_yr)))
cbPalette = c("gray50", "blue", "cyan", "green", "orange", "red")
pd = position_dodge(.3)
#c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred")
p = geom_point(position = pd)
l = geom_line()
f = facet_grid(type ~ index, scales = "free")
lb = labs(x = "Year", y = "", color = "Removed year", title = "")
th = theme(#legend.position = c(0.18, 0.8),
  #legend.position = c(0.7, 0.9),
  legend.key = element_blank(),
  legend.background = element_blank(),
  axis.text.x = element_text(size = rel(1.5)), #x軸メモリ
  axis.text.y = element_text(size = rel(1.5)), #y軸メモリ
  axis.title.x = element_text(size = rel(1.5)), #x軸タイトル
  axis.title.y = element_text(size = rel(1.5)),
  legend.title = element_text(size = 13), #凡例タイトル
  legend.text = element_text(size = rel(1.5)),
  strip.text = element_text(size = rel(1.3)), #ファセットのタイトル
  plot.title = element_text(size = rel(1.5))) #タイトル
g+p+l+f+theme_bw()+th+lb+scale_x_continuous(breaks = seq(1995, 2018, 5))+scale_colour_manual(values = cbPalette)




# ---------------------------------------------------------------
# こっから下は使っていないコード -----------------------------------------------
# ---------------------------------------------------------------


### sel.def = "mean" ----

vpa_retro_sel_update = function(dat, retro_year = 5) {
  res0_step2 = res_vpa2019
  res0_step2$input$dat = dat
  res0_step2$input$sel.def = "mean"
  res0_step2$input$sel.f = NULL
  res0_step2$input$sel.update = TRUE
  res0_step2 = do.call(vpa, res0_step2$input)
  retro_step1 = retro.est3(res0_step1,n=retro_year)
  retro_step2 = retro.est3(res0_step2, n = retro_year, sel.mat = NULL)
  return(list(res=res0_step2, retro = retro_step2))
}

res_dat0 = vpa_retro_sel_update(dat0)
colSums(res_dat0$res$ssb)
res_dat0$retro$mohn
res_dat0$res$sigma

res_dat1 = vpa_retro_sel_update(dat1)
colSums(res_dat1$res$ssb)
res_dat1$retro$mohn
res_dat1$res$sigma

res_dat2 = vpa_retro_sel_update(dat2)
colSums(res_dat2$res$ssb)
res_dat2$retro$mohn
res_dat2$res$sigma

res_dat3 = vpa_retro_sel_update(dat3)
colSums(res_dat2$res$ssb)
res_dat3$retro$mohn
res_dat3$res$sigma

summary_table4 = bind_rows(get_tbl(res_dat0, tune = "sel_update"),
                           get_tbl(res_dat1, tune = "sel_update"),
                           get_tbl(res_dat2, tune = "sel_update"),
                           get_tbl(res_dat3, tune = "sel_update")) %>%
  mutate(Index_type = c("Egg_abundance","Nominal","VAST_noChub","VAST_Chub"),
         sel_def = "mean")

summary_table = bind_rows(summary_table, summary_table4)

summary_table = summary_table %>% mutate(method = "ls")

### method = "ml" ----

vpa_retro_sel_update = function(dat, retro_year = 5) {
  res0_step2 = res_vpa2019
  res0_step2$input$dat = dat
  res0_step2$input$sel.def = "mean"
  res0_step2$input$est.method = "ml"
  res0_step2$input$sel.f = NULL
  res0_step2$input$sel.update = TRUE
  res0_step2 = do.call(vpa, res0_step2$input)
  retro_step1 = retro.est3(res0_step1,n=retro_year)
  retro_step2 = retro.est3(res0_step2, n = retro_year, sel.mat = NULL)
  return(list(res=res0_step2, retro = retro_step2))
}

res_dat0 = vpa_retro_sel_update(dat0)
colSums(res_dat0$res$ssb)
res_dat0$retro$mohn
res_dat0$res$sigma

res_dat1 = vpa_retro_sel_update(dat1)
colSums(res_dat1$res$ssb)
res_dat1$retro$mohn
res_dat1$res$sigma

res_dat2 = vpa_retro_sel_update(dat2)
colSums(res_dat2$res$ssb)
res_dat2$retro$mohn
res_dat2$res$sigma

res_dat3 = vpa_retro_sel_update(dat3)
colSums(res_dat2$res$ssb)
res_dat3$retro$mohn
res_dat3$res$sigma


summary_table4 = bind_rows(get_tbl(res_dat0, tune = "sel_update"),
                           get_tbl(res_dat1, tune = "sel_update"),
                           get_tbl(res_dat2, tune = "sel_update"),
                           get_tbl(res_dat3, tune = "sel_update")) %>%
  mutate(Index_type = c("Egg_abundance","Nominal","VAST_noChub","VAST_Chub"),
         sel_def = "mean",method = "ml")

summary_table = bind_rows(summary_table, summary_table4)

summary_table = summary_table %>% mutate(b1 = 1, b2 = 1)

summary_table = filter(summary_table, -b1,-b2)


### ridge VPA ----

require(TMB)
use_rvpa_tmb()

base0 = res_dat3$res
base0$input$TMB = TRUE
base0$input$term.F = "all"
base0$input$lambda = 0.01
base0$input$est.method = "ls"
base0 = do.call(vpa, base0$input)
base0$term.f
colSums(base0$ssb)/1000
base0$sigma
base_retro0 = retro.est3(base0)
base_retro0$mohn

### b est ----

## うまくいかない

base0 = res_dat3$res

base = res_dat3$res
base$input$b.est = TRUE
base$input$b.fix = c(NA,1)
base = do.call(vpa, base$input)
base$b
base$sigma

base2 = res_dat3$res
base2$input$b.est = TRUE
base2$input$b.fix = c(NA,1)
base2$input$est.method = "ls"
base2 = do.call(vpa, base2$input)
base2$b
base2$sigma
base2$term.f
colSums(base2$ssb)/1000
colSums(base$ssb)/1000

base_retro = retro.est3(base, b.fix = TRUE)
base_retro$mohn

base_retro = retro.est3(base, b.fix = FALSE)
base_retro$mohn

base_retro2 = retro.est3(base2, b.fix = TRUE)
base_retro2$mohn

base_retro2 = retro.est3(base2, b.fix = FALSE)
base_retro2$mohn

colSums(base0$ssb)

View(summary_table)

write.csv(summary_table, file = "pgm_summary.csv")

