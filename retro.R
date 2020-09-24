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

dat0 = data.handler(caa, waa, maa, cpue0, M = 0.4)
dat1 = data.handler(caa, waa, maa, cpue1, M = 0.4)
dat2 = data.handler(caa, waa, maa, cpue2, M = 0.4)
dat3 = data.handler(caa, waa, maa, cpue3, M = 0.4)


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


# ---------------------------------------------------------------
# retrospective analysis ----------------------------------------
# sel.update = TRUE ---------------------------------------------
# ---------------------------------------------------------------
# make function
vpa_retro_sel_update = function(dat, retro_year, tf.year) {
  res0_step2 = res_vpa2019
  res0_step2$input$dat = dat
  res0_step2$input$tf.year = tf.year #year
  res0_step2$input$sel.f = NULL
  res0_step2$input$sel.update = TRUE
  res0_step2 = do.call(vpa, res0_step2$input)
  retro_step2 = retro.est3(res0_step2, n = retro_year, sel.mat = NULL)
  return(list(res=res0_step2, retro = retro_step2))
}

# set the year for retro and terminal F
retro_year = 5
tf.year = 2014:2017

# run
#nominal
res_dat11 = vpa_retro_sel_update(dat1, retro_year, tf.year)
colSums(res_dat11$res$ssb)
res_dat11$retro$mohn
res_dat11$res$sigma

#chub-
res_dat21 = vpa_retro_sel_update(dat2, retro_year, tf.year)
colSums(res_dat21$res$ssb)
res_dat21$retro$mohn
res_dat21$res$sigma

#chub+
res_dat31 = vpa_retro_sel_update(dat3, retro_year, tf.year)
colSums(res_dat31$res$ssb)
res_dat31$retro$mohn
res_dat31$res$sigma

summary_table3 = bind_rows(# get_tbl(res_dat01, tune = "sel_update"),
                           get_tbl(res_dat11, tune = "sel_update"),
                           get_tbl(res_dat21, tune = "sel_update"),
                           get_tbl(res_dat31, tune = "sel_update")) %>%
  mutate(Index_type = c("Nominal","VAST_noChub","VAST_Chub"), tf_yr = "2013:2017", sel_def = "max")



# sensitivity for terminal year ---------------------------------
tf.year = 2015:2017

# run
#nominal
res_dat1 = vpa_retro_sel_update(dat1, retro_year, tf.year)
colSums(res_dat1$res$ssb)
res_dat1$retro$mohn
res_dat1$res$sigma

#chub-
res_dat2 = vpa_retro_sel_update(dat2, retro_year, tf.year)
colSums(res_dat2$res$ssb)
res_dat2$retro$mohn
res_dat2$res$sigma

#chub+
res_dat3 = vpa_retro_sel_update(dat3, retro_year, tf.year)
colSums(res_dat2$res$ssb)
res_dat3$retro$mohn
res_dat3$res$sigma

summary_table4 = bind_rows(# get_tbl(res_dat0, tune = "sel_update"),
                           get_tbl(res_dat1, tune = "sel_update"),
                           get_tbl(res_dat2, tune = "sel_update"),
                           get_tbl(res_dat3, tune = "sel_update")) %>%
  mutate(Index_type = c("Nominal","VAST_noChub","VAST_Chub"), tf_yr = "2015:2017", sel_def = "max")

summary_table = bind_rows(summary_table3, summary_table4)
write.csv(summary_table, "summary_table.csv")




# fig. 4 --------------------------------------------------------
start = 1995
end = 2019
N1 = NULL
B1 = NULL
S1 = NULL
N2 = NULL
B2 = NULL
S2 = NULL
N3 = NULL
B3 = NULL
S3 = NULL

for(i in 1:3){
  data = get(paste0("res_dat", i, "1"))
  
  if(i == 1){ # Nominal
    for(j in 1:retro_year){
      n0 = data_frame(value = colSums(data$res$naa)) %>% mutate(year = rep(start:end), ret_yr = 0, index = "Nominal") %>% filter(year != end) %>% filter(year != end)
      n_ret = data_frame(value = colSums(data$retro$Res[[j]]$naa)) %>% mutate(year = rep(start:(end-j)), ret_yr = paste(j), index = "Nominal") %>% filter(year != (end-j))
      N1 = rbind(N1, n_ret)
      
      b0 = data_frame(value = colSums(data$res$baa)) %>% mutate(year = rep(start:end), ret_yr = 0, index = "Nominal") %>% filter(year != end) %>% filter(year != end)
      b_ret = data_frame(value = colSums(data$retro$Res[[j]]$baa)) %>% mutate(year = rep(start:(end-j)), ret_yr = paste(j), index = "Nominal") %>% filter(year != (end-j))
      B1 = rbind(B1, b_ret)
      
      s0 = data_frame(value = colSums(data$res$ssb)) %>% mutate(year = rep(start:end), ret_yr = 0, index = "Nominal") %>% filter(year != end) %>% filter(year != end)
      s_ret = data_frame(value = colSums(data$retro$Res[[j]]$ssb)) %>% mutate(year = rep(start:(end-j)), ret_yr = paste(j), index = "Nominal") %>% filter(year != (end-j))
      S1 = rbind(S1, s_ret)
    }
    
    N1 = rbind(n0, N1)
    B1 = rbind(b0, B1)
    S1 = rbind(s0, S1)
  }
  
  if(i == 2){ # Chub-
    for(j in 1:retro_year){
      n0 = data_frame(value = colSums(data$res$naa)) %>% mutate(year = rep(start:end), ret_yr = 0, index = "Chub-") %>% filter(year != end) %>% filter(year != end)
      n_ret = data_frame(value = colSums(data$retro$Res[[j]]$naa)) %>% mutate(year = rep(start:(end-j)), ret_yr = paste(j), index = "Nominal") %>% filter(year != (end-j))
      N2 = rbind(N2, n_ret)
      
      b0 = data_frame(value = colSums(data$res$baa)) %>% mutate(year = rep(start:end), ret_yr = 0, index = "Chub-") %>% filter(year != end) %>% filter(year != end)
      b_ret = data_frame(value = colSums(data$retro$Res[[j]]$baa)) %>% mutate(year = rep(start:(end-j)), ret_yr = paste(j), index = "Nominal") %>% filter(year != (end-j))
      B2 = rbind(B2, b_ret)
      
      s0 = data_frame(value = colSums(data$res$ssb)) %>% mutate(year = rep(start:end), ret_yr = 0, index = "Chub-") %>% filter(year != end) %>% filter(year != end)
      s_ret = data_frame(value = colSums(data$retro$Res[[j]]$ssb)) %>% mutate(year = rep(start:(end-j)), ret_yr = paste(j), index = "Nominal") %>% filter(year != (end-j))
      S2 = rbind(S2, s_ret)
    }
    
    N2 = rbind(n0, N2)
    B2 = rbind(b0, B2)
    S2 = rbind(s0, S2)
  }
  
  if(i == 3){ # Chub+
    for(j in 1:retro_year){
      n0 = data_frame(value = colSums(data$res$naa)) %>% mutate(year = rep(start:end), ret_yr = 0, index = "Chub+") %>% filter(year != end) %>% filter(year != end)
      n_ret = data_frame(value = colSums(data$retro$Res[[j]]$naa)) %>% mutate(year = rep(start:(end-j)), ret_yr = paste(j), index = "Nominal") %>% filter(year != (end-j))
      N3 = rbind(N3, n_ret)
      
      b0 = data_frame(value = colSums(data$res$baa)) %>% mutate(year = rep(start:end), ret_yr = 0, index = "Chub+") %>% filter(year != end) %>% filter(year != end)
      b_ret = data_frame(value = colSums(data$retro$Res[[j]]$baa)) %>% mutate(year = rep(start:(end-j)), ret_yr = paste(j), index = "Nominal") %>% filter(year != (end-j))
      B3 = rbind(B3, b_ret)
      
      s0 = data_frame(value = colSums(data$res$ssb)) %>% mutate(year = rep(start:end), ret_yr = 0, index = "Chub+") %>% filter(year != end) %>% filter(year != end)
      s_ret = data_frame(value = colSums(data$retro$Res[[j]]$ssb)) %>% mutate(year = rep(start:(end-j)), ret_yr = paste(j), index = "Nominal") %>% filter(year != (end-j))
      S3 = rbind(S3, s_ret)
    }
    
    N3 = rbind(n0, N3)
    B3 = rbind(b0, B3)
    S3 = rbind(s0, S3)
  }
  N = rbind(N1, N2, N3) %>% mutate(type = "Numbers")
  B = rbind(B1, B2, B3) %>% mutate(type = "Biomass")
  S = rbind(S1, S2, S3) %>% mutate(type = "SSB")
  retro = rbind(N, B, S)
}

retro$index = factor(retro$index, levels = c("Nominal", "Chub-", "Chub+"))
retro$type = factor(retro$type, levels = c("Numbers", "Biomass", "SSB"))
write.csv(retro, "retro_13-17.csv")

levels(fig_retro$type)
summary(retro)

g = ggplot(retro, aes(x = year, y = value, colour = as.factor(ret_yr)))
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

