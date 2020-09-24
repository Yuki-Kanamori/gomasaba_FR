
#-- 下準備 ----

vpaname = vpaname_[i]

#-- MSY管理基準値を計算する ----
#-- 1) 入出力ファイル　----
#-- 1-1) 読み込みファイル ----
#--- VPA結果が保存されているファイルの名前
vpa_file_path_MSY <- paste0("res_vpa_",vpaname,".rda")

#vpa_file_path_MSY <- "../data_SC2020/res_vpa_PMA2019assess.rda"

#--- VPAの結果のファイルの種類（1: Rオブジェクト(save(vpa_res,file="vpa_res.rda")), 2: csv形式(out.vpa(vpa_res)))
vpa_file_type_MSY <- 1

#-- 1-2) 出力ファイルを置くフォルダの名前  ----

output_folder <- paste0("output_",vpaname)
#output_folder <- "output_PMA/RIBHA1ave3000"

#--- 実行したときの警告を表示するかどうか (-1: 表示しない, 0: 表示する)
warning_option <- -1
#--- graph_file_MSYをPDFで出力するときの日本語フォントの設定。windows, Linuxであれば以下の"Japan1GothicBBB"でOKそうです。Macの人は適当なものを自分で指定してください。 !!! Yield curveのプロットだけまだうまく出力されません!!!通常は、PNGファイルでの出力をおすすめします。
font_family <- "Japan1GothicBBB"

#-- 2) 再生産関係のフィット ----
#--- 再生産関係をあてはめる年の範囲 0: 全部の年のデータを使う, マイナスの数字: 指定された年数分、最近年データを除く, 正の数字: 指定された数字に一致する年のデータを用いる
year_for_srfit <- 0
#--- 再生産関係の式 (1: HS, 2: BH, 3: RI)
function_srfit <- 3
#--- 最小化手法 (1: 最小絶対値, 2: 最小二乗)
L1L2_srfit <- 1
#--- 自己相関の考慮 (0: なし, 1: あり)
AR_srfit <- 0
#--- 自己相関を計算するときに、自己相関を外側で計算する(1)か、尤度に組み込んで内側で計算する(2)か。推奨は外側(1)。
AR_estimation <- 1
#--- レジームシフトを考慮する (0: 考慮しない, 1: 考慮する)
#---   考慮する場合、上で指定された再生産関係の式と最小化手法が使われますが、自己相関は考慮されません
#---   また、レジームシフトを考慮する場合には、モデル診断と網羅的な再生産関係のフィット、モデル平均は
#---   使えません
use_regime_model <- 0
if(use_regime_model==1){
    regime_year_MSY <- c(1976,1988) # 新しいレジームがスタートする年
    regime_key_MSY  <- c(0, 1, 0)　# どの期間を同じレジームと判断するか
    regime_independent_pars_MSY <- c("a","b") # レジーム間で独立と仮定するパラメータ: "a", "b", "sd"の３つから選ぶ
    future_regime_MSY <- 0 # regime_key_MSYで定義したどのレジームが将来おこると仮定するか
    future_regime_scenario <- NULL # 将来の期間にregimeが変わると仮定した場合のシナリオ (未実装)
}
#--- 再生産関係を網羅的にフィットしてその結果を表にするかどうか(0: しない, 1: する(時間がちょっとかかります))
make_SRmodel_table <- 0
if(make_SRmodel_table==1){
    #--- AICを網羅的に検討したその結果を保存するファイルの名前(csvファイル)
    SR_compared_file_path <- "model_selection.csv"
}
#--- 再生産関係を平均する(0: 平均しない、1: 平均する)
average_SRmodel <- 0
if(average_SRmodel==1){
    set_average_SRmodel  <- tibble(SR.rel=c("RI","BH"),
                                  L.type=c("L2","L2"),
                                  AR=c(1,1),
                                  out.AR=c(FALSE,FALSE),
                                  weight=c(0.71,0.29))
}
#--- データから推定するのではなく、外部からパラメータを指定して与える（暫定的） (0; 与えない、1; 与える)
#--- 関数型は上で設定されたものが用いられます
given_SR_pars <- 0  # 有効にする場合は1
if(given_SR_pars==1){
    # a, b, sd, rhoの順番です
    SR_specific_par_MSY <- c(0.030, 1477000, 0.60, 0)
}

#-- 3) MSY推定の設定（F一定の条件下での将来予測をもとにする） ----
#--- 3-1) 選択率の設定　----
#--- MSY推定をするかどうか（1: する, 0: しない）
do_MSY_estimation <- 1
#--- MSY推定で用いる選択率（近年の選択率がそのまま将来も受け継がれるという仮定）
#---   (1: vpaの結果の中のFc.at.ageをそのまま使う; ややこしいので廃止予定)
#---   2: 手動でFcurrentを設定する
#---   3: vpaのF at ageに対して年を指定し、その平均を使う,
#---   4: 選択率を参照する年と漁獲圧を参照する年を別にする（漁獲圧はSPR換算して、指定された選択率において同じ漁獲圧になるようなFcurrentを用いる。SPR換算するときの生物パラメータは、漁獲圧として指定された年の生物パラメータの平均とする））
#---  5: 選択率を手動で打ち込んだあとに、Fの強さは年数指定する（未実装）
select_Fcurrent_MSY <- 3
#---- 上で2を選んだ場合:FcurrentとしたいFをベクトルで入力
if(select_Fcurrent_MSY==2){
    Fcurrent_MSY <- c(0.1, 0.2, 0.3, 0.3)
}
#---- 上で3を選んだ場合:Fを平均したい年数
if(select_Fcurrent_MSY==3){
    # 実際の年を指定する場合
    Fcurrent_year_MSY <- 2014:2018
    # マイナス値を入れると最新年から数えて何年分遡るかで指定できる
    #Fcurrent_year_MSY <- -1:-3
}
#---- 上で4を選んだ場合:選択率参照年とFcurrent参照年を分ける
if(select_Fcurrent_MSY==4){
    # 漁獲圧の強さとして近年の漁獲圧を代表とする年(実際の年を指定するか、マイナス値で相対値として指定する）
    # Fcurrent_year_MSY <- 2015:2017
    Fcurrent_year_MSY <- -1:-3
    # 選択率として近年の漁獲圧を代表とする年（マイナス値も可）
    #Fsel_year_MSY <- 2010:2017
    Fsel_year_MSY <- -1:-10
}

#--- 3-2) 各種計算方法　----
#--- 漁獲量の計算方法（1:VPAと同じ設定を使う, 2:Popeの近似式を使う, 3:Bavanovの式を使う）
is_pope <- 1
#--- 乱数のシード
MSY_seed <- 2020
#--- MSY計算時のシミュレーション回数(1000回以上推奨)
MSY_nsim <- 10000
#--- MSYの推定方法（0: R, 1: TMB）
#---- 1の場合TMBをインストールしてください
optim_method_msy <-1
#--- MSYの推定方法としてTMBを選んだ場合、初回の計算時にコンパイルをおこなうか(0: おこなわない、1: おこなう)
compile_tmb <- ifelse(i==1,1,0)
#--- 漁獲量曲線の計算などをする場合に並列計算をするか(0: しない, 1以上: 用いるコアの数)(windowsでは使えません)
ncore <- 0
#--- 平衡状態にあると仮定する年の決め方（1: 世代時間から計算する, 2: 具体的な年数を与える）
select_nyear_MSY <- 1
#---- 世代時間から計算する場合
if(select_nyear_MSY==1){
    #--- 世代時間の推定方法（0: 自動的に計算, 1以上の数：ここで指定された値を世代時間（年）とする）
    select_GT <- 0
    #--- 世代時間の何倍を平衡状態に置くか（デフォルトは20)
    GT_multi <- 20
}
#---- 具体的な年数を与える場合
if(select_nyear_MSY==2){
    nyear_MSY <- 400
}
#--- 複数シミュレーションの結果をあわせて漁獲量を最大化するときの統計量
#---- * 算定ルールでは1（平均）を使うことになっている。 1: 平均（デフォルト）, 2: 中央値
stat_maximize <- 1

#-- 3-3) 生物パラメータ
#--- MSY計算時の年齢別体重(資源量計算用)の設定(1:年で指定する、2:直接指定する)
select_waa_in_MSY <- 1
if(select_waa_in_MSY==1){ # 1の場合にはこちらを設定
    waa_year_in_MSY <- 2014:2018
}
if(select_waa_in_MSY==2){ # 2の場合にはこちらを設定。年毎に異る場合は年齢×年の行列を入れる
    waa_in_MSY <- c(100,200,300,400)
}

#--- MSY計算時の年齢別体重(漁獲量計算用)の設定(0:資源量計算用と同じ、1:年数で指定、2:直接指定)
select_waa.catch_in_MSY <- 0
if(select_waa.catch_in_MSY==1){ # 1の場合にはこちらを設定
    waa.catch_year_in_MSY <- 2016:2018
}
if(select_waa.catch_in_MSY==2){ # 2の場合にはこちらを設定。年毎に異る場合は年齢×年の行列を入れる
    waa.catch_in_MSY <- c(100,200,300,400)
}

#--- MSY計算時の年齢別成熟率の設定(1:年数で指定、2:直接指定)
select_maa_in_MSY <- 1
if(select_maa_in_MSY==1){ # 1の場合にはこちらを設定
    maa_year_in_MSY <- 2014:2018
}
if(select_maa_in_MSY==2){ # 2の場合にはこちらを設定。年毎に異る場合は年齢×年の行列を入れる
    maa_in_MSY <- c(0,0,0.5,1)
}

#--- MSY計算時の自然死亡係数の設定(1:年数で指定、2:直接指定)
select_M_in_MSY <- 1
if(select_M_in_MSY==1){ # 1の場合にはこちらを設定
    M_year_in_MSY <- 2014:2018
}
if(select_M_in_MSY==2){ # 2の場合にはこちらを設定。年毎に異る場合は年齢×年の行列を入れる
    M_in_MSY <- c(0,0,0.5,1)
}

#--- 体重が密度依存で変化するオプションを利用する(0:利用しない, 1:利用する)
#--- * この場合、漁獲量計算用と資源量計算用のweight at ageは同じものを利用します
waa_fun_MSY <- 0

#--- 3-4) 再生産の仮定　----
#---  バイアス補正（シミュレーションの平均と決定論的予測が一致するようにする）
#---  (1:する(デフォルト)、1以外: しない)
bias_correction_MSY <- 1
#--- 加入変動の誤差分布("lognormal": 対数正規誤差, "resample": 残差リサンプリング)
#---- 残差の分布がモデル診断の結果表示されるので、その分布を見て正規分布に近ければ"lognormal"を選んで
#---- 問題ないです。あからさまに変な分布だったら残差リサンプリングも検討してみてください。
#---- "backward"は、過去をブロックに区切って、段階的にリサンプリングできる期間を増やしていく方法です
SR_error_MSY <- "lognormal" # or "resample" # or "backward"
#---- SR_error_MSY="resample"の場合の設定
if(SR_error_MSY=="resample"){
    #--- リサンプリングの年の範囲
    #--- 0: 再生産関係のパラメータ推定に用いた年全部,
    #--- それ以外の数字: 指定した加入年の残差のみリサンプリング。再生産関係推定から除いた年を入れたい場合はこちらで指定
    select_resample_year <- 0 # or 1990:2000 (バイアス補正はサンプリングした年の範囲の残差のみからおこなうことに注意)
}
#---- SR_error_MSY="backward"の場合の設定
if(SR_error_MSY=="backward"){
    #--- リサンプリングの年の範囲(0: 全年, それ以外の数字: 指定した加入年の残差のみリサンプリング）
    select_resample_year <- 0 # or 1990:2000 (バイアス補正は全年の残差を使っていたかどうか？確認する。)
    backward_duration_MSY <- 5 # 1ブロックに入る年数
}
#---- コンスタントに再生産関係とは独立の加入がある場合(別系群からの移入や放流)の加入尾数
#----- * 通常は0
recruit_intercept_MSY <- 0

#--- 3-5) どのような管理基準値を計算するか　----
#--- 管理基準値を計算するPGYレベル（-1: 計算しない, 0から1までの数字のベクトル: MSYx割合に対応する親魚レベル）
select_PGY <- c(0.6,0.1)
#--- PGYの下側のみの管理基準値を計算する（1: 下側のみ（計算時間短縮）, 2:両側）
only_lowerPGY <- 1
#--- 管理基準値を計算するB0レベル（-1: 計算しない, 0から1までの数字のベクトル: B0x割合に対応する親魚レベル）
select_B0 <- -1
#--- 特定の親魚量レベルを直接指定する（-1: 計算しない, 親魚量のベクトル: その親魚量に維持するときの管理基準値）
# 過去最低の親魚資源量をvpaの結果から持ってくる
select_Babs <- -1 # c(12200, 20000)
#--- 目標管理基準値の選択 (0: MSY,
#---                   1以上の数字: MSY_res$summaryの行数,
#---                   負の数字: インタラクティブに決定)
select_Btarget <- 0 # 負の数字の場合にエラーが出るので修正
#--- 限界管理基準値の選択 (0: 60%MSY,
#---                   1以上の数字: MSY_res$summaryの行数,
#---                   負の数字: インタラクティブに決定)
select_Blimit  <- 0
#--- 禁漁水準の選択      (0: 10%MSY,
#---                   1以上の数字: MSY_res$summaryの行数,
#---                   負の数字: インタラクティブに決定)
select_Bban  <- 0

#------------------------------------
#--- 4) HCRをもとにした将来予測の設定 ---
#---   (上の設定を前提として将来予測をする。かつての2do_future_projection.r) ---
#------------------------------------
#--- 4-1) 各種設定 --------------- ---
#--- 将来予測シミュレーションの乱数のシード
future_seed <- 2020
#--- 将来予測時のシミュレーション回数(1000回以上推奨)
future_nsim <- 10000
#--- 将来予測開始年
future_start_year <- 2019
#--- ABC計算年（この年からHCRに沿った漁獲を開始する）
future_ABC_year   <- 2020
#--- 将来予測の実施年数
future_nyear   <- 40

#--- 特定の年の生物パラメータをMSY計算時のパラメータから置き換える (0: 置き換えない, 1: 置き換える)
set_specific_biopara <- 0
if(set_specific_biopara==1){ # 1の場合、以下の変数を設定する
    # 特に置き換えが必要ないものにはNULLを入れる
    # 置き換えたい場合は、tibble("年"=その年の年齢別パラメータ)のように入れる
    # weight at age
    specific_waa_future <- tibble("2020"=c(10,20,30,40),"2021"=c(20,30,40,40))
    # catch weight at age
    specific_waa.catch_future <- NULL
    # maturity at age
    specific_maa_future <- tibble("2020"=c(0,0.1,0.5,1))
    # M at age
    specific_M_future   <- NULL
}

#--- 体重が密度依存で変化するオプションを利用する(0:利用しない, 1:利用する)
#--- * この場合、漁獲量計算用と資源量計算用のweight at ageは同じものを利用します
waa_fun_future <- 0
if(waa_fun_future==1){
    #--- 密度依存の体重を使い始める年を指定。この年以前は上記で設定したweight at ageが有効になります
    start_waafun_year_name_future <- 2021
}

#-- 直近の加入や漁獲の仮定
#--- 特定の年の加入だけを外部から固定値として与える (0: 設定しない, 1: 設定する)
select_specific_recruit <- 0
#---- 加入を外部から与える場合の設定
if(select_specific_recruit==1){
    recruit_year <- c(2018,2019,2020,2021)
    recruit_number <- c(10000,20000,20000,20000)
}
#--- 特定の年の漁獲量を外部から与える (0: 設定しない, 1: 設定する)
select_specific_catch <- 0
#---- 漁獲量を外部から与える場合の設定
if(select_specific_catch==1){
    catch_year <- c(2018,2019)
    catch_weight <- c(1000,1000)
}
#---- 短期的将来予測のさいに、再生産関係と独立の加入を考慮する場合(別系群からの移入や放流)の加入尾数
#---- MSY計算のときの設定は上書きされる
#----- * 通常は0
recruit_intercept_future <- 0
#---- 短期的将来予測のときにrhoを変更する（0; しない, 1:する）；感度分析用として利用
set_future_rho <- 0
if(set_future_rho==1) future_rho <- 0

#--- 上記の設定以外に加えて、将来予測の加入の残差の仮定をMSY計算時と別にする場合
#--- 再生産関係の形自体は変えられないので注意
#----  （0:別にしない＝MSY計算と同じ仮定を用いる。1: 別に設定する）
set_specific_future_recruit <- 0
if(set_specific_future_recruit==1){
    # バイアス補正するか？
    bias_correction_future <- 1
    #--- 加入変動の誤差分布("lognormal": 対数正規誤差, "resample": 残差リサンプリング)
    SR_error_future <- "lognormal" # or "resample" # or "backward"
    #---- SR_error_future="resample"の場合の設定
    if(SR_error_future=="resample"){
        #--- 0: 再生産関係のパラメータ推定に用いた年全部,
        #--- それ以外の数字: 指定した加入年の残差のみリサンプリング。再生産関係推定から除いた年を入れたい場合はこちらで指定        
        select_resample_year_future <- 0 # or 1990:2000 (バイアス補正はサンプリングした年の範囲の残差のみから実施)
}
    #---- SR_error_MSY="backward"の場合の設定
    if(SR_error_future=="backward"){
        #--- リサンプリングの年の範囲(0: 全年, それ以外の数字: 指定した加入年の残差のみリサンプリング）
        select_resample_year_future <- 0 # or 1990:2000 (バイアス補正は全年の残差を使っている)
        backward_duration_future <- 5 # 1ブロックに入る年数
    }
}

#-- 4-2) HCRシナリオの設定 ----
#--- HCRを実施するときのSSB参照年 (0: ABC算定年とSSBの参照年が同じ。0より小さい負の整数：時間遅れの年数（２年遅れの場合は-2）。デフォルトは0)
HCR_year_lag <- 0
#---- HCRの将来予測におけるデフォルトのベータ(通常は0.8)
beta_default <- 0.9
#---- ベータをいろいろ変える将来予測におけるベータの値
beta_table <- c(0.6,0.7,0.8,0.9,1)

#-- 4-3) 将来の漁獲の強さや選択率の設定 ----
#--  * HCRをもとにした将来予測では、ABC計算年以前にはselect_FAA_preABCで設定された年齢別漁獲係数で漁獲し、
#--    ABC計算年以降はselect_FAA_afterABCで設定された年齢別漁獲係数×β×γで漁獲します)
#--    これとは別に、select_FAA_preABCで設定された年齢別漁獲係数で漁獲しつづける将来予測(「現在の漁獲圧」シナリオ)も実施します。
#---
#--- ABC計算年以前の漁獲係数(FAA_preABC)の設定(デフォルトは3または4で、ABC算定年以前のF at ageを最もよく代表していると思われるFを用いる)
#---   1: Fmsy at age (=MSY_resで"Btarget0"の管理基準値に対応するF at age)
#---   2: 手動でFcurrentを設定する
#---   3: vpaのF at ageに対して年を指定し、その平均を使う,
#---   4: 選択率を参照する年と漁獲圧を参照する年を別にする（漁獲圧はSPR換算して、指定された選択率においてx同じ漁獲圧になるようなFcurrentを用いる。SPR換算するときの生物パラメータは、漁獲圧として指定された年の生物パラメータの平均とする））
#---   5??: 漁獲圧はFmsyを使うが、別の選択率を用いる（漁獲圧はSPR換算して、Fmsyと漁獲圧になるようなFcurrentを計算する。SPR換算するときの生物パラメータは、MSY推定に用いた生物パラメータの平均とする）
#--- 6: 選択率はFmsyを使うが、漁獲圧はなんか別のものを使う？？
select_FAA_preABC <- 3
#---- 上で2を選んだ場合:FcurrentとしたいFをベクトルで入力
if(select_FAA_preABC==2){
    FAA_preABC <- c(0.02302929,0.04498867,0.07500238,
                      0.10139408,0.14474343,0.15539237,
                      0.16375070,0.12544913,0.12544913)
}
#---- 上で3を選んだ場合:Fを平均したい年数
if(select_FAA_preABC==3){
    # 実際の年を指定する場合
    FAA_preABC_year <- 2014:2018
    # マイナス値を入れると最新年から数えて何年分遡るかで指定できる
    #FAA_preABC_year <- -1:-3
}
#---- 上で4を選んだ場合:選択率参照年とFcurrent参照年を分ける
if(select_FAA_preABC==4){
    # 漁獲圧の強さとして近年の漁獲圧を代表とする年(実際の年を指定するか、マイナス値で相対値として指定する）
    FAA_preABC_year <- 2015:2017
    # FAA_preABC_year <- -1:-3
    # 選択率として近年の漁獲圧を代表とする年（マイナス値も可）
    Fsel_preABC_year <- 2010:2017
    # Fsel_year <- -1:-10
}
#---- 上で5を選んだ場合:Fmsyをもとにするが別の選択率を用いる
if(select_FAA_preABC==5){
    # 選択率として近年の漁獲圧を代表とする年（マイナス値も可）
    Fsel_preABC_year <- 2010:2017
    # Fsel_year <- -1:-10
}

#--- ABC計算年（HCR適用年）以降に基本となるF-at-age(FAA)の設定(デフォルトは1)
#---   1: Fmsy at age (=MSY_resで"Btarget0"の管理基準値に対応するF at age)
#---   2: ABC.year以前のF at ageと共通
#---   3: 手動で設定する
#---   4: vpaのF at ageに対して年を指定し、その平均を使う,
#---   5: 選択率を参照する年と漁獲圧を参照する年を別にする（漁獲圧はSPR換算して、指定された選択率において同じ漁獲圧になるようなFcurrentを用いる。SPR換算するときの生物パラメータは、漁獲圧として指定された年の生物パラメータの平均とする））
#---   6: 漁獲圧はFmsyを使うが、別の選択率を用いる（漁獲圧はSPR換算して、Fmsyと漁獲圧になるようなFcurrentを計算する。SPR換算するときの生物パラメータは、MSY推定に用いた生物パラメータの平均とする）

select_FAA_afterABC <- 1
#---- 上で3を選んだ場合はこちらも設定する
if(select_FAA_afterABC==3){
    FAA_afterABC <- c(0.2,0.3,0.3,0.44) # ベクトルとして直接指定する場合
}
#---- 上で4を選んだ場合:Fを平均したい年数
if(select_FAA_afterABC==4){
    # 実際の年を指定する場合
    #FAA_afterABC_year <- 2015:2017
    # マイナス値を入れると最新年から数えて何年分遡るかで指定できる
    FAA_afterABC_year <- -1:-3
}
#---- 上で5を選んだ場合:選択率参照年とFcurrent参照年を分ける
if(select_FAA_afterABC==5){
    # 漁獲圧の強さとして近年の漁獲圧を代表とする年(実際の年を指定するか、マイナス値で相対値として指定する）
    FAA_afterABC_year <- 2015:2017
    # FAA_afterABC_year <- -1:-3
    # 選択率として近年の漁獲圧を代表とする年（マイナス値も可）
    Fsel_afterABC_year <- 2010:2017
    # Fsel_afterABC_year <- -1:-10
}
#---- 上で6を選んだ場合:Fmsyをもとにするが別の選択率を用いる
if(select_FAA_afterABC==6){
    # 選択率として近年の漁獲圧を代表とする年（マイナス値も可）
    Fsel_afterABC_year <- 2010:2017
    # Fsel_year <- -1:-10
}

#-- 5) 出力の調整 ----
#-- Kobe plot
#--- 管理基準値にラベルをつけるか？(0: つけない, 1:つける)
put_label_kobe <- 1
#--- Btarget, Blimit, Bbanのラベルを指定(kobe chart以外でも共通のラベルとなる）
label_name_kobe <- c("目標管理基準値案","限界管理基準値案","禁漁水準案")
#--- kobe plot を書く年の範囲 (0: 特に指定しない, 年数(1990:2000とか): その年のデータだけでKobe plotを書く
plot_year_kobe <- 0 #1989:2018
#--- 漁獲量曲線に過去の漁獲量を重ね書きする場合、過去の漁獲量をとる範囲 (0: 全年を指定)
past_year_range_yieldcurve <- 0 # c(1990:2000, 2005:2010) # 左のように、漁獲量を書く年をすべて指定する
#--- 再生産関係の点にラベルをつける年
SRplot_label_year <- c(1995,2000,2005,2010,2015,2018)
#--- y軸 (1: F, 2: U, 3: 両方)
ylabel_kobe <- 3
#--- HCRを重ね書きする場合のbeta (デフォルトは0.8, HCRを重ね書きしない場合は負の値を入れる)
#---- HCRを入れる場合と入れない場合を2つ作る場合はベクトルで入力
#---- ylabel_kobeとのすべての組み合わせのkobe chartが出力される
beta_kobe <- c(-1,0.8)

#--- 将来予測の出力の設定
#---- バイオマスの単位(出力をこの値で割る)。すべてのバイオマスのデフォルトの単位は「トン」と考えている
biomass.unit_MSY <- 1
#---- 加入尾数の単位(出力をこの値で割る)
number.unit_MSY <- 1
#---- 加入尾数の名前(VPA結果そのものの尾数の単位が系群によって異なる。それぞれの系群で適切な単位を入力する
number.name_MSY <- "百万"

#--- 漁獲量曲線を細かく書くか（0: 書かない（計算時間短縮), 1: 書く）
calc_yieldcurve <- 0
#--- βを変えた場合の将来予測について表として出力したい年数を入れるか、表が必要ない場合はマイナス値を入れる
#--- 将来の平均漁獲量
year_catch_average <- c(2019:2031,2041,2051)
#--- 将来の平均親魚量
year_ssb_average <- c(2019:2031,2041,2051)
#--- 目標管理基準値を上回る確率
year_catch_average <- c(2019:2031,2041,2051)
#--- 目標管理基準値を上回る確率
year_ssbtarget_prob <- c(2019:2031,2041,2051)
#--- 限界管理基準値を上回る確率
year_ssblimit_prob  <- c(2019:2031,2041,2051)
#--- 禁漁水準を上回る確率
year_ssbban_prob    <- c(2019:2031,2041,2051)
#--- 過去の最低親魚量を上回る確率
year_ssbmin_prob    <- c(2019:2031,2041,2051)
#--- 過去の最高親魚量を上回る確率
year_ssbmax_prob    <- c(2019:2031,2041,2051)
#--- 漁獲量のAAV
year_catch_aav      <- c(2019:2031,2041,2051)
#--- Fの削減率の平均
year_Fsakugen_mean  <- c(2019:2031,2041,2051)


####################################################
### 以下は基本的には編集しないこと
####################################################


## 出力ファイルの名前
if(!file.exists(output_folder)) dir.create(output_folder, recursive=TRUE)

old.warning <- options()$warn
options(warn=warning_option)
options(tibble.width=Inf)

## define graph object
graph_MSY <- list()

# 1) VPA結果の読み込み
if(vpa_file_type_MSY==1){
    res_vpa_name <- load(vpa_file_path_MSY)
    res_vpa_MSY <- get(res_vpa_name)
}
if(vpa_file_type_MSY==2){
    res_vpa_MSY <- read.vpa(vpa_file_path_MSY)
}

# 最新年を除く
res_vpa_MSY$input$dat$caa <- res_vpa_MSY$input$dat$caa[-ncol(res_vpa_MSY$input$dat$caa)]
res_vpa_MSY$input$dat$maa <- res_vpa_MSY$input$dat$maa[-ncol(res_vpa_MSY$input$dat$maa)]
res_vpa_MSY$input$dat$waa <- res_vpa_MSY$input$dat$waa[-ncol(res_vpa_MSY$input$dat$waa)]
res_vpa_MSY$input$dat$M <- res_vpa_MSY$input$dat$M[-ncol(res_vpa_MSY$input$dat$M)]

res_vpa_MSY$naa <- res_vpa_MSY$naa[-ncol(res_vpa_MSY$naa)]
res_vpa_MSY$faa <- res_vpa_MSY$faa[-ncol(res_vpa_MSY$faa)]
res_vpa_MSY$baa <- res_vpa_MSY$baa[-ncol(res_vpa_MSY$baa)]
res_vpa_MSY$ssb <- res_vpa_MSY$ssb[-ncol(res_vpa_MSY$ssb)] 


# 2) SR関係のフィット

data_SR <- get.SRdata(res_vpa_MSY, return.df=TRUE)
data_SR <- data_SR %>% dplyr::filter(R>0)
SR_weight <- rep(0,nrow(data_SR))
if(year_for_srfit<0) year_for_srfit <- rev(data_SR$year)[year_for_srfit]
if(year_for_srfit>0) SR_weight[data_SR$year %in% year_for_srfit] <- 1
if(year_for_srfit==0) SR_weight[] <- 1

input_SR <- character()
input_SR[1] <- dplyr::case_when(function_srfit==1 ~ "HS",
                                function_srfit==2 ~ "BH",
                                function_srfit==3 ~ "RI")
if(is.na(input_SR[1])) stop("Set appropriate number (1-3) in function_srfit")

input_SR[2] <- dplyr::case_when(L1L2_srfit==1 ~ "L1",
                                L1L2_srfit==2 ~ "L2",)
if(is.na(input_SR[2])) stop("Set appropriate number (1-2) in L1L2_srfit")

if(use_regime_model==0){
    res_SR_MSY <- fit.SR(data_SR,
                         SR      = input_SR[1],
                         method  = input_SR[2],
                         AR      = AR_srfit,
                         out.AR  = switch(AR_estimation,
                                          TRUE,
                                          FALSE,
                                          "Set appropriate number (1-2) in AR_estimation"),
                         hessian = FALSE,
                         w=SR_weight)
    
    # 対馬マアジ用(HSL1A0)の緊急措置
    ### 対馬マアジではパラメータの変化は軽微だったが、データによって影響が大きくなる場合があるので今後確認必要
    ### 今回は、fit.SRのlengthオプションはデフォルトの20を使う 
    if(input_SR[1]=="HS" && input_SR[2]=="L1" && AR_srfit==0){
            # 初期値を変えて最大尤度ももつうちbが中央値のものを選ぶ
        n = 100
        set.seed(12345)
        resSR = res_SR_MSY
        resSR_list = lapply(1:n, function(i) {
            input = resSR$input
            input$p0 = resSR$opt$par + rnorm(length(resSR$opt$par))
            do.call(fit.SR, input)
        })
        loglik_list = sapply(1:n, function(i) resSR_list[[i]]$loglik)
        is_max_loglik = abs(loglik_list-max(loglik_list))<1.0e-6
        b_list = sapply(1:n, function(i) resSR_list[[i]]$pars$b) #bで選ぶ
        id = which.min(abs(b_list-median(b_list[is_max_loglik]))) #中央値に最も近いもの
        res_SR_MSY = resSR_list[[id]]
        check_res_SR_MSY = check.SRfit(res_SR_MSY, output=TRUE, filename = paste0(output_folder,"/check_res_SR_MSY"))
#        out.SR(res_SR_MSY, filename = paste0(output_folder,"/res_SR_MSY"))
    }
}

if(use_regime_model==1){
    res_SR_MSY <- fit.SRregime(data_SR,
                               SR     = input_SR[1],
                               method = input_SR[2],
                               regime.year = regime_year_MSY,
                               regime.key  = regime_key_MSY,
                               regime.par  = regime_independent_pars_MSY,
                               w           = SR_weight)

}

if(make_SRmodel_table==1 && use_regime_model==0){
    #-- 網羅的なパラメータ推定もやる
    select_type <- ifelse(res_SR_MSY$input$AR==0,"non",
                   ifelse(res_SR_MSY$input$out.AR==TRUE,"outer","inner"))

    SRmodel.list <- expand.grid(SR.rel = c("HS","BH","RI"), L.type = c("L1", "L2")) %>%
        as_tibble()

    # frasyr2.01ではL1の場合のSDの計算方法を変更。こちらのスクリプトの変更は必要ないが、
    # 2.01を用いるとここの数字が変わってくる
    SRmodel.list$pars <- purrr::map2(SRmodel.list$SR.rel, SRmodel.list$L.type,
                                     function(x,y){
                                         res1 <- unlist(fit.SR(data_SR, SR = x, method = y, 
                                                               AR = 0, hessian = FALSE, out.AR=TRUE, w=SR_weight)[c("pars","AICc")])
                                         tmp <- fit.SR(data_SR, SR = x, method = y, 
                                                       AR = 1, hessian = FALSE, out.AR=TRUE, w=SR_weight)
                                         res2 <- unlist(tmp[c("pars","AICc")])
                                         res2 <- c(res2,"deltaAIC(AIC_AR-AIC_noAR)"=tmp$AIC.ar[2]-tmp$AIC.ar[1])
                                         res3 <- unlist(fit.SR(data_SR, SR = x, method = y, w=SR_weight,
                                                               AR = 1, hessian = FALSE, out.AR=FALSE)[c("pars","AICc")])
                                         bind_rows(res1,res2,res3,.id="id")
                                     })

    SRmodel.list <- SRmodel.list %>%
        unnest() %>%
        left_join(tibble(id=as.character(1:3),AR.type=c("non","outer","inner"))) %>%
        arrange(AICc,AR.type) %>%
        mutate(selection=ifelse(L.type==res_SR_MSY$input$method &
                                SR.rel==res_SR_MSY$input$SR &
                                AR.type==select_type,"selected",0))%>%
        select(-id)

    ## print results of SR fit
    cat("## --------------------------------------------------------\n")
    cat("## print estimated SR parameters\n")
    cat("## --------------------------------------------------------\n")
    print(SRmodel.list)
    readr::write_csv(SRmodel.list,path=str_c(output_folder,"/",SR_compared_file_path))
    cat("## --------------------------------------------------------\n")
}

if(average_SRmodel==0){
    ## print results of SR fit
    cat("## --------------------------------------------------------\n")
    cat("## print estimated SR parameters\n")
    cat("## --------------------------------------------------------\n")
    as_tibble(res_SR_MSY$pars) %>% mutate(AICc   = res_SR_MSY$AICc,
                                      method = res_SR_MSY$input$method,
                                      type   = res_SR_MSY$input$SR) %>%
        print()
    cat("## --------------------------------------------------------\n")
}

# 3) MSY推定のための将来予測の設定

# 将来予測計算用のデータを構成する

# set future projection year
if(select_nyear_MSY==1){
    if(select_GT==0){
        select_GT <- Generation.Time(res_vpa_MSY,
                                     maa=rowMeans(res_vpa_MSY$input$dat$maa,na.rm=T),
                                     M=rowMeans(res_vpa_MSY$input$dat$M,na.rm=T))
        cat("## -----Generation time: ", select_GT, "-----\n")
    }
    nyear_MSY <- round(select_GT * GT_multi)
}

cat("## --------------------------------------------------------\n")
cat("## -----future projecition year for MSY: ", nyear_MSY, "-----\n")
cat("## --------------------------------------------------------\n")

vpa_years <- colnames(res_vpa_MSY$naa)
future_MSY_year <- vpa_years[apply(res_vpa_MSY$input$dat$caa,2,sum, na.rm=T)>0] %>%
    as.numeric() %>% max()

# set biological parameter
if(select_waa_in_MSY==1) waa_in_MSY <- apply_year_colum(res_vpa_MSY$input$dat$waa, waa_year_in_MSY)
if(select_maa_in_MSY==1) maa_in_MSY <- apply_year_colum(res_vpa_MSY$input$dat$maa, maa_year_in_MSY)
if(select_M_in_MSY  ==1) M_in_MSY   <- apply_year_colum(res_vpa_MSY$input$dat$M  , M_year_in_MSY  )
if(select_waa.catch_in_MSY==0) waa_catch_in_MSY <- waa_in_MSY
if(select_waa.catch_in_MSY==1) waa_catch_in_MSY <- apply_year_colum(res_vpa_MSY$input$dat$waa.catch, waa_year_in_MSY)

# set future F
if(select_Fcurrent_MSY==1) Fcurrent_MSY <- res_vpa_MSY$Fc.at.age
if(select_Fcurrent_MSY==3){
    Fcurrent_MSY <- apply_year_colum(res_vpa_MSY$faa,target_year=Fcurrent_year_MSY)
}
if(select_Fcurrent_MSY==4){
    Fcurrent_MSY <- convert_faa_perSPR(res_vpa_MSY,sel_year=Fsel_year_MSY,
                                       faa_year=Fcurrent_year_MSY)
}

# set pope
is_pope_logical <- dplyr::case_when(is_pope==1 ~ res_vpa_MSY$input$Pope,
                                    is_pope==2 ~ TRUE,
                                    is_pope==3 ~ FALSE)
if(is.na(is_pope_logical)) stop("Set appropriate number (1-3) in is_pope")

if(do_MSY_estimation==1){

    # if averaging model parameter
    {if(average_SRmodel==1){
        res_SR_MSY_average <- purrr::pmap(set_average_SRmodel[,-5],
                                    function(SR.rel,L.type,AR,out.AR)
                                        fit.SR(data_SR,SR=SR.rel,method=L.type,AR=AR,out.AR=out.AR,w=SR_weight))
        average_option <- list(SR_list=res_SR_MSY_average,weight=set_average_SRmodel$weight)        
        cat("## --------------------------------------------------------\n")
        cat("## print estimated SR parameters\n")
        cat("## --------------------------------------------------------\n")
        purrr::map_dfr(res_SR_MSY_average, function(x) x$pars) %>%
            print()
        cat("## --------------------------------------------------------\n")

    }
    else{
        average_option <- NULL
    }}

    {if(use_regime_model==1){
         regime_shift_option <- list(future_regime=future_regime_MSY)
     }
     else{
         regime_shift_option <- NULL
     }}

    # if given_SR_pars is used
    if(given_SR_pars==1){
        res_SR_MSY$pars[] <- SR_specific_par_MSY
        cat("## --------------------------------------------------------\n")
        cat("## print estimated SR parameters (over-ridden)\n")
        cat("## --------------------------------------------------------\n")
        as_tibble(res_SR_MSY$pars) %>% mutate(AICc   = res_SR_MSY$AICc,
                                              method = res_SR_MSY$input$method,
                                              type   = res_SR_MSY$input$SR) %>%
            print()
        cat("## --------------------------------------------------------\n")
    }

    # L1を使う場合、SDは残差のsdを用いる（fit.SRの返り値のsdはラプラス分布のsd）
    # モデル平均・レジームを考慮する場合・ARありの場合は未対応
    if(use_regime_model==0 && average_SRmodel==0 && given_SR_pars==0 &&
       res_SR_MSY$input$method == "L1" && res_SR_MSY$input$AR == 0){
        resid <- res_SR_MSY$resid[SR_weight==1]
        sd_obs <- sqrt(sum(resid^2)/length(resid))
        res_SR_MSY$pars$sd <- sd_obs # 計算された残差の標準偏差に置き換える(技術ノート, 式(8))
    }

    data_future_msy <- make_future_data(res_vpa = res_vpa_MSY,
                                        nsim = MSY_nsim, # number of simulation
                                        nyear = nyear_MSY, # number of future year
                                        future_initial_year_name = future_MSY_year,
                                        start_F_year_name = future_MSY_year+1,
                                        start_biopar_year_name=future_MSY_year+1,
                                        start_random_rec_year_name = future_MSY_year+1,
                                        # biopar setting
                                        waa_year=NULL, waa=waa_in_MSY,
                                        waa_catch_year=NULL, waa_catch=waa_catch_in_MSY,
                                        maa_year=NULL, maa=maa_in_MSY,
                                        M_year=NULL, M=M_in_MSY,
                                        waa_fun=as.logical(waa_fun_MSY),
                                        start_waafun_year_name=future_MSY_year+1,
                                        # faa setting
                                        faa_year=NULL,
                                        currentF=Fcurrent_MSY,
                                        futureF=Fcurrent_MSY,
                                        # HCR setting (not work when using TMB)
                                        start_ABC_year_name=future_MSY_year+1,
                                        HCR_beta=1,
                                        HCR_Blimit=-1,
                                        HCR_Bban=-1,
                                        HCR_year_lag=0,
                                        # SR setting
                                        res_SR=res_SR_MSY,
                                        seed_number=MSY_seed,
                                        bias_correction=ifelse(bias_correction_MSY==1,TRUE,FALSE),
                                        resid_type=SR_error_MSY,
                                        resample_year_range={if(SR_error_MSY%in%c("resample","backward")) select_resample_year else 0},
                                        backward_duration={if(SR_error_MSY%in%c("backward")) backward_duration_MSY else NULL},
                                        recruit_intercept=recruit_intercept_MSY,
                                        model_average_option=average_option,
                                        regime_shift_option =regime_shift_option,
                                        # others
                                        Pope=is_pope_logical
                                        )


    # F=0からssbがゼロになるまでFを順次大きくしたtraceを実行する
    trace.multi <- unique(sort(c(0.001,seq(from=0,to=2,by=0.1),10,100)))
    trace_pre <- frasyr::trace_future(data_future_msy$data, trace.multi=trace.multi, ncore=ncore)
    B0stat <- trace_pre %>% dplyr::filter(fmulti==0) %>% mutate(RP_name="B0")
    trace.multi2 <- unique(range(trace.multi[trace_pre$ssb.mean>0.001]))

    f_range <- range(trace.multi[which.max(trace_pre$catch.mean)+c(-1,1)])

    optim_method_msy = ifelse(optim_method_msy==0, "R", "tmb")

    # 以降、初期値はそれを参考に決める
    res_future_MSY <- future_vpa(tmb_data = data_future_msy$data,
                                 optim_method=optim_method_msy,
                                 multi_init=mean(f_range),
                                 multi_lower=f_range[1],
                                 multi_upper=f_range[2],
                                 compile=as.logical(compile_tmb))

    MSYstat <- res_future_MSY %>% get.stat(use_new_output=TRUE) %>%
        mutate(RP_name="MSY")

    cat("## print setting for MSY estimation as example (1st year, 1st run parameters)------\n")
    cat("## --------------------------------------------------------\n")
    print(tibble(age                =dimnames(res_future_MSY$naa)$age,
                 currentF           =Fcurrent_MSY,
                 futureF            =Fcurrent_MSY,
                 maturity_init_year =data_future_msy$data$maa_mat[,1,1],
                 bweight_init_year  =data_future_msy$data$waa_mat[,1,1],
                 #             cweight_init_year  =data_future_msy$data$waa.catch_mat[,1,1],
                 natural_mortality  =data_future_msy$data$M[,1,1]))
    cat("## --------------------------------------------------------\n")

    (graph_fcurrent <- plot_Fcurrent(vpares=res_vpa_MSY,Fcurrent=Fcurrent_MSY,
                                    year.range=rev((future_MSY_year)-0:6)))
    ggsave_SH(str_c(output_folder,"/graph_fcurrent.png"), graph_fcurrent,
              family=font_family)

    if(waa_fun_MSY==1){
        png(str_c(output_folder,"/waa_fun_diagnostics.png"))
        par(mfrow=c(data_future_msy$data$nage,2),mar=c(2,2,1,1))
        for(i in 1:data_future_msy$data$nage){
            matplot(log(res_future_MSY$naa[i,,]),
                    log(res_future_MSY$waa[i,,]),
                    col="gray",pch=20,xlab="log(numbers)",ylab="log(weight)")
            matpoints(log(res_vpa_MSY$naa[i,,]),
                      log(res_vpa_MSY$input$dat$waa[i,,]),col="red",pch=20)
            abline(a=mean(data_future_msy$data$waa_par_mat[i,,"b0"]),
                   b=mean(data_future_msy$data$waa_par_mat[i,,"b1"]),col=2)
            title(str_c("Log scale, Age",rownames(res_vpa_MSY$naa)[i]),line=-1)

            matplot(res_future_MSY$naa[i,,],res_future_MSY$waa[i,,],
                    col="gray",pch=20,xlab="numbers",ylab="weight")
            matpoints(res_vpa_MSY$naa[i,,],res_vpa_MSY$input$dat$waa[i,,],
                      col="red",pch=20)
            title(str_c("Normal scale, Age",rownames(res_vpa_MSY$naa)[i]),line=-1)
            if(i==1) legend("topright",pch=c(20,20,NA),lty=c(NA,NA,1),
                   col=c("red","gray","red"),
                   legend=c("predicted data","observed data","regression line"))
        }
        dev.off()
    }

    # 他管理基準値を推定するためのオブジェクトを作っておく
    obj_mat <- NULL
    if(select_PGY[1]>0){

        obj_mat <- bind_rows(obj_mat,
                             tibble(RP_name    = str_c("PGY",select_PGY,"lower",sep="_"),
                                    obj_value  = select_PGY * MSYstat$catch.mean,
                                    multi_init = res_future_MSY$multi*1.2,
                                    multi_lower= res_future_MSY$multi,
                                    multi_upper= 10,
                                    objective="PGY"
                                    ))

        if(only_lowerPGY==2){
            obj_mat2 <- tibble(RP_name    = str_c("PGY",select_PGY,"upper",sep="_"),
                               obj_value  = select_PGY * MSYstat$catch.mean,
                               multi_init = res_future_MSY$multi*0.5,
                               multi_upper= res_future_MSY$multi,
                               multi_lower= 0.001,
                               objective="PGY")
            obj_mat <- bind_rows(obj_mat, obj_mat2)
        }
    }

    if(select_B0[1]>0){
        fssb.range <- trace.multi[trace_pre$ssb.mean>0.1]
        obj_mat <- bind_rows(obj_mat,
                             tibble(RP_name    = str_c("B0-",select_B0*100,"%"),
                                    obj_value  = select_B0 * B0stat$ssb.mean,
                                    multi_init = mean(fssb.range),
                                    multi_upper= max (fssb.range),
                                    multi_lower= 0.001,
                                    objective="SSB"
                                    ))
    }

    if(select_Babs[1]>0){
        fssb.range <- trace.multi[trace_pre$ssb.mean>0.1]
        obj_mat <- bind_rows(obj_mat,
                             tibble(RP_name    = str_c("Ben-",select_Babs,""),
                                    obj_value  = select_Babs,
                                    multi_init = mean(fssb.range),
                                    multi_upper= max (fssb.range),
                                    multi_lower= 0.001,
                                    objective="SSB"
                                    ))
    }

    # obj_matをまとめてmapで回す
    other_RP_stat <- NULL
    if(!is.null(obj_mat)){

        other_RP_stat <-
            purrr::map_dfr(1:nrow(obj_mat),
                           function(x){
                               res <- future_vpa(tmb_data     = data_future_msy$data,
                                                 optim_method = optim_method_msy,
                                                 multi_init   = obj_mat$multi_init[x],
                                                 multi_lower  = obj_mat$multi_lower[x],
                                                 multi_upper  = obj_mat$multi_upper[x],
                                                 compile      = FALSE,
                                                 objective    = obj_mat$objective[x],
                                                 obj_value    = obj_mat$obj_value[x],
                                                 obj_stat     = "mean") %>%
                                   get.stat(use_new_output=TRUE)})

        other_RP_stat <- bind_cols(other_RP_stat, select(obj_mat, RP_name))
        print(bind_cols(obj_mat[,1:2], select(other_RP_stat,catch.mean, ssb.mean)))
    }

    all.stat <- bind_rows(MSYstat, B0stat, other_RP_stat)
    sum.stat <- get_summary_stat(all.stat)

    if(calc_yieldcurve==1){
        # update trace
        trace.multi2 <- c(res_MSY$summary$"Fref/Fcur",trace.multi2)
        trace.multi2 <- trace.multi2[trace.multi2>0] %>%
            purrr::map(function(x) x * c(0.9,0.925,0.95,0.975,1.025,1.05,1.075)) %>%
            unlist() %>% sort() %>% unique()
        diff.trace <- diff(log(trace.multi2))
        trace.multi2[which(mean(diff.trace)<diff.trace)]
        trace.multi2 <- c(trace.multi2,
                          trace.multi2[which(mean(diff.trace)<diff.trace)] +
                          diff.trace[which(mean(diff.trace)<diff.trace)]/2) %>%
            sort()
        trace_pre2 <- trace_future(data_future_msy$data,
                                   trace.multi=trace.multi2, ncore=ncore)
        trace_pre <- bind_rows(trace_pre,trace_pre2)
        trace_pre <- trace_pre[!duplicated(trace_pre$ssb.mean),]
    }

    res_MSY <- lst(all.stat=all.stat, summary=sum.stat$sumvalue,
                   Fvector=sum.stat$Fvector,input=res_future_MSY$input,
                   input_data=data_future_msy$input,
                   trace=trace_pre, res_vpa=res_vpa_MSY, res_SR=res_SR_MSY)

    res_MSY$summary$perSPR <-
        purrr::map_dbl(1:dim(res_MSY$Fvector)[1],
                   function(x)
                       calc_perspr(fout=format_to_old_future(res_future_MSY),
                                   res_vpa=res_vpa_MSY,Fvector=res_MSY$Fvector[x,]))

    options(tibble.width=100)

    # define RP.definition for Btarget
    if(select_Btarget!=0){
        if(select_Btarget<0){
            print(select(res_MSY$summary,-Catch.CV))
            select_Btarget <- readline("Enter row number to be Btarget: ")
            select_Btarget <- as.integer(select_Btarget)
        }
        res_MSY$summary$RP.definition[1] <- NA
        res_MSY$summary$RP.definition[select_Btarget] <- "Btarget0"
    }
    # define RP.definition for Blimit
    if(select_Blimit!=0){
        if(select_Blimit<0){
            print(select(res_MSY$summary,-Catch.CV))
            select_Blimit <- readline("Enter row number to be Blimit: ")
            select_Blimit <- as.integer(select_Blimit)
        }
        res_MSY$summary$RP.definition[which(res_MSY$summary$RP.definition=="Blimit0")] <- NA
        res_MSY$summary$RP.definition[select_Blimit] <- "Blimit0"
    }
    # define RP.definition for Bban
    if(select_Bban!=0){
        if(select_Bban<0){
            print(select(res_MSY$summary,-Catch.CV,-RP.definition))
            select_Bban <- readline("Enter row number to be Bban: ")
            select_Bban <- as.integer(select_Bban)
        }
        res_MSY$summary$RP.definition[which(res_MSY$summary$RP.definition=="Bban0")] <- NA
        res_MSY$summary$RP.definition[select_Bban] <- "Bban0"
    }

    Btarget0 <- derive_RP_value(res_MSY$summary,"Btarget0")$SSB
    Blimit0  <- derive_RP_value(res_MSY$summary,"Blimit0")$SSB
    Bban0    <- derive_RP_value(res_MSY$summary,"Bban0")$SSB
    SPR_MSY0 <- derive_RP_value(res_MSY$summary,"Btarget0")$perSPR
    Fmsy0 <- res_MSY$Fvector %>%
        slice(which(res_MSY$summary$RP.definition=="Btarget0")) %>%
        as.numeric()

    cat("## print estimated RP parameters ------------------------------\n")
    cat("## --------------------------------------------------------\n")
    print(select(res_MSY$summary,-Catch.CV))
    cat("## --------------------------------------------------------\n")
    options(tibble.width=NULL)
    save(data_future_msy,file=str_c(output_folder,"/data_future_msy.rda"))
    save(res_MSY,file=str_c(output_folder,"/res_MSY.rda"))
}

# plot graph
{if(do_MSY_estimation==1){
    refs <- list(Bmsy  = derive_RP_value(res_MSY$summary,"Btarget0")$SSB,
                 Blimit= derive_RP_value(res_MSY$summary,"Blimit0" )$SSB,
                 Bban  = derive_RP_value(res_MSY$summary,"Bban0"   )$SSB)
}
else{
    refs <- list(Bmsy  = 0,
                 Blimit= 0,
                 Bban  = 0)
    }}

if(average_SRmodel==0){
    if(use_regime_model==0){
        (graph_MSY$SRplot <- SRplot_gg(res_SR_MSY,
                                xscale=1000,xlabel="千トン",
                                yscale=1000,ylabel="千尾",
                                labeling.year=SRplot_label_year,
                                ## 90%信頼区間を表示していることに注意！
                                refs=NULL,plot_CI=TRUE,CI=0.9,
                                add.info=TRUE) + theme_SH())
    }
    if(use_regime_model==1){

        (graph_MSY$SRplot <- qplot(data_SR$SSB,data_SR$R,xlab="SSB",ylab="R",
                            color=factor(res_SR_MSY$regime_resid$regime),
                            xlim=c(0,max(data_SR$SSB)),ylim=c(0,max(data_SR$R))) +
             geom_line(data=res_SR_MSY$pred, mapping=aes(x=SSB,y=R,color=Regime)) +
             labs(caption=str_c("関数形: ",res_SR_MSY$input$SR,", 自己相関: ", 0 ,
                                ", 最適化法:",res_SR_MSY$input$method,", AICc: ",
                                round(res_SR_MSY$AICc,2))) +
             theme_SH()+theme(legend.position="top"))

    }
}

if(average_SRmodel==1){
    graph_MSY$SRplot <- compare_SRfit(res_SR_MSY_average)+ggtitle("SR functions averaged")
}

ggsave_SH(str_c(output_folder,"/graph_SRplot.png"), graph_MSY$SRplot, family=font_family)

## kobe chart
if(do_MSY_estimation==1){
#-- kobe plot
# SPR.msyを目標としたとき、それぞれのF at age by yearを何倍すればSPR.msyを達成できるか計算
SPR.history <- get.SPR(res_vpa_MSY,
                       target.SPR=SPR_MSY0*100,
                       max.age=Inf,Fmax=8)$ysdata
kobe.ratio <- tibble(year=colnames(res_vpa_MSY$ssb),
                     Fratio=SPR.history$"F/Ftarget",
                     Bratio=colSums(res_vpa_MSY$ssb, na.rm=T)/
                         derive_RP_value(res_MSY$summary,"Btarget0")$SSB) %>%
    dplyr::filter(!is.na(Bratio))

cat("## --------------------------------------------------------\n")
cat("## Historical F/Fmsy & B/Bmsy values ------------\n")
cat("## --------------------------------------------------------\n")
kobe.ratio %>% print()
cat("## --------------------------------------------------------\n")

if(put_label_kobe==0) label_name_kobe <- c("","","")
if(ylabel_kobe==1) ylabel_kobe <- "F"
if(ylabel_kobe==2) ylabel_kobe <- "U"
if(ylabel_kobe==3) ylabel_kobe <- c("F","U")

    kobe_setting <- expand.grid(beta=beta_kobe,ylabel=ylabel_kobe) %>%
        as_tibble() %>%
        mutate(graph=rep(list(0),nrow(.)))
    names(kobe_setting$graph) <- str_c("kobe_",1:nrow(kobe_setting))

for(kk in 1:nrow(kobe_setting)){
    kobe_setting$graph[[kk]] <- plot_kobe_gg(res_vpa_MSY,
                           refs_base=res_MSY$summary,
                           roll_mean=1,category=4,
                           Btarget="Btarget0",
                           Blow="Btarget0",
                           beta =kobe_setting$beta[kk],
                           refs.color=rep("black",3),
                           yscale=1.2,
                           HCR.label.position=c(1,1),
                           RP.label=label_name_kobe,
                           ylab.type=kobe_setting$ylabel[kk],
                           Fratio=kobe.ratio$Fratio,
                           plot.year=ifelse(plot_year_kobe==0, "all", plot_year_kobe))
    ggsave_SH(str_c(output_folder,"/graph_kobe",kk,".png"),
              kobe_setting$graph[[kk]], family=font_family)
}
    graph_MSY <- c(graph_MSY, kobe_setting$graph)
}



###########################################################3
###########################################################3
#-------------- HCRをもとにした将来予測パート
###########################################################3
###########################################################3

# setting future F pre ABC year
if(select_FAA_preABC%in%c(1,3,4,5)){
    if(select_FAA_preABC==1){
        FAA_preABC <- Fmsy0
    }
    if(select_FAA_preABC==3){
        FAA_preABC <- apply_year_colum(res_vpa_MSY$faa,target_year=FAA_preABC_year)
    }
    if(select_FAA_preABC==4){
        FAA_preABC <- convert_faa_perSPR(res_vpa_MSY,sel_year=Fsel_preABC_year,faa_year=FAA_preABC_year)
    }
    if(select_FAA_preABC==5){
        FAA_preABC <- convert_faa_perSPR(res_vpa_MSY,sel_year=Fsel_preABC_year,faa_year=NULL,
                                         Fcurrent_MSY=Fmsy0)
    }
    cat("Set future F vector before ABC.year as: ")
    FAA_preABC  %>% as.numeric() %>% round(2) %>% print()
}
if(select_FAA_preABC>5) stop("select_FAA_preABC should be 1-5")

# setting future F after ABC year
if(select_FAA_afterABC%in%c(1,2,4,5,6)){
    if(select_FAA_afterABC==1){
        FAA_afterABC <- Fmsy0
    }
    if(select_FAA_afterABC==2){
        FAA_afterABC <- FAA_preABC
    }
    if(select_FAA_afterABC==4){
        FAA_afterABC <- apply_year_colum(res_vpa_MSY$faa,target_year=FAA_afterABC_year)
    }
    if(select_FAA_afterABC==5){
        FAA_afterABC <- convert_faa_perSPR(res_vpa_MSY,sel_year=Fsel_afterABC_year,faa_year=FAA_afterABC_year)
    }
    if(select_FAA_afterABC==6){
        FAA_afterABC <- convert_faa_perSPR(res_vpa_MSY,sel_year=Fsel_afterABC_year,faa_year=NULL,
                                         Fcurrent_MSY=Fmsy0)
    }
    cat("Set future F vector after ABC.year as: ")
    FAA_afterABC  %>% round(2) %>% print()
}

# setting HCR
HCR.future <- list(Blim    = Blimit0,
                   Bban    = Bban0,
                   beta    = beta_default,
                   year.lag= HCR_year_lag)

# setting SR function for future
res_SR_future <- res_SR_MSY
if(set_future_rho==1) res_SR_future$pars$rho <- future_rho

if(set_specific_future_recruit==0){
    bias_correction_future <- bias_correction_MSY
    SR_error_future        <- SR_error_MSY
    if(SR_error_future=="resample"|SR_error_future=="backward") select_resample_year_future <- select_resample_year
    if(SR_error_future=="backward") backward_duration_future <- backward_duration_MSY
}

# make future data
data_future_0.8HCR <- make_future_data(res_vpa = res_vpa_MSY,
                                    nsim = future_nsim, # number of simulation
                                    nyear = future_nyear, # number of future year
                                    future_initial_year_name = future_start_year-1,
                                    start_F_year_name = future_start_year,
                                    start_biopar_year_name=future_start_year,
                                    start_random_rec_year_name = future_start_year,
                                    # biopar setting
                                    waa_year=NULL, waa=waa_in_MSY,
                                    waa_catch_year=NULL, waa_catch=waa_catch_in_MSY,
                                    maa_year=NULL, maa=maa_in_MSY,
                                    M_year=NULL, M=M_in_MSY,
                                    waa_fun=as.logical(waa_fun_future),
                                    start_waafun_year_name=ifelse(as.logical(waa_fun_future),
                                                                  start_waafun_year_name_future,
                                                                  NA),
                                    # faa setting
                                    faa_year=NULL,
                                    currentF=FAA_preABC,
                                    futureF=FAA_afterABC,
                                    # HCR setting (not work when using TMB)
                                    start_ABC_year_name=future_ABC_year,
                                    HCR_beta=beta_default,
                                    HCR_Blimit=Blimit0,
                                    HCR_Bban=Bban0,
                                    HCR_year_lag=HCR_year_lag,
                                    # Pope setting
                                    Pope=is_pope_logical,
                                    # SR setting
                                    res_SR=res_SR_future,                       
                                    seed_number=MSY_seed,
                                    bias_correction=ifelse(bias_correction_future==1,TRUE,FALSE),
                                    resid_type=SR_error_future,
                                    resample_year_range={if(SR_error_future%in%c("resample","backward")) select_resample_year_future else 0},
                                    backward_duration={if(SR_error_future%in%c("backward")) backward_duration_future else NULL},                                    
                                    recruit_intercept=recruit_intercept_future,
                                    model_average_option=average_option,
                                    regime_shift_option =regime_shift_option,
                                    fix_recruit=if(select_specific_recruit==0) NULL else list(year=recruit_year, rec=recruit_number),
                                    fix_wcatch=if(select_specific_catch==0) NULL else list(year=catch_year, wcatch=catch_weight)
                                    )

# set specific biological parameters
if(set_specific_biopara==1){
    if(!is.null(specific_waa_future)){
        waa_in_MSY <- data_future_0.8HCR$data$waa_mat
        waa_in_MSY[,colnames(specific_waa_future),] <- as.matrix(specific_waa_future)
        data_future_0.8HCR$input$waa <- waa_in_MSY
    }
    if(!is.null(specific_waa.catch_future)){
        waa_catch_in_MSY <- data_future_0.8HCR$data$waa_catch_mat
        waa_catch_in_MSY[,colnames(specific_waa.catch_future),] <- as.matrix(specific_waa_catch_future)
        data_future_0.8HCR$input$waa_catch <- waa_catch_in_MSY
    }
    if(!is.null(specific_maa_future)){
        maa_in_MSY <- data_future_0.8HCR$data$maa_mat
        maa_in_MSY[,colnames(specific_maa_future),] <- as.matrix(specific_maa_future)
        data_future_0.8HCR$input$maa <- maa_in_MSY
    }
    if(!is.null(specific_M_future)){
        M_in_MSY <- data_future_0.8HCR$data$M_mat
        M_in_MSY[,colnames(specific_M_future),] <- as.matrix(specific_M_future)
        data_future_0.8HCR$input$M <- M_in_MSY
    }
    data_future_0.8HCR <- do.call(make_future_data, data_future_0.8HCR$input)
}

res_future_0.8HCR <- future_vpa(tmb_data = data_future_0.8HCR$data,
                                optim_method="none",
                                multi_init=1,
                                multi_lower=1,
                                multi_upper=1,
                                SPRtarget=derive_RP_value(res_MSY$summary,"Btarget0")$perSPR*100,
                                compile=FALSE)

# current F run
data_future_current <- data_future_0.8HCR

data_future_current$data$faa_mat[,as.character(future_start_year:max(dimnames(data_future_current$data$faa_mat)[[2]])),] <- data_future_current$data$faa_mat[,as.character(future_ABC_year-1),1]
data_future_current$data$HCR_mat[,,c("Blimit","Bban")] <- -1
data_future_current$data$HCR_mat[,,c("beta")] <- 1

res_future_current <- future_vpa(tmb_data = data_future_current$data,
                                 optim_method="none",
                                 multi_init=1,
                                 multi_lower=1,
                                 multi_upper=1,
                                 SPRtarget=derive_RP_value(res_MSY$summary,"Btarget0")$perSPR*100,
                                 compile=FALSE)

# apply(res_future_0.8HCR$wcaa,c(2,3),sum)["2020",] %>% mean()
# apply(res_future_0.8HCR$wcaa,c(2,3),sum)["2021",] %>% mean()


# kobe II table
# 80%信頼区間を資源評価票の書式にのっとって出せるようにしておく
input_future_0.8HCR <- res_future_0.8HCR$input
input_future_0.8HCR$SPRtarget <- NULL
kobeII.data <- beta.simulation(input_future_0.8HCR,
                               beta_vector=beta_table,
                               year.lag=HCR_year_lag,
                               type="new")

tmp <- convert_future_table(format_to_old_future(res_future_current), label="Fcurrent") %>%
    mutate(HCR_name=label) %>% select(-label)
kobeII.data <- kobeII.data %>%
    mutate(HCR_name=as.character(HCR_name))
kobeII.data <- bind_rows(kobeII.data,tmp)

kobeII.table <- make_kobeII_table(kobeII.data,
                                  res_vpa        = res_vpa_MSY,
                                  year.catch     = year_catch_average,
                                  year.ssb       = year_ssb_average,
                                  year.Fsakugen  = year_Fsakugen_mean,
                                  year.ssbtarget = year_ssbtarget_prob,
                                  year.ssblimit  = year_ssblimit_prob,
                                  year.ssbban    = year_ssbban_prob,
                                  year.ssbmin    = year_ssbmin_prob,
                                  year.ssbmax    = year_ssbmax_prob,
                                  year.aav       = year_catch_aav,
                                  Btarget=Btarget0,
                                  Blimit =Blimit0,
                                  Bban   =Bban0
                                  )

cat("## --------------------------------------------------------\n")
cat("## print setting for future simulations (with current F) \n")
cat("## --------------------------------------------------------\n")
last_year <- dim(res_future_current$naa)[[2]]
res_future_current.old <- format_to_old_future(res_future_current)
print(tibble(age                = dimnames(res_future_current.old$naa)$age,
             currentF           = res_future_current.old$currentF,
             futureF            = res_future_current.old$futureF,
             F_init_year        = res_future_current.old$faa[,1,1],
             F_last_year        = res_future_current.old$faa[,last_year,1],
             maturity_init_year = res_future_current.old$maa[,1,1],
             maturity_last_year = res_future_current.old$maa[,last_year,1],
             bweight_init_year  = res_future_current.old$waa[,1,1],
             bweight_last_year  = res_future_current.old$waa[,last_year,1],
             cweight_init_year  = res_future_current.old$waa.catch[,1,1],
             cweight_last_year  = res_future_current.old$waa.catch[,last_year,1],
             natural_init_mortality = res_future_current.old$M[,1,1],
             natural_last_mortality = res_future_current.old$M[,last_year,1]))

cat("## --------------------------------------------------------\n")

cat("## --------------------------------------------------------\n")
cat("## print setting for future simulations (with HCR) \n")
cat("## --------------------------------------------------------\n")
res_future_0.8HCR.old <- format_to_old_future(res_future_0.8HCR)
print(tibble(age                =dimnames(res_future_0.8HCR.old$naa)$age,
             currentF           =res_future_0.8HCR.old$currentF,
             futureF            =res_future_0.8HCR.old$futureF,
             maturity_init_year =res_future_0.8HCR.old$maa[,1,1],
             bweight_init_year  =res_future_0.8HCR.old$waa[,1,1],
             cweight_init_year  =res_future_0.8HCR.old$waa.catch[,1,1],
             natural_mortality  =res_future_0.8HCR.old$M[,1,1]))
cat("## --------------------------------------------------------\n")

cat("## print results of future probabilities under given HCR ------------\n")
cat("## --------------------------------------------------------\n")
print(kobeII.table)
cat("## --------------------------------------------------------\n")

# save results
cat("\n***** Summary results *****\n")
save(data_future_0.8HCR, file=str_c(output_folder,"/data_future_0.8HCR.rda"))
save(data_future_current,file=str_c(output_folder,"/data_future_current.rda"))
save(res_future_0.8HCR,  file=str_c(output_folder,"/res_future_0.8HCR.rda"))
save(res_future_current, file=str_c(output_folder,"/res_future_current.rda"))
save(kobeII.table,file=str_c(output_folder,"/kobeII.table.rda"))

## yield curve & kobe chart
if(do_MSY_estimation==1){

    refs.plot <- dplyr::filter(res_MSY$summary,RP.definition%in%c("Btarget0","Blimit0","Bban0"))

    (graph_MSY$yield_curve_detail <- plot_yield(res_MSY$trace,
                                  refs.plot,
                                  refs.label=label_name_kobe,
                                  future=list(res_future_0.8HCR),
                                  past=res_vpa_MSY,labeling=FALSE,
                                  refs.color=rep("black",3),
                                  biomass.unit=1000,
                                  AR_select=FALSE,
                                  past_year_range=past_year_range_yieldcurve,
                                  xlim.scale=0.7,ylim.scale=1.1
                                  ) + theme_SH())

    (graph_MSY$yield_curve_simple <- plot_yield(res_MSY$trace,
                                  refs.plot,
                                  refs.label=label_name_kobe,
                                  future=NULL,
                                  past=NULL,labeling=FALSE,
                                  refs.color=rep("black",3),
                                  biomass.unit=1000,
                                  AR_select=FALSE,
                                  xlim.scale=0.7,ylim.scale=1.1
                                  ) + theme_SH())

    ggsave_SH(str_c(output_folder,"/graph_yield_curve_detail.png"),
              graph_MSY$yield_curve_detail, family=font_family)
    ggsave_SH(str_c(output_folder,"/graph_yield_curve_simple.png"),
              graph_MSY$yield_curve_simple, family=font_family)
}


# plot future projection
(graph_MSY$future <- plot_futures(res_vpa_MSY, #vpaの結果
                   list(res_future_0.8HCR,res_future_current), # 将来予測結果
                   future.name=c(str_c(beta_default,"HCR"),"Fcurrent"),
                   ## 90%信頼区間を表示していることに注意！
                   CI_range=c(0.05,0.95),
                   maxyear=2045,
                   ncol=2, # 図の出力の列数。3行x1列ならncol=1
                   what.plot=c("Recruitment","biomass","SSB","catch","U","Fratio"),
                   Btarget=derive_RP_value(res_MSY$summary,"Btarget0")$SSB,
                   Blimit=derive_RP_value(res_MSY$summary,"Blimit0")$SSB,
                   Bban=derive_RP_value(res_MSY$summary,"Bban0")$SSB,
                   Umsy=derive_RP_value(res_MSY$summary,"Btarget0")$U*100,
                   SPRtarget=derive_RP_value(res_MSY$summary,"Btarget0")$perSPR*100,
                   RP_name=label_name_kobe,
                   biomass.unit=biomass.unit_MSY,  # バイオマスの単位(100, 1000, or 10000トン)
                   number.unit=number.unit_MSY,  #尾数の単位
                   number.name=number.name_MSY,  #尾数の名前
                   n_example=5,seed=2,
                   example_width=0.3)+ # どのシミュレーションをピックアップするかはseedの値を変えて調整してください
    scale_color_hue(labels=c(VPA="過去の推定値",s1="現状の漁獲圧",s2="漁獲管理規則案\n(β=0.8)"))+
    scale_fill_hue(labels=c(VPA="過去の推定値",s1="現状の漁獲圧",s2="漁獲管理規則案\n(β=0.8)"))+
    scale_linetype_discrete(guide=FALSE)
)

ABC_ = graph_MSY$future$data %>% filter(stat=="catch" & year==2020 & scenario!="Fcurrent")
ABC = c(ABC,as.numeric(ABC_$mean))

# if (0) {

#gridExtra::grid.arrange(g1_SRplot,g2_yield_curve,g3_kobe, g4_future)

ggsave(str_c(output_folder,"/graph_future.png"),
       graph_MSY$future,
       width=200*1.1,height=250*1.1,dpi=600,units="mm",
       family=font_family)

table_2020_average <- kobeII.data %>%
    dplyr::filter(year==2020) %>%
    group_by(HCR_name, beta, stat) %>%
    summarise(average=mean(value)) %>%
    spread(key=stat, value=average)

table_2030_ssb <- kobeII.data %>%
    dplyr::filter(year==2030, stat=="SSB") %>%
    group_by(HCR_name, beta) %>%
    summarise(average=mean(value),
              ci10              =quantile(value,probs=0.1),
              ci90=quantile(value,probs=0.9),
              Prob_over_target =mean(value>Btarget0)*100,
              Prob_over_limit    =mean(value>Btarget0)*100,
              Prob_over_ban     =mean(value>Btarget0)*100)

get_first_year <- function(prob.mat){
    prob.mat <- prob.mat %>% select(-HCR_name, -stat_name, -beta) >50
    year.range <- as.numeric(colnames(prob.mat))
    apply(prob.mat, 1, function(x) min(year.range[x==TRUE],na.rm=T))
}

table_over_first_year <- purrr::map_dfc(kobeII.table[c("prob.over.ssbtarget","prob.over.ssblimit","prob.over.ssbban")],
                                        get_first_year) %>%
    mutate(HCR=kobeII.table$prob.over.ssbtarget$HCR_name) %>%
    select(HCR, prob.over.ssbtarget, prob.over.ssblimit, prob.over.ssbban)

kobeII.table$table_2020_average <- table_2020_average
kobeII.table$table_2030_ssb         <- table_2030_ssb
kobeII.table$table_over_first_year<- table_over_first_year

out.vpa(res=res_vpa_MSY,
        srres=res_SR_MSY,
        msyres=(if(do_MSY_estimation==1) res_MSY else NULL),
        fres_current=res_future_current,
        fres_HCR=res_future_0.8HCR,
        kobeII=kobeII.table,filename=NULL,
        csvname=str_c(output_folder,"/allresults.csv"),
        pdfname=str_c(output_folder,"/simple_plot.pdf"))

write("\n# Kobe ratio",file=str_c(output_folder,"/allresults.csv"),append=T)
kobe.ratio %>%
    write_csv(path=str_c(output_folder,"/allresults.csv"),append=T, col_names=TRUE)

# write final table
final_stat <- list(SBmsy = Btarget0,
                   MSY   = derive_RP_value(res_MSY$summary,"Btarget0")$Catch,
                   Fmsy  = Fmsy0,
                   Umsy  = derive_RP_value(res_MSY$summary,"Btarget0")$U,
                   per_SPR_MSY_biolpar_eq = derive_RP_value(res_MSY$summary,"Btarget0")$perSPR,
                   SBlimit = Blimit0,
                   SBban   = Bban0,
                   #per_SPR_Fcurrent_biolpar_1styear_future =
                    #   calc_perspr(fout=res_future_current,res_vpa=res_vpa_MSY,
                    #              Fvector=FAA_preABC, target.col=1),
                   "FBratio_last_year"=tail(kobe.ratio,n=1),
                   SR_par_for_future=apply(data_future_0.8HCR$data$SR_mat[,,c("a","b","rho")],3,mean),
                   table_2020_average=table_2020_average,
                   table_2030_ssb=table_2030_ssb,
                   table_over_first_year=table_over_first_year
                   )

cat("## Summary statistics ------------\n")
cat("## --------------------------------------------------------\n")
print(final_stat)
cat("## --------------------------------------------------------\n")

cat("## Summary graphs ------------\n")

res_vpa_tibble <- convert_vpa_tibble(res_vpa_MSY)

(graph_MSY$ssb_history <- res_vpa_tibble %>%
     dplyr::filter(stat=="SSB") %>%
     ggplot() +
     geom_path(aes(x=year,y=value/1000),color=rgb(73,99,173,maxColorValue=255),lwd=1)+
     xlab("漁期年")+ylab("千トン")+
     #coord_cartesian(ylim=c(0,1.05*max(value/1000)),expand=0)+
     #     scale_y_continuous(expand=expand_scale(mult=c(0,0.05)))+
     theme_SH(base_size=11)+theme(legend.position="top")+
     geom_hline(yintercept=Btarget0/1000,col="#00533E",lty=2,lwd=1)+
     geom_hline(yintercept=Blimit0/1000, col="#edb918",lty=2,lwd=1)+
     geom_hline(yintercept=Bban0/1000,   col="#C73C2E",lty=2,lwd=1)+
     geom_hline(yintercept=0,            col="black")
)

(graph_MSY$catch_history <- res_vpa_tibble %>%
     dplyr::filter(stat=="catch") %>%
     ggplot() +
     geom_path(aes(x=year,y=value/1000),color=rgb(126,171,117,maxColorValue=255),lwd=1)+
     xlab("漁期年")+ylab("千トン")+
     scale_y_continuous(expand=expand_scale(mult=c(0,0.05)))+
     theme_SH(base_size=11)+theme(legend.position="top")+
     geom_hline(yintercept=0,            col="black"))

(graph_MSY$Fratio_history <- kobe.ratio %>%
     ggplot() +
     geom_path(aes(x=as.numeric(year),y=Fratio),color=rgb(122,78,155,maxColorValue=255),lwd=1)+
     xlab("漁期年")+ylab("漁獲圧の比(F/Fmsy)")+
     scale_y_continuous(expand=expand_scale(mult=c(0,0.05)))+
     theme_SH(base_size=11)+theme(legend.position="top")+
     geom_hline(yintercept=1, col="black",lty=2,lwd=1)+
     geom_hline(yintercept=0, col="black"))

(graph_MSY$HCR <- plot_HCR(Btarget0,Blimit0,Bban0,Ftarget=1,
                           biomass.unit=biomass.unit_MSY,beta=beta_default,
                           Fcurrent=-1, #1/derive_RP_value(res_MSY$summary,"Btarget0")$"Fref2Fcurrent",
                           RP.label=label_name_kobe) + ylim(0,1.3))

(graph_MSY$HCR_catch <- plot_HCR_by_catch(trace=res_MSY$trace,
                           fout0.8=res_future_0.8HCR,
                           SBtarget=Btarget0,SBlim=Blimit0,SBban=Bban0,
                           Fmsy_vector=Fmsy0,
                           MSY=derive_RP_value(res_MSY$summary,"Btarget0")$Catch,
                           M_vector=as.numeric(M_in_MSY),
                           biomass.unit=biomass.unit_MSY,beta=beta_default,
                           Pope=is_pope_logical,
                           RP.label=label_name_kobe))

inch <- 2.54
ggsave(str_c(output_folder,"/graph_ssb_history.png"  ), graph_MSY$ssb_history,   family=font_family,
       height=6/inch,width=9/inch)
ggsave(str_c(output_folder,"/graph_catch_history.png"), graph_MSY$catch_history, family=font_family,
       height=6/inch,width=9/inch)
ggsave(str_c(output_folder,"/graph_Fratio_histry.png"), graph_MSY$Fratio_history,family=font_family,
       height=6/inch,width=9/inch)

ggsave(str_c(output_folder,"/graph_HCR.png"), graph_MSY$HCR,family=font_family,
       height=8/inch,width=15/inch)
ggsave(str_c(output_folder,"/graph_HCR_catch.png"), graph_MSY$HCR_catch,family=font_family,
       height=8/inch,width=15/inch)

cat("## --------------------------------------------------------\n")

do.call(gridExtra::grid.arrange,graph_MSY)
save(graph_MSY,file=str_c(output_folder,"/graph_all.rda"))

# save results
if(average_SRmodel==1) res_SR_MSY <- res_SR_MSY_average
save(res_SR_MSY,file=str_c(output_folder,"/res_SR.rda"))
save(res_SR_future,file=str_c(output_folder,"/res_SR_future.rda"))

options(warn=old.warning)
options(tibble.width=NULL,tibble.print_max=20)

cat("## End of calculation (probably successfully finished)------------\n")

# }