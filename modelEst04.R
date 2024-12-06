# model estimation (2)
# simplified model
#####
rm(list=ls())
gc();gc();

library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(ggplot2)
library(glmnet)
library(furrr)
library(momentfit)
# library(foreach)
# library(doSNOW)

# E:/WorkDir01/prog/R/2024/2024_E-STAT_API/dataPrep01.R
load("data/SynD3.xdr") # SynD3
SynD3$MUN <- as.character(SynD3$MUN)

for (ii in 1:length(SynD3$MUN)){ 
  if (nchar(SynD3$MUN[[ii]]) == 4) {
    SynD3$MUN[[ii]] <- paste0("0", SynD3$MUN[[ii]])
  }
}
save(SynD3,file="data/SynD3.for.esrimap.xdr")
indv=SynD3$code %>% unique() %>% sort()


LS02.L=list(rep(NA,length(indv)))
em.kk=list(rep(NA,length(indv)))
system.time({ # 210.97 sec
  for(ii in 1:length(indv)){ # ii=4
    cat("ii=",ii,"\n")
    tSynD=SynD3 %>% filter(code==indv[ii]) %>% 
      mutate(mi.11=sl.11-va.11,mi.15=sl.15-va.15, kk11=k1.11+k2.11, kk15=k1.15+k2.15)
    tSynD[sapply(tSynD,function(x)return(x<0))] <- NA
    tSynD$BE.15[tSynD$BE.15 < 1] <- NA
    tSynD$em.15[tSynD$em.15 < 1] <- NA
    tSynD=na.omit(tSynD)
    
    mean_value11=mean(tSynD$BE.11,na.rm = TRUE)
    mean_value15=mean(tSynD$BE.15,na.rm = TRUE)
    
    sd_value11=sd(tSynD$BE.11,na.rm = TRUE)
    sd_value15=sd(tSynD$BE.15,na.rm = TRUE)
    
    tSynD$BE.11[abs(tSynD$BE.11-mean_value11) > 4*sd_value11] <- NA
    tSynD$BE.15[abs(tSynD$BE.15-mean_value15) > 4*sd_value15] <- NA
    
    tSynD=na.omit(tSynD)
    
    tSynD11=dplyr::select(tSynD,contains("11"))
    tSynD15=dplyr::select(tSynD,contains("15"))
    
    colnames(tSynD11)=c("BE","va","sl","em","k1","k2","a.bd","a.fc","te","acc","mi","kk")
    colnames(tSynD15)=c("BE","va","sl","em","k1","k2","a.bd","a.fc","te","acc","mi","kk")
    
    tSynD11m=log(tSynD11+1)
    tSynD15m=log(tSynD15+1)
    
    # OLS
    lmm.11=lm("va ~ em + kk", data = tSynD11m) # without accessibility (productivity)
    lmm.15=lm("va ~ em + kk", data = tSynD15m) # with accessibility (effect on productivity)
    
    # ACF approach
    ### 1st stage ###
    system.time({ # 1.39  
      df_fit_1st_poly01 <- tibble(q= 1:10) %>%
        mutate(model = map(q, function(q){
          if(q == 1){
            XX=model.matrix(va ~ 1 + em + kk, data = tSynD11m)
          } else{
            poly_term <-  paste0("va ~ em  + kk + poly(em,kk,mi, degree =", q, ")");
            XX=model.matrix(as.formula(poly_term), data = tSynD11m)
          }
          yy=tSynD11m$va;
          cv.glmnet(XX, yy,alpha=0,weights=tSynD11m$BE); # Ridge regression, cross validation(回帰式の見直し)
        })) %>%
        mutate(
          rmse = map_dbl(model, function(x) x$cvm[which(x$lambda==x[["lambda.1se"]])] %>% sqrt()), # cvm: The mean cross-validated error
          # https://stackoverflow.com/questions/63171921/is-there-a-way-in-r-to-determine-aic-from-cv-glmnet
          #誤差
          aic = map_dbl(model, function(x) {
            whlm=which(x$lambda==x[["lambda.1se"]]);
            tLL=x$glmnet.fit$nulldev*x$glmnet.fit$dev.ratio[whlm]/2;
            k=x$glmnet.fit$df[whlm];# n=x$glmnet.fit$nobs;
            return(-2*tLL+2*k)} ),
          # coef = map(model, function(x) enframe(coef(x)) %>% filter(name %in% c("em.15", "k1.15","k2.15","a.bd15","a.fc15","acc15")))
          coef = map(model, function(x) coef(x) %>% .[rownames(.) %in% c("em", "kk"),] %>% t())
        ) %>% unnest(coef) # %>% pivot_wider(names_from = name, values_from = value)
    })
    
    ### 2nd stage ###
    tI01=which.min(df_fit_1st_poly01$aic)
    qq01=df_fit_1st_poly01$q[tI01] # order
    fit_1st_poly01 <- df_fit_1st_poly01$model[[tI01]] # polynominal model
    
    # coef(df_fit_1st_poly01$model[[qq01]])
    
    
    # coeff <- coef(fit_1st_poly01) # %>% intersect(c("em.15","k1.15","k2.15","a.bd15","a.fc15"))
    # vars=row.names(coeff) %>% intersect(c("em","kk"))
    # poly_term02 <-  paste0("va.15 ~ ",paste(vars, collapse = '+'),"+ poly(em.15,k1.15,k2.15,mi.15, degree =", qq02, ")");
    
    # tSynD11m2=tSynD11m
    # colnames(tSynD11m2)=colnames(tSynD15m)
    
    # full parameter model
    if(qq01 == 1){
      # XX=model.matrix(va ~ 1 + em + kk, data = tSynD11m)
      XX11=model.matrix(va ~ 1 + em + kk, data = tSynD11m)
      XX15=model.matrix(va ~ 1 + em + kk, data = tSynD15m)
    } else{
      poly_term01 <-  paste0("va ~ em  + kk + poly(em,kk,mi, degree =", qq01, ")");
      # poly_term <-  paste0("va ~ em  + kk + poly(em,kk,mi, degree =", q, ")");
      # XX=model.matrix(as.formula(poly_term), data = tSynD11m)
      XX11=model.matrix(as.formula(poly_term01), data = tSynD11m)
      XX15=model.matrix(as.formula(poly_term01), data = tSynD15m)
    }
    # XX11=model.matrix(as.formula(poly_term01), data = tSynD11m)
    # XX15=model.matrix(as.formula(poly_term01), data = tSynD15m)
    phi15=predict(fit_1st_poly01, newx = XX15, s = "lambda.1se")
    phi11=predict(fit_1st_poly01, newx = XX11, s = "lambda.1se")
    
    # plot(phi11,phi15)
    # abline(0,1,col="red")
    
    b_v01=coef(fit_1st_poly01) %>% .[rownames(.) %in% c("(Intercept)","em","kk"),]
    b_v01=b_v01[-2]
    
    # GMM: momentfit package
    # ACF original approach
    mmf01=function(theta, xx){ # theta=theta0
      tb_v=theta
      
      bpomega15=phi15-(tSynD15m[names(tb_v)] %>% as.matrix())%*%tb_v # beta0+omega
      bpomega11=phi11-(tSynD11m[names(tb_v)] %>% as.matrix())%*%tb_v # beta0+omega
      # plot(omega11,omega15)
      
      bpomega.lm=lm(bpomega15~bpomega11+0) #  coef=rho
      bpomega15.p=bpomega.lm$fitted.values %>% as.vector() # rho*(beta0+omega)@(t-1)
      
      # ee=tSynD15m$va-omega15.p-(tSynD15m[names(theta)] %>% as.matrix())%*%theta
      ee=bpomega15-bpomega15.p
      # mean(ee)
      # return(ee*tSynD15m[c("em.15","k1.15","k2.15")])
      return(ee*tSynD15m[names(tb_v)])
    }
    
    model_gmm_01 <-
      momentModel(g = mmf01,
                  theta0 = b_v01[-1])
    fit_gmm_01 <- gmmFit(model_gmm_01)
    ss= summary(fit_gmm_01)
    # ?momentModel
    
    LS02.L[[ii]]=list(lmm.11=lmm.11,
                      lmm.15=lmm.15,
                      df_fit_1st_poly01=df_fit_1st_poly01,
                      fit_1st_poly01=fit_1st_poly01,
                      model_gmm_01=model_gmm_01,
                      fit_gmm_01=fit_gmm_01,
                      ss=ss)
    
    em.kk[[ii]] = fit_gmm_01@theta
  }
})
# file.exists("data/LS02.L.xdr")
#save(LS02.L,file="data/LS03.L.xdr")
#save(em.kk,file="data/em.kk.xdr")
load("data/LS03.L.xdr") # LS02.L
load("data/em.kk.xdr")
lapply(LS02.L,function(x)return(summary(x$lmm.15)))
matrix_em.kk <- do.call(rbind,em.kk )

lapply(LS02.L,function(x)return(x$ss@coef))
lapply(LS02.L,function(x)return(x$ss@coef[,"Pr(>|t|)"]))
# LS02.L[[1]]$ss@coef

#lapply(LS02.L,function(x)return(summary(x$fit_gmm_01)))

#lapply(LS02.L,function(x)return(summary(x$fit_gmm_01)@coef))
#lapply(LS02.L,function(x){model_gmm_01=x$model_gmm_01;return(summary(x$fit_gmm_01))})
#線形回帰T検定
lmm_result <- list()
for(ii in 1:length(indv)){
  lmm_summary <- lapply(LS02.L,function(x)return(summary(x$lmm.15)))
  lmm_result[[ii]] <- lmm_summary[[ii]][["coefficients"]]
}
lmm_result <- do.call(rbind,lmm_result)
#GMM推計T検定
gmm_result <- list() 
gmm_result <- lapply(LS02.L,function(x)return(x$ss@coef))
gmm_result <-  do.call(rbind,gmm_result)

#従業者数、付加価値額の集計
EM_list <- list()
VA_list <- list()
for(ii in 1:length(indv)){#ii=1
  tSynD=SynD3 %>% filter(code==indv[ii]) %>% 
    mutate(mi.11=sl.11-va.11,mi.15=sl.15-va.15, kk11=k1.11+k2.11, kk15=k1.15+k2.15)
  tSynD[sapply(tSynD,function(x)return(x<0))] <- NA
  tSynD=na.omit(tSynD)
  
  #tSynD11=dplyr::select(tSynD,contains("11"))
  tSynD15=dplyr::select(tSynD,contains("15"))
  
  #colnames(tSynD11)=c("BE","va","sl","em","k1","k2","a.bd","a.fc","te","acc","mi","kk")
  colnames(tSynD15)=c("BE","va","sl","em","k1","k2","a.bd","a.fc","te","acc","mi","kk")
  
  EM = 0
  VA = 0
  for(ii in 1:nrow(tSynD15)){
    EM <- EM+ tSynD15$em[[ii]]*tSynD15$BE[[ii]]
    VA <- VA+ tSynD15$va[[ii]]*tSynD15$BE[[ii]]
  }
  EM_list[[ii]] <- EM
  VA_list[[ii]] <- VA
}
EM_list <- do.call(rbind,EM_list)
VA_list <- do.call(rbind,VA_list)
# DWH test
# ii=1
# DWH(LS02.L[[ii]]$fit_gmm_01,LS02.L[[ii]]$lmm.15)
# DWH(LS02.L[[ii]]$fit_gmm_01)
# ?DWH
# https://www.ier.hit-u.ac.jp/~kitamura/lecture/Hit/08Statsys5.pdf
pp <- matrix(c(1:30), nrow = 2, byrow = TRUE)
system.time({ # 210.97 sec
  for(ii in 1:length(indv)){ # ii=1
    v0=vcov(LS02.L[[ii]]$lmm.15)
    v0=v0[-1,] %>% .[,-1]
    v1=vcov(LS02.L[[ii]]$fit_gmm_01)
    attr(v1,"type")=NULL
    
    b0=LS02.L[[ii]]$lmm.15$coefficients %>% .[-1]
    b1=LS02.L[[ii]]$fit_gmm_01@theta
    
    DHW1=t(b0-b1)%*%solve(v0-v1)%*%t(t(b0-b1)) # difference from OLS
    pp[1,ii]=pchisq(DHW1,df=2)
    
    DHW2=t(b1-b0)%*%solve(v1-v0)%*%t(t(b1-b0)) # difference from OLS
    pp[2,ii]=pchisq(DHW2,df=2)
  }
})
pp2 <- t(pp)
save(pp,file="data/pp.xdr")
load(pp,file="data/pp.xdr")

#全要素生産性(TFP)の計算
TFP <- list()
em.kk.DWH <- list()
for(ii in 1:length(indv)){ # ii=12
  cat("ii=",ii,"\n")
  tSynD=SynD3 %>% filter(code==indv[ii]) %>% 
    mutate(mi.11=sl.11-va.11,mi.15=sl.15-va.15, kk11=k1.11+k2.11, kk15=k2.11+k2.15,va.per.em11=va.11/em.11,va.per.em15=va.15/em.15)
  tSynD[sapply(tSynD,function(x)return(x<0))] <- NA
  tSynD$BE.15[tSynD$BE.15 < 1] <- NA
  tSynD$em.15[tSynD$em.15 < 1] <- NA
  tSynD=na.omit(tSynD)
  
  #tSynD11=dplyr::select(tSynD,contains("11"))
  tSynD15=dplyr::select(tSynD,contains("15"))
  
  #em.15re <- tSynD15 [,c("em.15")]
  #BE.15re <- tSynD15 [,c("BE.15")]
  #EM.15 = log(em.15re*BE.15re+1)
  
  #tSynD11.MUN=dplyr::select(tSynD,contains("MUN"),contains("11"))
  tSynD15.MUN=dplyr::select(tSynD,contains("MUN"),contains("va.per.em15"))
  
  #colnames(tSynD11)=c("BE","va","sl","em","k1","k2","a.bd","a.fc","te","acc","mi","kk")
  colnames(tSynD15)=c("BE","va","sl","em","k1","k2","a.bd","a.fc","te","acc","mi","kk")
  
  #tSynD11m=log(tSynD11+1)
  tSynD15m=log(tSynD15+1)
  
  em.15 <- tSynD15m [,c("em")]
  kk.15 <- tSynD15m [,c("kk")]
  va.15 <- tSynD15m [,c("va")]
  BE.15 <- tSynD15m [,c("BE")]
  EM.15 = BE.15+em.15
  
  #if(ii==3 || ii==8 ||  ii==11 ){
  #em.coef = LS02.L[[ii]][["lmm.15"]][["coefficients"]][["em"]]
  #kk.coef = LS02.L[[ii]][["lmm.15"]][["coefficients"]][["kk"]]
  #tfp.15 = tSynD15m$va-em.15*em.coef-kk.15*kk.coef
  #}
  #else{
  em.coef = LS02.L[[ii]][["fit_gmm_01"]]@theta[["em"]]
  kk.coef = LS02.L[[ii]][["fit_gmm_01"]]@theta[["kk"]]
  tfp.15 = va.15-em.15*em.coef-kk.15*kk.coef
  #}
  tfp <- tSynD15.MUN %>% mutate(em15 = em.15,kk15 = kk.15,TFP = tfp.15,va15 = va.15,va.per.em15 = log(va.per.em15+1),EM15 = EM.15)
  TFP[[ii]] <- tfp
  em.kk.DWH[[ii]] <- c(em.coef,kk.coef)
  
}
view(TFP[[1]])

#従業者数、TFPグラフ
TFP_plot <-list()
for (ii in 1:length(indv)){
  plot <- ggplot(data = TFP[[ii]], aes(x = EM15, y = TFP)) +
    geom_point() +
    labs( x = "従業者数の対数値", y = "TFP")+
    theme(
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size=20),
    )
  TFP_plot[[ii]] <- plot
}
TFP_plot[[8]]
ggsave("plot_picture/農業、林業、漁業.png", plot = TFP_plot[[1]], width = 6, height = 4, dpi = 1500)
ggsave("plot_picture/鉱業、採石業.png", plot = TFP_plot[[2]], width = 6, height = 4, dpi = 1500)
ggsave("plot_picture/建設業.png", plot = TFP_plot[[3]], width = 6, height = 4, dpi = 1500)
ggsave("plot_picture/製造業.png", plot = TFP_plot[[4]], width = 6, height = 4, dpi = 1500)
ggsave("plot_picture/電気・ガス・熱供給・水道業.png", plot = TFP_plot[[5]], width = 6, height = 4, dpi = 1500)
ggsave("plot_picture/情報通信業.png", plot = TFP_plot[[6]], width = 6, height = 4, dpi = 1500)
ggsave("plot_picture/運輸業、郵便業.png", plot = TFP_plot[[7]], width = 6, height = 4, dpi = 1500)
ggsave("plot_picture/卸売業、小売業.png", plot = TFP_plot[[8]], width = 6, height = 4, dpi = 1500)
ggsave("plot_picture/金融業、保険業.png", plot = TFP_plot[[9]], width = 6, height = 4, dpi = 1500)
ggsave("plot_picture/不動産、物品賃貸業.png", plot = TFP_plot[[10]], width = 6, height = 4, dpi = 1500)
ggsave("plot_picture/学術研究、専門・技術サービス業.png", plot = TFP_plot[[11]], width = 6, height = 4, dpi = 1500)
ggsave("plot_picture/宿泊業、飲食サービス業.png", plot =TFP_plot[[12]], width = 6, height = 4, dpi = 1500)
ggsave("plot_picture/生活関連サービス業他.png", plot = TFP_plot[[13]], width = 6, height = 4, dpi = 1500)
ggsave("plot_picture/教育、学習支援業.png", plot = TFP_plot[[14]], width = 6, height = 4, dpi = 1500)
ggsave("plot_picture/医療、福祉.png", plot = TFP_plot[[15]], width = 6, height = 4, dpi = 1500)
#save(TFP,file="data/TFP.xdr")
#save(em.kk.DWH,file="data/em.kk.coef.xdr")
#TFP.1 = TFP[[1]]
#TFP.10 =TFP[[10]]
#TFP.1.Osaka = TFP[[1]] %>% filter(MUN >= 27000 & MUN <= 27999)
#TFP.1.Kansai = TFP[[1]] %>% filter(MUN >= 24000 & MUN <= 30999)
#view(SynD3)
#地図データ整理
library(sf)
Japan.map <- st_read("japan_ver84/japan_ver84.shp")
hokkaido.map <- Japan.map[1:194,]
Kansai.map <- Japan.map %>% filter(JCODE >= 24000 & JCODE <= 30999)
Kanto.map <- Japan.map[426:782,]
Kyushu.map <- Japan.map %>% filter(JCODE >= 40000 & JCODE <= 46999)

Japan.map.TFP <- list()
Japan.map.va.per.em <- list()
for(ii in 1:length(indv)){#ii=1
  TFP[[ii]]$MUN <- as.character(TFP[[ii]]$MUN)
  Japan.map.tfp <- left_join(Japan.map,TFP[[ii]],by=c("JCODE"="MUN"))
  Japan.map.tfp.filter <- select(Japan.map.tfp,TFP,geometry)
  Japan.map.va.per.em.filter <- select(Japan.map.tfp,va.per.em15,geometry)
  Japan.map.TFP[[ii]] <- Japan.map.tfp.filter
  Japan.map.va.per.em[[ii]] <- Japan.map.va.per.em.filter
}
save(Japan.map.TFP,file="data/Japan.map.TFP.xdr")


#地図描画                    
library(ggplot2)
library(ggspatial)
strokecolor=NA
fillcolor = c('#097FB3', '#A13675')
fillcolor = c("azure1")
alpha = 1

TFP.map <- list()
va.per.em.map <- list()
ii=3
for(ii in 1:length(indv)){
  #日本地図
  Japan.map.gg <- ggplot() +
    geom_sf(data = Japan.map.TFP[[ii]], color = strokecolor, fill = fillcolor[1], alpha = alpha)
  #全要素生産性マップ
  TFP.min = min(TFP[[ii]][["TFP"]])
  TFP.max = max(TFP[[ii]][["TFP"]])
  
  TFP.map[[ii]] <- Japan.map.gg +
    geom_sf(data=Japan.map.TFP[[ii]],color=NA,aes(fill=TFP),alpha=1)+
    scale_fill_viridis_c(option = "viridis",begin = 0.1,limits=c(TFP.min,TFP.max), oob = scales::squish)
  
  
  #従業者あたりの付加価値額マップ
  va.per.em.min = min(TFP[[ii]][["va.per.em15"]])
  va.per.em.max = max(TFP[[ii]][["va.per.em15"]])
  
  va.per.em.map[[ii]] <- Japan.map.gg +
    geom_sf(data=Japan.map.va.per.em[[ii]],color=NA,aes(fill=va.per.em15),alpha=1)+
    scale_fill_viridis_c(option = "viridis",begin = 0.1,limits=c(va.per.em.min,va.per.em.max), oob = scales::squish)
}

ggsave("農業、林業、漁業.png", plot = TFP.map[[1]], width = 6, height = 4, dpi = 1500)
ggsave("鉱業、採石業.png", plot = TFP.map[[2]], width = 6, height = 4, dpi = 1500)
ggsave("建設業.png", plot = TFP.map[[3]], width = 6, height = 4, dpi = 1500)
ggsave("製造業.png", plot = TFP.map[[4]], width = 6, height = 4, dpi = 1500)
ggsave("電気・ガス・熱供給・水道業.png", plot = TFP.map[[5]], width = 6, height = 4, dpi = 1500)
ggsave("情報通信業.png", plot = TFP.map[[6]], width = 6, height = 4, dpi = 1500)
ggsave("運輸業、郵便業.png", plot = TFP.map[[7]], width = 6, height = 4, dpi = 1500)
ggsave("卸売業、小売業.png", plot = TFP.map[[8]], width = 6, height = 4, dpi = 1500)
ggsave("金融業、保険業.png", plot = TFP.map[[9]], width = 6, height = 4, dpi = 1500)
ggsave("不動産、物品賃貸業.png", plot = TFP.map[[10]], width = 6, height = 4, dpi = 1500)
ggsave("学術研究、専門・技術サービス業.png", plot = TFP.map[[11]], width = 6, height = 4, dpi = 1500)
ggsave("宿泊業、飲食サービス業.png", plot = TFP.map[[12]], width = 6, height = 4, dpi = 1500)
ggsave("生活関連サービス業他.png", plot = TFP.map[[13]], width = 6, height = 4, dpi = 1500)
ggsave("教育、学習支援業.png", plot = TFP.map[[14]], width = 6, height = 4, dpi = 1500)
ggsave("医療、福祉.png", plot = TFP.map[[15]], width = 6, height = 4, dpi = 1500)
ggsave("付加価値額6.png",plot =va.per.em.map[[6]],width = 6, height = 4, dpi = 1500)


#不動産資本投入マップ
TFP.10$MUN <-as.character(TFP.10$MUN)
Japan.map.realestate <- left_join(Japan.map,TFP.10,by=c("JCODE"="MUN"))
Japan.map.realestate.filter <- select(Japan.map.realestate,kk15,geometry)


#Japan.map.gg <- ggplot() +
#geom_sf(data = Japan.map.realestate, color = strokecolor, fill = fillcolor[1], alpha = alpha)

realestate.kk.map <- Japan.map.gg +
  geom_sf(data=Japan.map.realestate.filter,aes(fill=kk15),alpha=1)+
  scale_fill_viridis_c(option = "viridis",begin = 0.1,limits=c(0,3.7), oob = scales::squish)

ggsave("10.kk.png", plot = realestate.kk.map, width = 6, height = 4, dpi = 1500)

# orver identification: no mean

# summary(fit_gmm_02)


# ?gmmFit
# ?momentModel
# 
# NCC=30 #並列計算するコア数
# cl=makeCluster(NCC, type = "SOCK")
# registerDoSNOW(cl)
# 
# system.time({ # 46.97  sec
#   irmdc.df=foreach(kk =1:nrow(mjpn2),.combine = "rbind") %dopar% {procF01(kk)} # 
# })
# 
# # average bee-line distance of intra-municipality travel for business and geographical centroid weighted by employee (JGD2000, "EPSG",4612)
# save(irmdc.df,file="data/irmdc.df.xdr")
# 
# stopCluster(cl)

#####


