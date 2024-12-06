#地図プロット
library(sf)
Japan.map <- st_read("japan_ver84/japan_ver84.shp")
#hokkaido.map <- Japan.map[1:194,]
#Kansai.map <- Japan.map %>% filter(JCODE >= 24000 & JCODE <= 30999)
#Kanto.map <- Japan.map[426:782,]
#Kyushu.map <- Japan.map %>% filter(JCODE >= 40000 & JCODE <= 46999)

Japan.map.TFP <- list()
Japan.map.va.per.em <- list()
for(ii in 1:length(indv)){#ii=1
  TFP[[ii]]$MUN <- as.character(TFP[[ii]]$MUN)
  Japan.map.tfp <- left_join(Japan.map,TFP[[ii]],by=c("JCODE"="MUN"))
  Japan.map.tfp.filter <- select(Japan.map.tfp,JCODE,tfp15,geometry)
  Japan.map.va.per.em.filter <- select(Japan.map.tfp,JCODE,va.per.em15,geometry)
  Japan.map.TFP[[ii]] <- Japan.map.tfp.filter
  Japan.map.va.per.em[[ii]] <- Japan.map.va.per.em.filter
}
save(Japan.map.TFP,file="data/Japan.map.TFP.xdr")



library(ggplot2)
library(ggspatial)
strokecolor=NA
fillcolor = c('#097FB3', '#A13675')
fillcolor = c("azure1")
alpha = 1

TFP.map <- list()
va.per.em.map <- list()

for(ii in 1:length(indv)){
  #日本地図
  Japan.map.gg <- ggplot() +
    geom_sf(data = Japan.map.TFP[[ii]], color = strokecolor, fill = fillcolor[1], alpha = alpha)
  #全要素生産性マップ
  TFP.min = min(TFP[[ii]][["tfp15"]])
  TFP.max = max(TFP[[ii]][["tfp15"]])
  
  TFP.map[[ii]] <- Japan.map.gg +
    geom_sf(data=Japan.map.TFP[[ii]],color=NA,aes(fill=tfp15),alpha=1)+
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
