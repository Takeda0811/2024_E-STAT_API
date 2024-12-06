#honi=1:214
#okii=215:221
oki.map.TFP <- list()
kgs.map.TFP <- list()
hon.map.TFP <- list()
Japan.map.TFP <- list()

for(ii in 1:length(indv)){#ii=1
  TFP[[ii]]$MUN <- as.character(TFP[[ii]]$MUN)
  Japan.map.tfp <- left_join(Japan.map,TFP[[ii]],by=c("JCODE"="MUN")) %>% st_transform(crs=4326)#データの結合
  #沖縄抽出
  shp.hon <- subset(Japan.map.tfp, substring(Japan.map.tfp$JCODE, 1, 2) != "47") %>% st_transform(crs=4326) # subset(MEAmap2, substring(shp$N03_007, 1, 2) != "47")  # 本土分抽出
  #shp.oki <- subset(Japan.map.tfp, substring(Japan.map.tfp$JCODE, 1, 2) == "47") %>% st_transform(crs=4326) # subset(shp, substring(shp$N03_007, 1, 2) == "47")  # 沖縄分抽出
  #shp.oki.g=st_geometry(shp.oki)
  
  mjpn0.oki=subset(Japan.map.tfp, substring(Japan.map.tfp$JCODE, 1, 2) == "47") %>%st_transform(crs=4326)  # 沖縄分抽出
  mjpn0.oki.g=st_geometry(mjpn0.oki)
  mjpn0.kgs=subset(Japan.map.tfp, substring(Japan.map.tfp$JCODE, 1, 2) == "46") %>%st_transform(crs=4326) # 鹿児島分抽出
  mjpn0.kgs.g=st_geometry(mjpn0.kgs)
  
  pt1=st_point(x=c(5,16.5)) %>% st_sfc(crs=4326) #%>% st_transform(crs=6668)
  #shp.oki.g2=shp.oki.g+pt1
  mjpn0.oki.g2=mjpn0.oki.g+pt1
  mjpn0.kgs.g2=mjpn0.kgs.g+pt1
  
  st_crs(mjpn0.oki.g2)=st_crs(mjpn0.oki.g)
  st_crs(mjpn0.kgs.g2)=st_crs(mjpn0.oki.g)
  #st_crs(R10map.oki.g2)=st_crs(mjpn0.oki.g)
  #st_crs(shp.oki.g2)=st_crs(mjpn0.oki.g)
  
  mjpn0.oki$geometry <- mjpn0.oki.g2
  mjpn0.kgs$geometry <- mjpn0.kgs.g2
  
  
  #R10map.oki.g1=R10map %>% subset(Reg10code==10 | Reg10code==9) %>% st_geometry()
  #R10map.oki.g2=R10map.oki.g1+pt1
  
  # border <- cbind(c(129, 132.5, 137.0, 137.0),c( 39, 39, 42.5, 45)) %>% st_linestring() %>% st_sfc(crs=4326)
  
  
  
  
  #Japan.map.tfp.filter <- select(Japan.map.tfp,JCODE,TFP,geometry)
  #Japan.map.va.per.em.filter <- select(Japan.map.tfp,JCODE,va.per.em15,geometry)
  oki.map.TFP[[ii]] <- mjpn0.oki
  kgs.map.TFP[[ii]] <- mjpn0.kgs
  Japan.map.TFP[[ii]] <- Japan.map.tfp
  #Japan.map.va.per.em[[ii]] <- Japan.map.va.per.em.filter
}



border <- cbind(c(129, 132.5, 137.0, 137.0),c( 39, 39, 42.5, 45)) %>% st_linestring() %>% st_sfc(crs=4326)
TFP.map2 <- list()
TFP.range <- list()
#TFP.min.range <- list()
#TFP.max.range <- list()
for(ii in 1:length(indv)){#ii=1
  TFP.min = min(TFP[[ii]][["TFP"]])
  TFP.max = max(TFP[[ii]][["TFP"]])
  average = mean(TFP[[ii]][["TFP"]])
  variance = var(TFP[[ii]][["TFP"]])
  #TFP.range[[ii]] <- average
  #TFP.min.range[[ii]] <- TFP.min
  #TFP.max.range[[ii]] <- TFP.max
  
  basemap= ggplot(data = shp.hon) +
    geom_sf(data=Japan.map.TFP[[ii]],color=NA,aes(fill=TFP),alpha=1)+
    geom_sf(data=oki.map.TFP[[ii]],color=NA,aes(fill=TFP),alpha=1)+
    geom_sf(data=kgs.map.TFP[[ii]],color=NA,aes(fill=TFP),alpha=1)+
    geom_sf(data=border,fill=NA,lwd=0.5,color="black")+
    #coord_sf(xlim = c(map.bounds["xmin"], map.bounds["xmax"]),
    #ylim = c(map.bounds["ymin"], map.bounds["ymax"])) +
  
    scale_fill_viridis_c(option = "viridis",begin = 0.1,limits=c(average-2.58*variance,average+2.58*variance), oob = scales::squish)+
    coord_sf(xlim = c(128.34369, 146.44038),
           ylim = c(31.11246,45.52638)) +
    # coord_fixed(xlim = c(map.bounds["xmin"], map.bounds["xmax"]),
    #          ylim = c(map.bounds["ymin"], map.bounds["ymax"])) +
    theme(text = element_text(size = 24),legend.key.width=unit(0.5,"cm"),legend.key.height=unit(1.5,"cm"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line=element_blank())
  
  #方位付マップ
  basemap_wnasc=basemap+
    annotation_north_arrow(location = "br", which_north = "true",
                           height=unit(2,"cm"),width=unit(2,"cm"),
                           pad_x = unit(0.5, "cm"), pad_y = unit(1, "cm"),
                           style = north_arrow_fancy_orienteering(text_size=10)) +
    annotation_scale(location = "br", width_hint = 0.3,height=unit(0.25,"cm"),text_cex=1,pad_x = unit(0.25, "cm"), pad_y = unit(0.25, "cm"),)
  
  TFP.map2[[ii]] <- basemap_wnasc
}
TFP.map2[[8]]
#which(TFP[[4]]$MUN == "01517")

ggsave("pictures/農業、林業、漁業.png", plot = TFP.map2[[1]], width = 6, height = 4, dpi = 1500)
ggsave("pictures/鉱業、採石業.png", plot = TFP.map2[[2]], width = 6, height = 4, dpi = 1500)
ggsave("pictures/建設業.png", plot = TFP.map2[[3]], width = 6, height = 4, dpi = 1500)
ggsave("pictures/製造業.png", plot = TFP.map2[[4]], width = 6, height = 4, dpi = 1500)
ggsave("pictures/電気・ガス・熱供給・水道業.png", plot = TFP.map2[[5]], width = 6, height = 4, dpi = 1500)
ggsave("pictures/情報通信業.png", plot = TFP.map2[[6]], width = 6, height = 4, dpi = 1500)
ggsave("pictures/運輸業、郵便業.png", plot = TFP.map2[[7]], width = 6, height = 4, dpi = 1500)
ggsave("pictures/卸売業、小売業.png", plot = TFP.map2[[8]], width = 6, height = 4, dpi = 1500)
ggsave("pictures/金融業、保険業.png", plot = TFP.map2[[9]], width = 6, height = 4, dpi = 1500)
ggsave("pictures/不動産、物品賃貸業.png", plot = TFP.map2[[10]], width = 6, height = 4, dpi = 1500)
ggsave("pictures/学術研究、専門・技術サービス業.png", plot = TFP.map2[[11]], width = 6, height = 4, dpi = 1500)
ggsave("pictures/宿泊業、飲食サービス業.png", plot = TFP.map2[[12]], width = 6, height = 4, dpi = 1500)
ggsave("pictures/生活関連サービス業他.png", plot = TFP.map2[[13]], width = 6, height = 4, dpi = 1500)
ggsave("pictures/教育、学習支援業.png", plot = TFP.map2[[14]], width = 6, height = 4, dpi = 1500)
ggsave("pictures/医療、福祉.png", plot = TFP.map2[[15]], width = 6, height = 4, dpi = 1500)
  # basemap=ggplot(data = shp.hon) +
  #   geom_sf(data=mjpn1,fill="gray",lwd=0.2,color="gray")+
  #   geom_sf(data=mjpn0.oki.g2,fill="gray",lwd=0.2,color="gray")+
  #   geom_sf(data=mjpn0.kgs.g2,fill="gray",lwd=0.2,color="gray")+
  #   geom_sf(data=border,fill=NA,lwd=1,color="black")+
  #   coord_sf(xlim = c(map.bounds["xmin"], map.bounds["xmax"]),
  #            ylim = c(map.bounds["ymin"], map.bounds["ymax"])) +
  #   # coord_fixed(xlim = c(map.bounds["xmin"], map.bounds["xmax"]),
  #   #          ylim = c(map.bounds["ymin"], map.bounds["ymax"])) +
  #   theme(text = element_text(size = 24),legend.key.width=unit(1,"cm"),legend.key.height=unit(3,"cm"))

#方位付マップ
basemap_wnasc=basemap+
  annotation_north_arrow(location = "br", which_north = "true",
                         height=unit(2,"cm"),width=unit(2,"cm"),
                         pad_x = unit(0.5, "cm"), pad_y = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering(text_size=10)) +
  annotation_scale(location = "br", width_hint = 0.3,height=unit(0.25,"cm"),text_cex=1,pad_x = unit(0.25, "cm"), pad_y = unit(0.25, "cm"),)

# dir.create("figs/map240703")
tstr=paste0("figs/map240703/base_01.png")
ggsave(tstr, plot = basemap, width=1500,height=1500,units="px",dpi=300)
tstr=paste0("figs/map240703/base_01_wnasc.png")
ggsave(tstr, plot = basemap_wnasc, width=1500,height=1500,units="px",dpi=300)

