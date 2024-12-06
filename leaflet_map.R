
#install.packages("leaflet")
#install.packages("viridis")
library(viridis)
library(leaflet)
library(htmlwidgets)
tfp.leaf <- list()

for (ii in 1:length(indv)) {
  tfp.leaf.4 <- Japan.map.TFP[[ii]] %>% na.omit()
  mean_value_4 <- mean(tfp.leaf.4$TFP)
  var_value_4 <- var(tfp.leaf.4$TFP)

  # 上限と下限を設定
  lower_bound_4 <- mean_value_4 - 2.58*var_value_4
  upper_bound_4 <- mean_value_4 + 2.58*var_value_4


  # 値を上限と下限でクリップする
  tfp.leaf.4$clipped_TFP <- pmin(pmax(tfp.leaf.4$TFP, lower_bound_4), upper_bound_4)

  #桁数を丸める
  tfp.leaf.4$round_TFP <- round(tfp.leaf.4$TFP,3)

  tfp.leaf.crs.4 <- st_transform(tfp.leaf.4,crs = 4326)

  pal <- colorNumeric(
    palette = viridis(100),
    domain = tfp.leaf.crs.4$clipped_TFP
  )

#m <- leaflet() %>% addTiles()
#m

  map.4 <- leaflet() %>%
    addTiles() %>%
    #setView(lng = , lat = , zoom = )
    addPolygons(
      data = tfp.leaf.crs.4,
      fillColor = ~pal(clipped_TFP),
      color = NA,
      weight = 1,
      fillOpacity = 0.7,
      popup = ~paste('<div style="font-size: 18px; font-family: Arial; color: blue;">',
                     "TFP:", round_TFP,
                     '</div>')
      #popup = ~paste("TFP:", round_TFP)  
      #color = "blue", 
      #fillColor = "lightblue", 
      #fillOpacity = 0.5, 
      #weight = 2
    )%>%
    addLegend(
      "bottomright", pal = pal, values = tfp.leaf.crs.4$clipped_TFP,
      title = "TFP",
      labFormat = labelFormat(),
      opacity = 1
      )

  tfp.leaf[[ii]] <- map.4
}
tfp.leaf[[1]]

saveWidget(tfp.leaf[[1]], file="html/map1.html")
saveWidget(tfp.leaf[[2]], file="html/map2.html")
saveWidget(tfp.leaf[[3]], file="html/map3.html")

saveWidget(tfp.leaf[[4]], file="html/map4.html")
saveWidget(tfp.leaf[[5]], file="html/map5.html")
saveWidget(tfp.leaf[[6]], file="html/map6.html")
saveWidget(tfp.leaf[[7]], file="html/map7.html")

saveWidget(tfp.leaf[[8]], file="html/map8.html")
saveWidget(tfp.leaf[[9]], file="html/map9.html")
saveWidget(tfp.leaf[[10]], file="html/map10.html")
saveWidget(tfp.leaf[[11]], file="html/map11.html")
saveWidget(tfp.leaf[[12]], file="html/map12.html")
saveWidget(tfp.leaf[[13]], file="html/map13.html")
saveWidget(tfp.leaf[[14]], file="html/map14.html")
saveWidget(tfp.leaf[[15]], file="html/map15.html")
