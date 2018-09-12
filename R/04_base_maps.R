### base and plotly based maps ###

## get map of united states
map1 <- get_map(location='united states',zoom = 4, maptype="terrain", source='google')

## sample maps with size by exacerbation rate with and without plotly
map2 <- ggmap(map1) + geom_point(aes(x=longitude, y=latitude, size=total_exac_percent, color = mean_severity, fill = mean_severity),
  data=exac_grouped, alpha=0.6, na.rm=TRUE) + 
  scale_fill_gradient2(low="orange", mid="red", high="purple", midpoint=2) + 
  scale_color_gradient2(low="orange", mid="red", high="purple", midpoint=2)

map3 <- map2 + facet_wrap( ~ quarter, ncol=6)

map3
ggplotly(map3) # plotly based, note that it shifts all of the points north for some reason
