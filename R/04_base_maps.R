### base and plotly based maps ###

## get map of united states
map1 <- get_googlemap(center=c(lon=-96.5795, lat=39.8283), zoom=4, key="AIzaSyDAwVOtaV9r-yRyJtOxOYgaf--q4pv18hg", maptype="roadmap")

##### sample maps with size by exacerbation rate with and without plotly ###


map_azithro <- ggmap(map1) + geom_point(aes(x=longitude, y=latitude, size=total_exac_percent, color = mean_severity),
                                 data=exac_grouped_ninety_azithro, alpha=0.6, na.rm=TRUE) + 
  scale_color_gradient2(low="orange", mid="red", high="purple", midpoint=2, limits=c(1,4)) + 
  scale_size_continuous(limits=c(0,100), range=c(0,10)) +
  labs(colour="Severity of\nExacerbation", size="Exacerbation\nRate") +
  guides(colour = guide_colorbar(order = 1), 
         shape = guide_legend(order = 2)) +
  theme(legend.title = element_text(face="bold", size=20)) +
  theme(legend.text = element_text(size=14)) +
  # theme(legend.direction = "horizontal") +
  # theme(legend.position = c(0,0), legend.justification = c(0,0)) +
  theme(legend.background = element_rect(fill=alpha('grey',0.7))) +
  theme(legend.key = element_blank()) +
  facet_wrap( ~ ninetyday, ncol=4) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

map_azithro

# ggplotly(map_azithro) # plotly based, note that it shifts all of the points north for some reason, may need coord_cartesian

map_placebo <- ggmap(map1) + geom_point(aes(x=longitude, y=latitude, size=total_exac_percent, color = mean_severity),
                                        data=exac_grouped_ninety_placebo, alpha=0.6, na.rm=TRUE) + 
  scale_color_gradient2(low="orange", mid="red", high="purple", midpoint=2, limits=c(1,4)) + 
  scale_size_continuous(limits=c(0,100), range=c(0,10)) +
  labs(colour="Severity of\nExacerbation", size="Exacerbation\nRate") +
  guides(colour = guide_colorbar(order = 1), 
         shape = guide_legend(order = 2)) +
  theme(legend.title = element_text(face="bold", size=20)) +
  theme(legend.text = element_text(size=14)) +
  # theme(legend.direction = "horizontal") +
  # theme(legend.position = c(0,0), legend.justification = c(0,0)) +
  theme(legend.background = element_rect(fill=alpha('grey',0.7))) +
  theme(legend.key = element_blank()) +
  facet_wrap( ~ ninetyday, ncol=4) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

map_placebo

# ggplotly(map_placebo) # plotly based, note that it shifts all of the points north for some reason, may need coord_cartesian
