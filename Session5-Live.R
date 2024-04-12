
library(sf)
library(tmap)

markets <- st_read("data/farmers_markets_2012.shp")
head(markets)

tmap_mode("view")

tm_basemap("CartoDB.Voyager") + 
  tm_shape(markets) + tm_dots(size=0.01) 

markets.buffer <- st_buffer(markets, 2640)

tm_basemap("CartoDB.Voyager") + 
  tm_shape(markets) + tm_dots(alpha=0.5) +
  tm_shape(markets.buffer) + tm_borders(alpha = 0.6)

zips <- st_read("data/chizips.geojson")

tmap_mode("plot")

tm_shape(zips) + tm_borders(alpha=0.3) + tm_fill(col="gray90") +
  tm_shape(markets.buffer) + tm_fill(col = "turquoise1", alpha = 0.4) +
  tm_shape(markets) + tm_dots(size = 0.03) 

markets.buffer1 <- st_buffer(markets, 5280)

markets.buffer1 <- st_buffer(markets, 5280)

tm_shape(zips) + tm_borders(alpha=0.3) + tm_fill(col="gray90") +
  tm_shape(markets.buffer1) + tm_fill(col = "turquoise4", alpha = 0.4) +
  tm_shape(markets.buffer) + tm_fill(col = "turquoise1", alpha = 0.4) +
  tm_shape(markets) + tm_dots(size = 0.03) 

buffer.union <- st_union(markets.buffer)
buffer.union1 <- st_union(markets.buffer1)

tm_shape(zips) + tm_borders(alpha=0.3) + tm_fill(col="gray90") +
  tm_shape(buffer.union1) + tm_polygons(col = "turquoise4", alpha = 0.4) +
  tm_shape(buffer.union) + tm_fill(col = "turquoise1", alpha = 0.4) +
  tm_shape(markets) + tm_dots(size = 0.03)


tm_shape(zips) +
  tm_polygons("Case.Rate...Cumulative", 
              style="pretty", pal="viridis",
              legend.hist=T, n=4,
              title = "COVID Case Rate", ) + 
  tm_scale_bar(position = "left") + 
  tm_layout(legend.outside = TRUE, legend.outside.position = "right")


tm_shape(zips_final) +
  tm_polygons("Case.Rate...Cumulative", 
              style="quantile", pal="BuPu",
              legend.hist=T, n=6,
              title = "COVID Case Rate", ) + 
  tm_scale_bar(position = "left") + 
  tm_layout(legend.outside = TRUE, legend.outside.position = "right")


tm_shape(zips_final) +
  tm_polygons("Case.Rate...Cumulative", 
              style="jenks", pal="BuPu",
              legend.hist=T, n=4,
              title = "COVID Case Rate", ) + 
  tm_scale_bar(position = "left") + 
  tm_layout(legend.outside = TRUE, legend.outside.position = "right")


tm_shape(zips_final) +
  tm_polygons("Case.Rate...Cumulative", 
              style="equal", pal="BuPu",
              legend.hist=T, n=4,
              title = "COVID Case Rate", ) + 
  tm_scale_bar(position = "left") + 
  tm_layout(legend.outside = TRUE, legend.outside.position = "right")


COVID <- tm_shape(zips_final) + tm_fill("Case.Rate...Cumulative", 
                                        style="jenks", pal="Reds", n=4, title = "COVID Rt") 

Senior <- tm_shape(zips_final) + tm_fill("ovr65P", 
                                         style="jenks", pal="BuPu", n=4) 

NoHS <- tm_shape(zips_final) + tm_fill("noHSP", 
                                       style="jenks", pal="BuPu", n=4)

BlkP <- tm_shape(zips_final) + tm_fill("blackP", 
                                       style="jenks", pal="BuPu", n=4) 

Latnx <- tm_shape(zips_final) + tm_fill("hispP", 
                                        style="jenks", pal="BuPu", n=4) 

WhiP <- tm_shape(zips_final) + tm_fill("whiteP", 
                                       style="jenks", pal="BuPu", n=4) 

tmap_arrange(COVID, Senior, NoHS, BlkP, Latnx, WhiP)


#install.packages("cartogram")
library(cartogram)

carto.Dorling <- cartogram_dorling(zips_final, "Case.Rate...Cumulative", k = 2)

tm_shape(zips_final) + tm_polygons() + 
  tm_shape(carto.Dorling) + tm_polygons("Case.Rate...Cumulative",
                                        style="jenks", pal="BuPu", legend.hist=T, n=4,) + 
  tm_layout(legend.outside = TRUE, frame = FALSE, legend.outside.position = "right")


carto.distort <- cartogram_ncont(zips_final, "Case.Rate...Cumulative", k = 1)

tm_shape(zips_final) + tm_polygons() + 
  tm_shape(carto.distort) + tm_polygons("Case.Rate...Cumulative",
                                        style="jenks", pal="BuPu", legend.hist=T, n=4,) + 
  tm_layout(legend.outside = TRUE, frame = FALSE, legend.outside.position = "right")







