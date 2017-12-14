library(ggmap)
library(rgdal)
library(RColorBrewer)

setwd('D:/MySVN/UA-Research/Dr Xia/R Code/TTR_Interpolation/TTR_Interpolation');

# extent: 
# Longitude = (120.82, 121.10);
# Latitude = (31.25, 31.48)

Mo = rgb(80, 97, 109, maxColorValue = 255);
Zhe = rgb(132, 90, 51, maxColorValue = 255);
Shilv = rgb(22, 169, 81, maxColorValue = 255);
Shiqing = rgb(22, 133, 169, maxColorValue = 255);
Huaqing = rgb(0, 52, 114, maxColorValue = 255);

Range.Longitude = c(120.82, 121.10);
Range.Latitude = c(31.25, 31.48);
Center = c(mean(Range.Longitude), mean(Range.Latitude));
Bounding = c(120.82, 31.25, 121.10, 31.48);
BaseMap.Kunshan = get_map(location = Bounding,
                          zoom = 13, maptype = 'roadmap', color = 'bw');

TaxiLoadingPoint = read.csv(file = 'D:/MySVN/UA-Research/BasicTaxiGPS/tripinfo_example.csv');



Overlay = ggmap(BaseMap.Kunshan, extent = 'device') + # 
    theme(panel.background = element_rect(fill = alpha(Zhe, 0.3), colour = NULL)) +
            theme(panel.grid.major = element_line(colour = alpha(Zhe, 0.3), size = 0.1),
                panel.grid.minor = element_line(colour = alpha(Zhe, 0.3), size = 0.1)) +
            geom_density2d(data = TaxiLoadingPoint, aes(x = Loading.Longitude, y = Loading.Latitude), size = 4, alpha = 0.5, colour = Mo) +
            #geom_density2d(data = TaxiLoadingPoint, aes(x = Loading.Longitude, y = Loading.Latitude), size = 2, alpha = 0.5, colour = Zhe) +
            stat_density2d(data = TaxiLoadingPoint, aes(x = Loading.Longitude, y = Loading.Latitude,
                    fill = ..level.., alpha = ..level..), contour = TRUE,
                    size = 0.1, geom = 'polygon', bin = 32) + #
        scale_fill_gradient(low = Shilv, high = Huaqing) +
        scale_alpha(range = c(0.5, 0.5), guide = F); #+
    
        # stat_density2d(data = TaxiLoadingPoint, aes(x = Loading.Longitude, y = Loading.Latitude,
        #                                             fill = ..level.., alpha = ..level..),
        #                size = 0.1, geom = 'polygon', h = 0.1) + #
        # scale_fill_gradient(low = Zhe, high = Zhe) +
        # scale_alpha(range = c(0.3, 0.3), guide = F)
        # geom_point(data = TaxiLoadingPoint, aes(x = Loading.Longitude, y = Loading.Latitude), size = 4, col = 'yellow');

print(Overlay);

# ttr.grid = readOGR(dsn = 'Result/ttr_to_(col_45_row_47).sqlite', layer = 'grid_kunshan_monday')
#ttr.grid.normal = fortify(ttr.grid);
#Overlay = ggmap(BaseMap.Kunshan, extent = 'device') +
            #geom_polygon(data = ttr.grid.normal, aes(x = long, y = lat, group = group, fill = ti_17_avg, alpha = 0.5)) +
            #scale_fill_gradientn(colours = c('green', 'orange', 'darkred'),
                                #breaks = seq(from = 0, to = 40, by = 5));

#print(Overlay)

# display.brewer.all(type = 'all');
# breaks_qt = classIntervals(vector_of_values, n = how_many_classes, style = "quantile"[can be omitted - -the default])

# sub.grid = subset(ttr.grid[, 'ti_18_avg'], ttr.grid[, 'ti_18_avg']$ti_18_avg >= 0)
# sub.grid = rbind(sub.grid, ttr.grid[1, 'ti_18_avg'], ttr.grid[10000, 'ti_18_avg']);
# LabelInterval = seq(from = 0, to = 40, by = 5);
# pal = brewer.pal(length(LabelInterval), 'OrRd');
# pal = heat.colors(length(LabelInterval), alpha = 0.8);
# DefinedPalette = colorRampPalette(c('green', 'orange', 'darkred'));
# pal = DefinedPalette(length(LabelInterval));
# 
# sp.raster = list('grid.raster', BaseMap.Kunshan, x = Center[1], y = Center[2],
#                   default.units = 'native', first = TRUE)
# 
# p1 = spplot(sub.grid, 'ti_18_avg', sp.layout = sp.raster,
#     col.regions = pal, at = LabelInterval, col = NA);
# print(p1);
