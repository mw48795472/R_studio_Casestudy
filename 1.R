# 기본 페키지 세팅

install.packages(c("gstat","sf","tidyverse","ggplot2"))
install.packages("stars")

library(tidyverse)
library(sf)
library(gstat)
library(ggplot2)
library(stars)



## 공간적 내삽(Spatial Interpolation) ##


#데이터셋
N02<-read_csv(system.file("external/no2.csv",package="gstat"),show_col_type = FALSE)

View(N02)
dim(N02)
names(N02)


#좌표계정의의
crs <- st_crs("EPSG:32632")
N02.sf <- st_as_sf(N02, crs= "OGC:CRS84", coords=c("station_longitude_deg","station_latitude_deg")) |>
  st_transform(crs) 


#독일 행정경계
De <- read_sf("https://github.com/edzer/sdsr/raw/main/data/de_nuts1.gpkg") |>
  st_transform(crs)

View(De)


# 독익 N02 관측지도
ggplot() +
  geom_sf(
    data = De,
    fill = NA,
    color = "grey40"
  ) +
  geom_sf(
    data = N02.sf,
    aes(color = NO2),
    size = 2
  ) +
  scale_color_gradient(
    name = "N02",
    low = "grey30",
    high = "steelblue"
  ) +
  theme_minimal()




# 10km격자
grd <- st_bbox(De) |>
  st_as_stars(dx = 10000) |>
  st_crop

class(grd)

#역거리가중법(IDW)
idw <- gstat::idw(
  NO2 ~ 1,
  locations = N02.sf,
  newdata = grd)

class(idw)
names(idw)
plot(idw["var1.pred"])


ggplot() +
  geom_stars(
    data = idw,
    aes(fill = var1.pred)
  ) +
  geom_sf(
    data = De,
    fill = NA,
    color = "grey30"
  ) +
  geom_sf(
    data = N02.sf,
    color = "black",
    size = 0.8
  ) +
  scale_fill_gradient(
    name = "N02",
    low = "grey80",
    high = "steelblue"
  ) +
  theme_gray()





