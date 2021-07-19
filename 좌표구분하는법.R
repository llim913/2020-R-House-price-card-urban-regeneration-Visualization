# sp 자료 확인
shp@data
class(shp)


library(sp)
# 좌표 확인: UTM-K(GRS-80) 좌표계에서 WGS84 경위도 좌표계로 변환한다.
# sf 데이터에서 sp 데이터로 만들 때 따로 좌표를 빼기 위함
from.crs <- " +proj=longlat +datum=WGS84 +no_defs "
to.crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
shp <- spTransform(shp, to.crs)
### 지도 확인
plot(shp)
class(shp)

## shapefile --> 데이터프레임 변환
DJ_emd_SHP <- fortify(shp)
## sf 데이터프레임 --> shapefile 변환
DJ_EMD_shp <-  as(DJ_emd_shp, 'Spatial')
class(DJ_emd_shp)
class(DJ_EMD_shp)
str(DJ_emd_SHP)

plot(shp)