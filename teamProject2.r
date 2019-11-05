library(readxl)
library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
url <- "https://www.bea.gov/system/files/2018-12/GCP_Release_1.xlsx"
destfile <- "GCP_Release_1.xlsx"
curl::curl_download(url, destfile)
GCP_Release_1 <- read_excel(destfile)


names(GCP_Release_1)[6]<-"gdp"
GCP_Release_1$gdp_num <- as.numeric(GCP_Release_1$gdp)
GCP_Release_1$subregion <- tolower(GCP_Release_1$Countyname)
gdp_data =  data.frame(GCP_Release_1$gdp_num,GCP_Release_1$subregion,GCP_Release_1$LineCode)
gdp_data = gdp_data[gdp_data$GCP_Release_1.LineCode == 1,]
names(gdp_data)[1] <- "gdp"
names(gdp_data)[2] <- "subregion"
usa <- map_data("usa")
us_base <- ggplot(data=usa, mapping = aes(x=long, y=lat, group=group))+
  coord_fixed(1.3) +
  geom_polygon(color="black", fill="gray")
# merge gdp data
counties <- map_data("county")
usgdp <- inner_join(counties, gdp_data, by="subregion")
density <- us_base +
  geom_polygon(data=usgdp,aes(fill=gdp),color="white")+
  geom_polygon(color="black", fill=NA) +
  theme_bw() +
  scale_fill_gradient(trans="log10")

density2 <- density +
  scale_fill_gradientn(
    colors = rev(rainbow(7)),
    breaks = c(2, 4, 10, 10000,100000,1000000),
    trans="log10"
  )
