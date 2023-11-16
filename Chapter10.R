
library(chillR)
station_list<-handle_gsod(action="list_stations",
                          location=c(7.10,50.73),
                          time_interval=c(1990,2020))
require(kableExtra)

kable(station_list) %>%
  kable_styling("striped", position = "left", font_size = 8)

weather<-handle_gsod(action="download_weather",
                     location=station_list$chillR_code[4],
                     time_interval=c(1990,2020))

weather[[1]][1:20,]
cleaned_weather<-handle_gsod(weather)
cleaned_weather[[1]][1:20,]
