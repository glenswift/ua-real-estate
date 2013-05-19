#packages needed
library(RCurl)   #working with curl
library(XML)     #parsing xml
library(RODBC)   #connection with excel

#global variables
metro.kiev <- c('Героев Днепра', 'Минская', 'Оболонь', 'Петровка', 'Тараса Шевченко', 'Контрактовая площадь', 'Почтовая площадь', 'Майдан Независимости', 'Площадь Л. Толстого', 'Олимпийская', 'Дворец Украина', 'Лыбедская', 'Демиевская', 'Голосеевская', 'Васильковская', 'Выставочный центр', 'Ипподром', 'Академгородок', 'Житомирская', 'Святошин', 'Нивки', 'Берестейская', 'Шулявская', 'Политехнический институт', 'Вокзальная', 'Университет', 'Театральная', 'Крещатик', 'Арсенальная', 'Днепр', 'Гидропарк', 'Левобережная', 'Дарница', 'Черниговская', 'Лесная', 'Красный хутор', 'Бориспольская', 'Вырлица', 'Харьковская', 'Позняки', 'Осокорки', 'Славутич', 'Выдубичи', 'Дружбы народов', 'Печерская', 'Кловская', 'Дворец спорта', 'Золотые ворота', 'Лукьяновская', 'Дорогожичи', 'Сырецкая')

downloadData <- function() {
  #delete files in ./data/ folder
  do.call(file.remove,list(list.files(paste0(getwd(),'/data', collapse = ''), full.names=TRUE)))
  #connect to server and parse last pagination number
  url <- 'http://dom.ria.ua/ru/search/?advert_type=1&realty_type=&category=0&language=ru&limit=100&page=1'
  cookie <- 'Real Estate Data Parsing'
  html <- getURL(url, cookie=cookie)
  dom <- htmlParse(html)
  pagination <- xpathSApply(dom, "//div[@class='page']", xmlValue)
  maxPagination <- as.numeric(strsplit(pagination, split='...', fixed = TRUE)[[1]][2])
  #download files to ./data/ folder
  for (i in 1:maxPagination) {
    url <- paste0('http://dom.ria.ua/ru/exportsearch/?advert_type=1&realty_type=&category=0&language=ru&limit=100&page=',i,'&xls=1', collapse = '')
    dir <- paste0(getwd(),'/data/', i, '.xls', collapse = '')
    f <- download.file(url, dir, mode='wb')
  }
}

createDatabase <- function() {
  #number of files in ./data folder
  files <- length(list.files(paste0(getwd(),'/data', collapse = ''), full.names=TRUE))
  df <- data.frame()
  for (i in 1:files) {
    #console what file we're working with
    print(paste0('processing ',i,'.xls...'))
    #connect to excel file, join it to dataframe df and close connection
    channel <- odbcConnectExcel(paste0(getwd(),'/data/',i,'.xls', collapse = ''))
    catch <- try(sqlTables(channel))
    if (inherits(catch, 'try-error')) {
      print(paste0('error in ',i,'.xls. Skipping to next'))
      next
    }
    sh1 <- sqlFetch(channel, "Page 1")
    df <- rbind(data.frame(sh1), df)
    odbcClose(channel)
  }
  #export dataframe to database.csv in ./csvdata folder
  write.csv(df, file=paste0(getwd(),'/csvdata/database.csv'), row.names = FALSE)
}

getRiaCoords <- function(row) {
  baseUrl <- 'http://dom.ria.ua/ru/realty-'
  cookie <- 'Real Estate row Parsing'
  #print(paste0('Processing ', row$Realty.ID[1], ' - ', i, ' from ', length(row$Realty.ID)))
  url <- paste0(baseUrl, row$Realty.ID[1], '.html')
  html <- getURL(url, cookie=cookie)
  catch <- try({
    dom <- htmlParse(html)
  })
  if (inherits(catch, 'try-error')) {
    posX <- NA
    posY <- NA
    print('++Unexpected error, skipping to next')
    return (c(NA, NA))
  }
  posX <- xpathSApply(dom, "//input[@id='geo_x']", xmlGetAttr, "value")
  posY <- xpathSApply(dom, "//input[@id='geo_y']", xmlGetAttr, "value")
  return (c(posX, posY))
}

getYaCoord <- function(row) {
  baseUrl <- 'http://geocode-maps.yandex.ru/1.x/?geocode='
  afterUrl <- 'results=1'
  cookie <- 'Real Estate Data Parsing'
  country <- 'Украина'
  department <- paste0(row$Область[1], '+область')
  city <- row$Город[1]
  if (is.na(row$Район[1]) && is.na(row$Улица[1])) {
    #replace NA data with empty strings
    region = address = ''
  } else {
    #ok, we have one actual address parameter. Let's get address
    if (!is.na(row$Район[1])) {
      region <- paste0('район+', row$Район[1])
    } else {
      region <- ''
    }
    if (!is.na(row$Улица[1])) {
      #if address didn't set like underground station name
      if (!row$Улица[1] %in% metro.kiev) {
        #delete banned words from address field
        address <- paste0('улица+', 
                          sub("(Улица|Ул.|улица|ул.|ул)", "",
                              row$Улица[1])
        )
        afterUrl <- paste0(afterUrl, '&kind=house')
      } else {
        #if address set like underground station name
        address <- paste0('метро+', row$Улица[1])
        afterUrl <- paste0(afterUrl, '&kind=metro')
      }
    } else {
      address <- ''
    }
  }
  url <- paste(paste(baseUrl, country, department, city, region, address, sep = "+"), afterUrl, sep = '&')
  html <- getURL(url, cookie=cookie)
  dom <- htmlParse(html)
  print <- dom
  point <- xpathSApply(dom, "//pos", xmlValue)
  kind <- xpathSApply(dom, "//kind", xmlValue)
  catch <- try({
    posX <- as.numeric(substr(point, 0, 9))
    posY <- as.numeric(substr(point, 11, 20))
  })
  if (inherits(catch, 'try-error')) {
    posX <- NA
    posY <- NA
    print(paste0('error in ',i,'Object. Coordinates setted to NA. Skipping to next'))
    return(c(NA, NA))
  }
  if (kind == 'street') {
    posX = posX + rnorm(1, 0.005462, 0.01)
    posY = posY + rnorm(1, 0.005462, 0.01)
  }
  return(c(posX, posY))
}

getCoords <- function(data, start = 1001) {
  for (i in start:length(data$Realty.ID)) {
    connection <- try ({
      print(paste0('Processing ', data$Realty.ID[i], ' - ', i, ' from ', length(data$Realty.ID)))
      coordsRia <- getRiaCoords(data[i, ])
      posX <- coordsRia[1]
      posY <- coordsRia[2]
      if (is.list(coordsRia)) {
        print('++No result. Sending data to Yandex')
        coordsYa <- getYaCoord(data[i, ])
        posX <- coordsYa[1]
        posY <- coordsYa[2]
      }
      data$posX[i] <- as.numeric(posX)
      data$posY[i] <- as.numeric(posY)
      if (i %% 1000 == 0) {
        print('Saving changes..')
        write.csv(data, paste0(getwd(),'/csvdata/database.csv'), row.names=FALSE)
      }
    })
    if (inherits(connection, 'try-error')) {
      print('++Error in connection. Waiting for 10 seconds')
      Sys.sleep(10)
      return (getCoords(data, i))
    }
  }
  return(data)
}



getData <- function(city) {
  #import data from database.csv
  data <- data.frame(read.csv(paste0(getwd(),'/csvdata/database.csv')))
  #if city parameter set, choose selected city from data
  if (!missing(city)) {
    data <- subset(data, Город == city)
  }
  return(data)
}