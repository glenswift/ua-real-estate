downloadData <- function() {
  library(RCurl)
  library(XML)
  #Удалить файлы в директории ./data/
  do.call(file.remove,list(list.files(paste0(getwd(),'/data', collapse = ''), full.names=TRUE)))
  #Соединится с сервером и спарсить последнюю страницу в паганации
  url <- 'http://dom.ria.ua/ru/search/?advert_type=1&realty_type=&category=0&language=ru&limit=100&page=1'
  cookie <- 'Real Estate Data Parsing'
  html <- getURL(url, cookie=cookie)
  dom <- htmlParse(html)
  pagination <- xpathSApply(dom, "//div[@class='page']", xmlValue)
  maxPagination <- as.numeric(strsplit(pagination, split='...', fixed = TRUE)[[1]][2])
  #Скачать файлы в директорию ./data/
  for (i in 1:maxPagination) {
    url <- paste0('http://dom.ria.ua/ru/exportsearch/?advert_type=1&realty_type=&category=0&language=ru&limit=100&page=',i,'&xls=1', collapse = '')
    dir <- paste0(getwd(),'/data/', i, '.xls', collapse = '')
    f <- download.file(url, dir, mode='wb')
  }
}

createDatabase <- function() {
  library(RODBC)
  files <- length(list.files(paste0(getwd(),'/data', collapse = ''), full.names=TRUE))
  df <- data.frame()
  for (i in 1:files) {
    print(paste0('processing ',i,'.xls...'))
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
  write.csv(df, file=paste0(getwd(),'/csvdata/database.csv'), row.names = FALSE)
}

getData <- function(city) {
  data <- data.frame(read.csv(paste0(getwd(),'/csvdata/database.csv')))
  if (!missing(city)) {
    data <- subset(data, Город == city)
  }
  return(data)
}