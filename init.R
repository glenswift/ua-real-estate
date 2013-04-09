getData <- function() {
  library(RCurl)
  library(XML)
  #Удалить файлы в директории ./data/
  #do.call(file.remove,list(list.files(paste0(getwd(),'/data', collapse = ''), full.names=TRUE)))
  #Соединится с сервером и спарсить последнюю страницу в паганации
  url <- 'http://dom.ria.ua/ru/search/?advert_type=1&realty_type=&category=0&language=ru&limit=100&page=1'
  cookie <- 'Real Estate Data Parsing'
  html <- getURL(url, cookie=cookie)
  dom <- htmlParse(html)
  pagination <- xpathSApply(dom, "//div[@class='page']", xmlValue)
  maxPagination <- as.numeric(strsplit(pagination, split='...', fixed = TRUE)[[1]][2])
  #Скачать файлы в директорию ./data/
  for (i in 1534:maxPagination) {
    url <- paste0('http://dom.ria.ua/ru/exportsearch/?advert_type=1&realty_type=&category=0&language=ru&limit=100&page=',i,'&xls=1', collapse = '')
    dir <- paste0(getwd(),'/data/', i, '.xls', collapse = '')
    f <- download.file(url, dir, mode='wb')
  }
}
