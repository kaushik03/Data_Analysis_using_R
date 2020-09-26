install.packages("XML")
library("XML")
url <- "https://en.wikipedia.org/wiki/Big_data"
download.file(url, destfile = "./Data/BigData.html" )
html <- htmlTreeParse("./Data/BigData.html", useInternalNodes = TRUE)
xpathSApply(html, "//Title", xmlValue)
names(html)
