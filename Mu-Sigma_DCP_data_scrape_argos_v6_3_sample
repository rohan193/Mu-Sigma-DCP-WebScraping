#Sample 4 postcodes

#install.packages("RSelenium")
library(RSelenium)

#setwd("C:\\Users\\rohan.nanaware\\Desktop\\DSG Retail Limited\\Delivery availability\\Web scraping from argos")

setwd("C:/Users/rohan.nanaware/Documents/Documents/DSG Retail Limited/Delivery availability/Web scraping from argos/ ")

post.codes <- read.csv(file = "post_codes_sample.csv", as.is = T)

product.details <- read.csv(file = "Argos Matches 29 March.csv", header = T, as.is = T)

argos.data <- data.frame(matrix(NA, nrow= nrow(product.details)*nrow(post.codes), ncol=ncol(product.details)+4))

colnames(argos.data) <- c("Merchandise.Area", "Planning.Group", "Product", "Competitor", "Competitor.Product.Code",
                                 "URL", "Retail.Price.Excl.Delivery.2", "Sales.YTD", "post_codes",
                                 "Order now", "Standard delivery charge", "Saturday delivery availability")
#checkForServer()
startServer()
fox <- remoteDriver()
fox$open()
for (i in 1:nrow(product.details)) {
  a <- Sys.time()
  fox$navigate(product.details$URL[i])
  for (j in 1:nrow(post.codes)) {
    argos.searchbox.postcode <- fox$findElement(using = "css selector", "body div.site-container div.column div div.fulfilment-col.columns div.fulfilment div.fulfilment-content form input[type='text']")
    argos.searchbox.postcode$sendKeysToElement(list(post.codes$post_code[j]))
    argos.searchbox.postcode.button <- fox$findElement(using = "css selector", "body div.site-container div.column div div.fulfilment-col.columns div.fulfilment div.fulfilment-content form button")
    argos.searchbox.postcode.button$clickElement()
    Sys.sleep(runif(1, 5, 7.5))
    page <- (fox$getPageSource())
    page.char <- as.character(page)
    test.1.loc <- regexpr("pdp-fulfilment-message-delivery", page.char)
    test.1.string <- substr(page.char, test.1.loc, test.1.loc + 310) 
    test.1.split <- strsplit(test.1.string, "<")
    test.1.split <- strsplit(as.character(test.1.split), ">")
    if (regexpr("Order now", test.1.split) != -1) 
    {
      
      part.1.1 <- substr(test.1.split[[1]][3], 1, regexpr('\"', test.1.split[[1]][3]) - 1)
      part.1.2 <- substr(test.1.split[[1]][4], 1, regexpr('\"', test.1.split[[1]][4]) - 1)
      part.1.3 <- substr(test.1.split[[1]][5], 1, regexpr('\"', test.1.split[[1]][5]) - 1)
      part.1.4 <- substr(test.1.split[[1]][6], 1, regexpr('\"', test.1.split[[1]][6]) - 1)
      part.1.5 <- substr(test.1.split[[1]][7], 1, regexpr('\"', test.1.split[[1]][7]) - 1)
      part.1.6 <- substr(test.1.split[[1]][8], 1, regexpr('\"', test.1.split[[1]][8]) - 1)
      part.1 <- paste(part.1.1, part.1.2, part.1.3, part.1.4, part.1.5, part.1.6, sep = "")
      part.2 <- substr(test.1.split[[1]][13], 1, regexpr('\"', test.1.split[[1]][13]) - 1)
      part.3 <- substr(test.1.split[[1]][15], 1, regexpr('\"', test.1.split[[1]][15]) - 1)
      argos.data[nrow(post.codes)*(i-1)+j, 10] <- part.1
      argos.data[nrow(post.codes)*(i-1)+j, 11] <- part.2
      argos.data[nrow(post.codes)*(i-1)+j, 12] <- part.3
      argos.data[nrow(post.codes)*(i-1)+j, 1]  <- product.details$Merchandise.Area[i]
      argos.data[nrow(post.codes)*(i-1)+j, 2]  <- product.details$Planning.Group[i]
      argos.data[nrow(post.codes)*(i-1)+j, 3]  <- product.details$Product[i]
      argos.data[nrow(post.codes)*(i-1)+j, 4]  <- product.details$Competitor[i]
      argos.data[nrow(post.codes)*(i-1)+j, 5]  <- product.details$Competitor.Product.Code[i]
      argos.data[nrow(post.codes)*(i-1)+j, 6]  <- product.details$URL[i]
      argos.data[nrow(post.codes)*(i-1)+j, 7]  <- product.details$Retail.Price.Excl.Delivery.2[i]
      argos.data[nrow(post.codes)*(i-1)+j, 8]  <- product.details$Sales.YTD[i]
      argos.data[nrow(post.codes)*(i-1)+j, 9]  <- post.codes$post_code[j]
      
    } else {
      
      part.1.1 <- substr(test.1.split[[1]][3], 1, regexpr('\"', test.1.split[[1]][3]) - 1)
      part.1.2 <- substr(test.1.split[[1]][4], 1, regexpr('\"', test.1.split[[1]][4]) - 1)
      part.1 <-paste(part.1.1, part.1.2, sep = "")
      argos.data[nrow(post.codes)*(i-1)+j, 10] <- part.1
      argos.data[nrow(post.codes)*(i-1)+j, 5]  <- product.details$Competitor.Product.Code[i]
      argos.data[nrow(post.codes)*(i-1)+j, 6]  <- product.details$URL[i]
      argos.data[nrow(post.codes)*(i-1)+j, 1]  <- product.details$Merchandise.Area[i]
      argos.data[nrow(post.codes)*(i-1)+j, 2]  <- product.details$Planning.Group[i]
      argos.data[nrow(post.codes)*(i-1)+j, 3]  <- product.details$Product[i]
      argos.data[nrow(post.codes)*(i-1)+j, 4]  <- product.details$Competitor[i]
      argos.data[nrow(post.codes)*(i-1)+j, 5]  <- product.details$Competitor.Product.Code[i]
      argos.data[nrow(post.codes)*(i-1)+j, 6]  <- product.details$URL[i]
      argos.data[nrow(post.codes)*(i-1)+j, 7]  <- product.details$Retail.Price.Excl.Delivery.2[i]
      argos.data[nrow(post.codes)*(i-1)+j, 8]  <- product.details$Sales.YTD[i]
      argos.data[nrow(post.codes)*(i-1)+j, 9]  <- post.codes$post_code[j]
      
    }
 
  }
  b = Sys.time()
  
}
    
write.csv(argos.data, "argos.data.196.pd.4.pc.21.Jun.csv")
