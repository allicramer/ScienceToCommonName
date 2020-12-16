install.packages("RSelenium")
install.packages("rvest")
install.packages("tidyverse")


library(RSelenium)
library(rvest)
library(tidyverse)



#function looks up species at SciName Finder (sciname.info)
  #uses firefox (this can be changed but it runs smoother this way)
    #if it isn't opening a browser, change the "port" call
  #if it is returning a lot of "Not Founds", the website needs more time to load results. Change the LoadTime call
  #Sometimes results come back with other options besides English. Normally the website only has English name, but if you want to search for another you can. If that language is listed in the first two hits it will return the common name associated with that language in that hit

SciNameLookup <- function(SName = "Gadus morhua", Language = "English", port = 4545L, LoadTime = 8) {
  #create web instance 
  rD <- rsDriver(browser="firefox", port=port, verbose=F)
  remDr <- rD[["client"]]
  
  #url where you are scaraping from 
  url <- "https://www.sciname.info/"
  
  
  #navigate to url 
  remDr$navigate(url)  
  
  
  #add query to searchbar
  SN <- SName
  remDr$findElement(using = "name", value = "SciName")$sendKeysToElement(list(SN))

  #submit search to searchbar
  remDr$findElements("css", "#page_content > form:nth-child(17) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(2) > input:nth-child(1)")[[1]]$clickElement()

  #saving the resulting page as an object we can scrape 
  Sys.sleep(LoadTime) # give the page time to fully load
  html <- remDr$getPageSource()[[1]]
  remDr$closeall()
  
  results <- read_html(html) %>% 
    html_nodes( css = "#page_content > table:nth-child(23), #page_content > table:nth-child(29)") %>% 
    html_table() 
  
  Lang = Language
  
  if(length(results) == 0) {
    CN1 <- "Not Found"
    CN2 <- "Not Found"
    } else if (length(results) == 1) {
    CN1 <- results[[1]] %>% filter(X2 == "Common Name:" | X2 == paste0(Lang,":")) %>% select(X3)
    CN2 <- ""
  } else {
    CN1 <- results[[1]] %>% filter(X2 == "Common Name:" | X2 == paste0(Lang,":")) %>% select(X3)
    CN2 <-  results[[2]]  %>% filter(X2 == "Common Name:" | X2 == paste0(Lang,":")) %>% select(X3)
  }

  CommonName <- c(as.character(CN1), as.character(CN2)) 

  CommonName
}


#example using one name 
  #it takes a WHILE, and is clunky
SciNameLookup(SName = "Anthopleura xanthogrammica")

#make big list going thorugh vectors 

vec <- c("Lutra Lutra", "Anthopleura xanthogrammica", "bob")

vec.commonnames <- lapply(vec, function (x) {SciNameLookup(x)})


