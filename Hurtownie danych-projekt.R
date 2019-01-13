library(rvest)
library(dplyr)
library(stringr)
library(odbc)
library(dbi)

oferty_df <- data.frame(stringsAsFactors = F)

choices <- c("dolnoslaskie", "kujawsko-pomorskie", "lodzkie", "lubelskie", "lubuskie", "malopolskie", "mazowieckie", "opolskie", "podkarpackie", "podlaskie", "pomorskie","slaskie","œwiêtokrzyskie", "warmiñsko-mazurskie", "wielkopolskie", "zachodniopomorskie")

select.list(choices, preselect = NULL, multiple = FALSE,
            title = NULL, graphics = getOption("menu.graphics")) -> wybór


otodom = paste0("https://www.otodom.pl/wynajem/mieszkanie/", wybór ,"/?nrAdsPerPage=72&search%5Bpaidads_listing%5D=1&search%5Border%5D=created_at_first%3Adesc&page=")



for(i in 1:2) {
  
  #i <- 1
  page <- paste0(otodom, i)
  otodom_strona <- read_html(page)
  
  oferty <-otodom_strona %>%
    html_nodes("article.offer-item") 
  
  for (oferta in oferty) {
    
    #oferta <- oferty[1]
    
    nazwy <- oferta%>%
      
      html_node(".offer-item-title") %>%
      html_text()
    
    metraz <- oferta%>%
      
      html_node(".offer-item-area") %>%
      html_text()
    
    ceny <- oferta%>%
      
      html_node(".offer-item-price") %>%
      html_text()%>%
      gsub(" ", "", .) %>%
      gsub("z³/mc", "", .) %>%
      as.numeric()
    
    link <-oferta %>%
      html_node("h3 > a") %>%
      html_attr("href")
    
    
    #########             tutaj zaczynamy zbierac z kazdego liku po kolei
    
    #link <- "https://www.otodom.pl/oferta/mieszkanie-do-wynajecia-ul-chodkiewicza-ID3Rnqs.html#11b181c8a2"
    ogloszenie <- read_html(link)
    
    pietro <- ogloszenie %>%
      html_node("ul.main-list > li.param_floor_no > span > strong") %>%
      html_text()
    
    liczba_pokoi <- ogloszenie %>%
      html_node("ul.main-list > li:not([class]) > span > strong") %>%
      html_text()
    
    dodatkowo <- ogloszenie %>%
      html_node("ul.dotted-list") %>%
      html_text()
    
    dodatkowo <- str_replace_all(dodatkowo, "\n", ", ")
    
    parametry <- ogloszenie %>%
      html_nodes("ul.sub-list > li:not([class])")
    
    rok_budowy <- "brak danych"
    czynsz_dodatkowo <- "brak danych"
    kaucja <- "brak danych"
    rodzaj_zabudowy <- "brak danych"
    
    for (parametr in parametry) {
      
      item <- parametr %>%
        html_text()
      
      if(str_detect(tolower(item),"czynsz - dodatkowo:")) {
        label <- str_sub(item, start=0, end=str_locate(item,":")[1])
        czynsz_dodatkowo <- str_trim(str_sub(item, start=str_locate(item,":")[1]+1,end=str_length(item)))
      } 
      
      if(str_detect(tolower(item),"rok budowy:")) {
        label <- str_sub(item, start=0, end=str_locate(item,":")[1])
        rok_budowy <- str_trim(str_sub(item, start=str_locate(item,":")[1]+1,end=str_length(item)))
      } 
      
      if(str_detect(tolower(item),"kaucja:")) {
        label <- str_sub(item, start=0, end=str_locate(item,":")[1])
        kaucja <- str_trim(str_sub(item, start=str_locate(item,":")[1]+1,end=str_length(item)))
      } 
      
      if(str_detect(tolower(item),"rodzaj zabudowy:")) {
        label <- str_sub(item, start=0, end=str_locate(item,":")[1])
        rodzaj_zabudowy <- str_trim(str_sub(item, start=str_locate(item,":")[1]+1,end=str_length(item)))
      } 
       
    }
    
    
    oferty_df <- rbind(oferty_df,
                       data.frame(nazwy,metraz,ceny,pietro,liczba_pokoi,rodzaj_zabudowy,kaucja,czynsz_dodatkowo,rok_budowy,
                                  stringsAsFactors = F))
    
  }
}

write.csv(oferty_df, file = paste0("Oferty z dnia", " ", Sys.Date(), ".csv"))

conn <- DBI::dbConnect(odbc::odbc(),
                       Driver = "SQL Server",
                       Server = "MATEUSZ",
                       Database = "ProjektHD",
                       UID = rstudioapi::askForPassword("Username"),
                       PWD = rstudioapi::askForPassword("Password"),
                       Port = 1433)
dbWriteTable(conn, "tabela", oferty_df, overwrite=TRUE)
