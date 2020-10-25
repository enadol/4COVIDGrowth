simplifyCountry <- function(countryin, startDate, endDate){
  countryout <- list()
  toplot <- list()
  indexStart <- match(as.Date(startDate), fechas)
  indexEnd <- match(as.Date(endDate), fechas)
  
  for(i in 1:length(days)){
    element <- list()
    countryout$state[i] <- ""
    countryout$country[i] <- as.character(countryin$`Country/Region`[[1]])
    countryout$date[i] <- days[i]
    countryout$confirmed[i]<-sum(countryin[days[i]]) 
    
  }
  
  toplot$state <- countryout$state[indexStart:indexEnd]
  toplot$country <- countryout$country[indexStart:indexEnd]
  toplot$date <- countryout$date[indexStart:indexEnd]
  toplot$confirmed <- countryout$confirmed[indexStart:indexEnd]
  
  return(toplot)
}


simplifyCountryCases <- function(country, startDate, endDate){
  countryin <- filter(confirmed, `Country/Region` == country)
  countryout <- list()
  toplot <- list()
  indexStart <- match(as.Date(startDate), fechas)
  indexEnd <- match(as.Date(endDate), fechas)
  
  for(i in 1:length(days)){
    element <- list()
    countryout$state[i] <- ""
    countryout$country[i] <- as.character(countryin$`Country/Region`[[1]])
    countryout$date[i] <- days[i]
    countryout$confirmed[i]<-sum(countryin[days[i]]) 
    
  }
  toplot$state <- countryout$state[indexStart:indexEnd]
  toplot$country <- countryout$country[indexStart:indexEnd]
  toplot$date <- countryout$date[indexStart:indexEnd]
  toplot$confirmed <- countryout$confirmed[indexStart:indexEnd]
  
  return(toplot)
}

simplifyCountryRecovered <- function(country, startDate, endDate){
  countryout <- list()
  toplot <- list()
  indexStart <- match(as.Date(startDate), fechas)
  indexEnd <- match(as.Date(endDate), fechas)
  
  if(country %in% withProvinces){
    if(country == "Australia"){
      countryin <- australiaRin
    }
    if(country=="Canada"){
      countryin <- canadaRin
    }
    if(country=="France"){
      countryin <- franceRin
    }
    if(country=="China"){
      countryin <- chinaRin
    }
    if(country=="Netherlands"){
      countryin <- netherlandsRin
    }
    if(country=="United Kingdom"){
      countryin <- ukRin
    }
    if(country=="Denmark"){
      countryin <- denmarkRin
    }
  }
  
  
  for(i in 1:length(days)){
    element <- list()
    countryout$state[i] <- ""
    countryout$country[i] <- as.character(countryin$`Country/Region`[[1]])
    countryout$date[i] <- days[i]
    countryout$recovered[i]<-sum(countryin[days[i]]) 
    
  }
  
  toplot$state <- countryout$state[indexStart:indexEnd]
  toplot$country <- countryout$country[indexStart:indexEnd]
  toplot$date <- countryout$date[indexStart:indexEnd]
  toplot$recovered <- countryout$recovered[indexStart:indexEnd]
  
  return(toplot)
}

simplifyCountryNormalRecovered <- function(country, startDate, endDate){
  
  countryin <- filter(recovered, `Country/Region` == country)
  countryout <- list()
  toplot <- list()
  indexStart <- match(as.Date(startDate), fechas)
  indexEnd <- match(as.Date(endDate), fechas)
  
  
  for(i in 1:length(days)){
    element <- list()
    countryout$state[i] <- ""
    countryout$country[i] <- as.character(countryin$`Country/Region`[[1]])
    countryout$date[i] <- days[i]
    countryout$recovered[i]<-unlist(countryin[days[i]]) 
    
  }
  toplot$state <- countryout$state[indexStart:indexEnd]
  toplot$country <- countryout$country[indexStart:indexEnd]
  toplot$date <- countryout$date[indexStart:indexEnd]
  toplot$recovered <- countryout$recovered[indexStart:indexEnd]
  
  return(toplot)
}



simplifyCountryNormal <- function(country, startDate, endDate){
  
  countryin <- filter(deaths, `Country/Region` == country)
  countryout <- list()
  toplot <- list()
  indexStart <- match(as.Date(startDate), fechas)
  indexEnd <- match(as.Date(endDate), fechas)
  
  
  for(i in 1:length(days)){
    element <- list()
    countryout$state[i] <- ""
    countryout$country[i] <- as.character(countryin$`Country/Region`[[1]])
    countryout$date[i] <- days[i]
    countryout$deaths[i]<-unlist(countryin[days[i]]) 
    
  }
  toplot$state <- countryout$state[indexStart:indexEnd]
  toplot$country <- countryout$country[indexStart:indexEnd]
  toplot$date <- countryout$date[indexStart:indexEnd]
  toplot$deaths <- countryout$deaths[indexStart:indexEnd]
  
  return(toplot)
}

simplifyCountryMuertes <- function(country, startDate, endDate){
  countryin <- filter(deaths, `Country/Region` == country)
  
  countryout <- list()
  toplot <- list()
  indexStart <- match(as.Date(startDate), fechas)
  indexEnd <- match(as.Date(endDate), fechas)
  
  if(country %in% withProvinces){
    if(country == "Australia"){
      countryin <- australiaDin
    }
    if(country=="Canada"){
      countryin <- canadaDin
    }
    if(country=="France"){
      countryin <- franceDin
    }
    if(country=="China"){
      countryin <- chinaDin
    }
    if(country=="Netherlands"){
      countryin <- netherlandsDin
    }
    if(country=="United Kingdom"){
      countryin <- ukDin
    }
  }
  
  
  for(i in 1:length(days)){
    element <- list()
    countryout$state[i] <- ""
    countryout$country[i] <- as.character(countryin$`Country/Region`[[1]])
    countryout$date[i] <- days[i]
    countryout$deaths[i]<-sum(countryin[days[i]]) 
    
  }
  
  toplot$state <- countryout$state[indexStart:indexEnd]
  toplot$country <- countryout$country[indexStart:indexEnd]
  toplot$date <- countryout$date[indexStart:indexEnd]
  toplot$deaths <- countryout$deaths[indexStart:indexEnd]
  
  return(toplot)
}


simplifyCountryDeaths <- function(countryin, startDate, endDate){
  countryout <- list()
  toplot <- list()
  indexStart <- match(as.Date(startDate), fechas)
  indexEnd <- match(as.Date(endDate), fechas)
  
  for(i in 1:length(days)){
    element <- list()
    countryout$state[i] <- ""
    countryout$country[i] <- as.character(countryin$`Country/Region`[[1]])
    countryout$date[i] <- days[i]
    countryout$confirmed[i]<-sum(countryin[days[i]]) 
    
  }
  
  toplot$state <- countryout$state[indexStart:indexEnd]
  toplot$country <- countryout$country[indexStart:indexEnd]
  toplot$date <- countryout$date[indexStart:indexEnd]
  toplot$deaths <- countryout$confirmed[indexStart:indexEnd]
  
  return(toplot)
}
