#functions
getGrowthALL <- function(country, startDate, endDate){
  indexStart <- match(as.Date(startDate), fechas)
  indexEnd <- match(as.Date(endDate), fechas)
  countryList <- list()
  for(item in tablagrowth){
    if(item$country == country){
      index <- length(countryList$growth)+1
      countryList$growth[index] <-  item$confirmed
    }
  }
  return(unlist(countryList)[indexStart:indexEnd])
}

getDeathsALL <- function(country){
  countryList <- list()
  if(country %in% withProvinces){
    if(country == "Australia"){
      countryList <- simplifyCountry(australiaDin)
    }
    if(country == "Canada"){
      countryList <- simplifyCountry(canadaDin)
    }
    if(country=="France"){
      countryList <- simplifyCountry(franceDin)
    }
    if(country=="China"){
      countryList <- simplifyCountry(chinaDin)
    }
    if(country=="Netherlands"){
      countryList <- simplifyCountry(netherlandsDin)
    }
    if(country=="United Kingdom"){
      countryList <- simplifyCountry(ukDin)
    }
  }
  else{
    for(item in deaths$`Country/Region`){
      if(item$country == country){
        index <- length(countryList$deaths)+1
        countryList$deaths[index] <-  item$confirmed
      }
    }
  }
  return(unlist(countryList))
}



getDateAll <- function(){
  dateList <- c()
  for(item in tablagrowth){
    index <- length(dateList)+1
    dateList[index] <-  item$date
  }
  return(unique(dateList))
}

getCountryAll <- function(){
  countryList <- c()
  for(item in tablagrowth){
    index <- length(countryList)+1
    countryList[index] <-  item$country
  }
  return(unique(countryList))
}

setTotalDates <- function(item){
  item$date <- getDateAll()
}


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




computeRateswProvince <- function(country){
  if(country %in% withProvinces){
    if(country == "Australia"){
      lista <- simplifyCountry(australiain)
    }
    if(country=="Canada"){
      lista <- simplifyCountry(canadain)
    }
    if(country=="France"){
      lista <- simplifyCountry(francein)
    }
    if(country=="China"){
      lista <- simplifyCountry(chinain)
    }
    if(country=="Netherlands"){
      lista <- simplifyCountry(netherlandsin)
    }
    if(country=="United Kingdom"){
      lista <- simplifyCountry(ukin)
    }
    for(day in days){
      i <- match(day, days)
      if(i==1){
        lista$rates[1] <- 0
        
      }
      else{
        lista$rates[i] <- (lista$confirmed[i]-lista$confirmed[i-1])/lista$confirmed[i-1] * 100
      }  
    }
    return(lista)
  }
}

computeDeathGrowth <- function(country, startDate, endDate){
  toplot <- list()
  indexStart <- match(as.Date(startDate), fechas)
  indexEnd <- match(as.Date(endDate), fechas)
  lista <- list()
  
  if(country %in% withProvinces){
    if(country == "Australia"){
      lista <- simplifyCountry(australiaDin, startDate, endDate)
      listaTotal <- deathsAllAustralia
      }
    if(country=="Canada"){
      lista <- simplifyCountry(canadaDin, startDate, endDate)
      listaTotal <- deathsAllCanada
      }
    if(country=="France"){
      lista <- simplifyCountry(franceDin, startDate, endDate)
      listaTotal <- deathsAllFrance
      }
    if(country=="China"){
      lista <- simplifyCountry(chinaDin, startDate, endDate)
      listaTotal <- deathsAllChina
      }
    if(country=="Netherlands"){
      lista <- simplifyCountry(netherlandsDin, startDate, endDate)
      listaTotal <- deathsAllNetherlands
      }
    if(country=="United Kingdom"){
      lista <- simplifyCountry(ukDin, startDate, endDate)
      listaTotal <- deathsAllUK
    }
    
    for(day in lista$date){
        i <- match(day, lista$date)
        if(i == 1){
          if(startDate == "2020-01-22"){
          lista$dgrowth[i]== 0
          }
          else{
            f <- match(as.Date(day, format = "%m/%d/%y"), fechas)
            forOne <- listaTotal$confirmed[f-1]
            lista$dgrowth[i] <- (lista$confirmed[i]/forOne -1)* 100
          }
        }
        else{    
          lista$dgrowth[i] <- (lista$confirmed[i]-lista$confirmed[i-1])/lista$confirmed[i-1] * 100
          
        }  
      }
      return(lista)
  }
  
  else{
    lista <- simplifyCountryNormal(country, startDate, endDate)
  }
  
  for(day in lista$date){
    i <- match(day, lista$date)
    
    if(i == 1){
      if(startDate == "2020-01-22"){
      lista$dgrowth[i] <- 0
      }
      else{
        f <- match(country, deaths$`Country/Region`)
        h <- match(day, days)
        forOne <- deaths[[days[h-1]]][f]
        lista$dgrowth[i] <- (lista$deaths[i]/forOne * 100)-100
      }
    }
else{    
    lista$dgrowth[i] <- (lista$deaths[i]-lista$deaths[i-1])/lista$deaths[i-1] * 100
    
    }  
  }
  return(lista)
   
}

computeDGrowth <- function(country, startDate, endDate){
  toplot <- list()
  indexStart <- match(as.Date(startDate), fechas)
  indexEnd <- match(as.Date(endDate), fechas)
  lista <- list()
  
  if(country %in% withProvinces){
    if(country == "Australia"){
      lista <- simplifyCountryDeaths(australiaDin, startDate, endDate)
      
    }
    if(country=="Canada"){
      lista <- simplifyCountryDeaths(canadaDin, startDate, endDate)
      
    }
    if(country=="France"){
      lista <- simplifyCountryDeaths(franceDin, startDate, endDate)
      
    }
    if(country=="China"){
      lista <- simplifyCountryDeaths(chinaDin, startDate, endDate)
      
    }
    if(country=="Netherlands"){
      lista <- simplifyCountryDeaths(netherlandsDin, startDate, endDate)
      
    }
    if(country=="United Kingdom"){
      lista <- simplifyCountryDeaths(ukDin, startDate, endDate)
      
    }
  }
  else{
    lista <- simplifyCountryNormal(country, startDate, endDate)
  }
  
for(i in 1:length(lista$deaths)){
  if(i==1){
  lista$dgrowth[i] <- 0
}
  else{
  lista$dgrowth[i] <- (lista$deaths[i]-lista$deaths[i-1])/lista$deaths[i-1] * 100
}
}
  return(lista)
  
}

setPlot <- function(country, startDate, endDate){
  indexStart <- match(as.Date(startDate), fechas)
  indexEnd <- match(as.Date(endDate), fechas)
  tabla <- NULL
  tabla$growth <- getGrowthALL(country, startDate, endDate)
  tabla$date <- days[indexStart:indexEnd]
  dates <- as.Date(tabla$date, format= "%m/%d/%y")
  
  p <- plot_ly(as.data.frame(tabla))%>%add_lines(x=dates, y=tabla$growth)%>% layout(title=paste(country, "coronavirus cases daily growth"), xaxis=list(showticklabels=TRUE))
  return(p)
}
}

setPlotDeaths <- function(country, startDate, endDate){
  indexStart <- match(as.Date(startDate), fechas)
  indexEnd <- match(as.Date(endDate), fechas)
  toplot <- list()
  
  if(country %in% withProvinces){
    if(country == "Australia"){
      tabla <- simplifyCountry(australiaDin, startDate, endDate)
    }
    if(country=="Canada"){
      tabla <- simplifyCountry(canadaDin, startDate, endDate)
    }
    if(country=="France"){
      tabla <- simplifyCountry(franceDin, startDate, endDate)
    }
    if(country=="China"){
      tabla <- simplifyCountry(chinaDin, startDate, endDate)
    }
    if(country=="Netherlands"){
      tabla <- simplifyCountry(netherlandsDin, startDate, endDate)
    }
    if(country=="United Kingdom"){
      tabla <- simplifyCountry(ukDin, startDate, endDate)
    }
    dates <- as.Date(tabla$date, format= "%m/%d/%y")
    p <- plot_ly(as.data.frame(tabla))%>%add_lines(x=dates, y=~confirmed)%>% layout(title=paste(country, "coronavirus deaths"), xaxis=list(showticklabels=TRUE))
    return(p)
  }
  else{
  tabla <- simplifyCountryNormal(country, startDate, endDate)
  
  dates <- as.Date(tabla$date, format= "%m/%d/%y")
  
  
  p <- plot_ly(as.data.frame(tabla))%>%add_lines(x=dates, y=tabla$deaths)%>% layout(title=paste(country, "coronavirus deaths"), xaxis=list(showticklabels=TRUE))
  return(p)
}
}

setPlotDeathsGrowth <- function(country, startDate, endDate){
  indexStart <- match(as.Date(startDate), fechas)
  indexEnd <- match(as.Date(endDate), fechas)
  toplot <- list()

    if(country %in% withProvinces){
    if(country == "Australia"){
      tabla <- simplifyCountry(australiaDin, startDate, endDate)
    }
    if(country=="Canada"){
      tabla <- simplifyCountry(canadaDin, startDate, endDate)
    }
    if(country=="France"){
      tabla <- simplifyCountry(franceDin, startDate, endDate)
    }
    if(country=="China"){
      tabla <- simplifyCountry(chinaDin, startDate, endDate)
    }
    if(country=="Netherlands"){
      tabla <- simplifyCountry(netherlandsDin, startDate, endDate)
    }
    if(country=="United Kingdom"){
      tabla <- simplifyCountry(ukDin, startDate, endDate)
    }  
      dates <- as.Date(tabla$date, format= "%m/%d/%y")
      toplot$dgrowth <- tabla$confirmed[indexStart:indexEnd]
      p <- plot_ly(as.data.frame(tabla))%>%add_lines(x=dates, y=~dgrowth)%>% layout(title=paste(country, "coronavirus death % growth"), xaxis=list(showticklabels=TRUE))
      return(p)
    }
  else{
    tabla <- simplifyCountryNormal(country, startDate, endDate)
    
    dates <- as.Date(tabla$date, format= "%m/%d/%y")
    
    
    p <- plot_ly(as.data.frame(tabla))%>%add_lines(x=dates, y=tabla$deaths)%>% layout(title=paste(country, "coronavirus deaths"), xaxis=list(showticklabels=TRUE))
    return(p)
  }
}




sumaCD <- function(country){
  listCD <- c()
  
  for(i in 1:length(confirmed$Country.Region)){
    if(confirmed$Country.Region[i]==country){
        element <- confirmed[ncol(confirmed)][i,]
        listCD$Confirmed[length(listCD$Confirmed)+1] <- element
            }
  }
  
  return(sum(listCD$Confirmed))
}

sumaCDAyer <- function(country){
  listCDAyer <- c()
  
  for(i in 1:length(confirmed$Country.Region)){
    if(confirmed$Country.Region[i]==country){
      element <- confirmed[ncol(confirmed)-1][i,]
      listCDAyer$Confirmed[length(listCDAyer$Confirmed)+1] <- element
    }
  }
  
  injectwProvinces <- function(country){
    
    
  }
  
  return(sum(listCDAyer$Confirmed))
}

computeWProvinces <- function(country){
  hoy <- sumaCD(country)
  ayer <- sumaCDAyer(country)
  diferencia <- hoy - ayer
  percent <- diferencia /ayer * 100
  return(percent)
  
}

computeRateswProvince <- function(country, startDate, endDate){
  
  if(country %in% withProvinces){
    if(country == "Australia"){
      lista <- simplifyCountry(australiain, startDate, endDate)
      j <- match(as.Date(startDate), fechas)
      if(startDate==fechas[1]){
        unoAntes <- 0
      }else{
        unoAntes <- simplifyCountry(australiain, fechas[j-1], fechas[j-1])$confirmed
      }
    }
    
    if(country=="Canada"){
      lista <- simplifyCountry(canadain, startDate, endDate)
      j <- match(as.Date(startDate), fechas)
      if(startDate==fechas[1]){
        unoAntes <- 0
      }else{
        unoAntes <- simplifyCountry(canadain, fechas[j-1], fechas[j-1])$confirmed
      }
    }
    
    if(country=="France"){
      lista <- simplifyCountry(francein, startDate, endDate)
      j <- match(as.Date(startDate), fechas)
      if(startDate==fechas[1]){
        unoAntes <- 0
      }else{
        unoAntes <- simplifyCountry(francein, fechas[j-1], fechas[j-1])$confirmed
      }
    }
    
    if(country=="China"){
      lista <- simplifyCountry(chinain, startDate, endDate)
      j <- match(as.Date(startDate), fechas)
      if(startDate==fechas[1]){
        unoAntes <- 0
      }else{
        unoAntes <- simplifyCountry(chinain, fechas[j-1], fechas[j-1])$confirmed
      }
    }
    
    if(country=="Netherlands"){
      lista <- simplifyCountry(netherlandsin, startDate, endDate)
      j <- match(as.Date(startDate), fechas)
      if(startDate==fechas[1]){
        unoAntes <- 0
      }else{
        unoAntes <- simplifyCountry(netherlandsin, fechas[j-1], fechas[j-1])$confirmed
      }
    }
    
    if(country=="United Kingdom"){
      lista <- simplifyCountry(ukin, startDate, endDate)
      j <- match(as.Date(startDate), fechas)
      if(startDate==fechas[1]){
        unoAntes <- 0
      }else{
        unoAntes <- simplifyCountry(ukin, fechas[j-1], fechas[j-1])$confirmed
      }
    }
  }
  for(day in days){
    i <- match(day, days)
    
    if(startDate == fechas[1]){
      lista$rates[1] <- 0
      
    }
    
    else{
      if(i==1){
        lista$rates[i] <- (lista$confirmed[1]- unoAntes)/unoAntes * 100
      }
      else{
        lista$rates[i] <- (lista$confirmed[i]-lista$confirmed[i-1])/lista$confirmed[i-1] * 100
        lista$rates <- lista$rates[1:length(lista$date)]
      }  
    }
  }
  return(lista)
}

computeDeathGrowth <- function(country, startDate, endDate){
  lista <- list()
  
  if(country %in% withProvinces){
    if(country == "Australia"){
      lista <- simplifyCountry(australiaDin, startDate, endDate)
    }
    if(country=="Canada"){
      lista <- simplifyCountry(canadaDin, startDate, endDate)
    }
    if(country=="France"){
      lista <- simplifyCountry(franceDin, startDate, endDate)
    }
    if(country=="China"){
      lista <- simplifyCountry(chinaDin, startDate, endDate)
    }
    if(country=="Netherlands"){
      lista <- simplifyCountry(netherlandsDin, startDate, endDate)
    }
    if(country=="United Kingdom"){
      lista <- simplifyCountry(ukDin, startDate, endDate)
    }
    for(day in days){
      i <- match(day, days)
      if(i==1){
        lista$dgrowth[1] <- 0
        
      }
      else{
        lista$dgrowth[i] <- (lista$confirmed[i]-lista$confirmed[i-1])/lista$confirmed[i-1] * 100
      }  
    }
    return(lista)
  }
  
  else{
    lista <- simplifyCountryNormal(country, startDate, endDate)
  }
  
  for(day in days){
    i <- match(day, days)
    if(i==1){
      lista$dgrowth[1] <- 0
      
    }
    else{
      lista$dgrowth[i] <- (lista$deaths[i]-lista$deaths[i-1])/lista$deaths[i-1] * 100
    }  
  }
  
  return(lista)
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
    countryout$Lat[i] <- countryin$Lat
    countryout$Long[i] <- countryin$Long
    countryout$country[i] <- as.character(countryin$`Country/Region`[[1]])
    countryout$date[i] <- days[i]
    countryout$confirmed[i]<-unlist(countryin[days[i]]) 
    
    
    
    }
    toplot$state <- countryout$state[indexStart:indexEnd]
    toplot$country <- countryout$country[indexStart:indexEnd]
    toplot$date <- countryout$date[indexStart:indexEnd]
    toplot$confirmed <- countryout$confirmed[indexStart:indexEnd]
  
  return(toplot)
}

setPlotConfirmed <- function(country, startDate, endDate){
  
  tabla <- simplifyCountryCases(country, startDate, endDate)
  dates <- as.Date(tabla$date, format= "%m/%d/%y")
  
  p <- plot_ly(as.data.frame(tabla))%>%add_lines(x=dates, y=tabla$confirmed)%>% layout(title=paste(country, "coronavirus confirmed cases"), xaxis=list(showticklabels=TRUE))
  return(p)
}

setPlotActive <- function(country, stardDate, endDate){
  
  tablaRecovered <- simplifyCountryNormalRecovered(country, stardDate, endDate)
  active <- data.frame(dates, tablaRecovered$confirmed, tablaRecovered$recovered)
  colnames(active) <-  c("dates", "confirmed", "recovered")
  active$active <- active$confirmed-active$recovered
  active$average <- rowMeans(active[,c("confirmed", "recovered")])
  
  hovertexta <- paste("Date: ", active$date, "<br>Active cases: ", active$active)                
  
  p <- plot_ly(as.data.frame(tablaRecovered))%>%add_lines(x=dates, y=tablaRecovered$recovered)%>% layout(title=paste(country, "coronavirus confirmed cases"), xaxis=list(showticklabels=TRUE))
  return(p)
  
  
  
  
}

computeDeathswProvince <- function(country, startDate, endDate){
  lista <- list()
  if(country %in% withProvinces){
    if(country == "Australia"){
      lista <- simplifyCountry(australiaDin, startDate, endDate)
    }
    if(country=="Canada"){
      lista <- simplifyCountry(canadaDin, startDate, endDate)
    }
    if(country=="France"){
      lista <- simplifyCountry(franceDin, startDate, endDate)
    }
    if(country=="China"){
      lista <- simplifyCountry(chinaDin, startDate, endDate)
    }
    if(country=="Netherlands"){
      lista <- simplifyCountry(netherlandsDin, startDate, endDate)
    }
    if(country == "United Kingdom"){
      lista <- simplifyCountry(ukDin, startDate, endDate)
    }
    return(lista)
  }
  else{
    
    for(day in days){
      
      i <- match(country, deaths$`Country/Region`)
      lista[length(lista)+1] <- deaths[i,][day]
      
    }
    return(lista)    
  }
  
}

computeDailyAbsoluteVariation <- function(country, startDate, endDate){
  lista <- list()
  unoAntes <- list()
  j <- match(as.Date(startDate), fechas)
  if(country %in% withProvinces){
    if(country == "Australia"){
      lista <- simplifyCountry(australiain, startDate, endDate)
      if(startDate != fechas[1]){
      unoAntes <- simplifyCountry(australiain, fechas[j-1], fechas[j-1])
      }
    }
    
    if(country=="Canada"){
      lista <- simplifyCountry(canadain, startDate, endDate)
      if(startDate != fechas[1]){
      unoAntes <- simplifyCountry(canadain, fechas[j-1], fechas[j-1])
      }
    }
    
    if(country=="France"){
      lista <- simplifyCountry(francein, startDate, endDate)
      if(startDate != fechas[1]){
        unoAntes <- simplifyCountry(francein, fechas[j-1], fechas[j-1])
      }
    }
    
    if(country=="China"){
      lista <- simplifyCountry(chinain, startDate, endDate)
      if(startDate != fechas[1]){
      unoAntes <- simplifyCountry(chinain, fechas[j-1], fechas[j-1])
      }
    }
    
    if(country=="Netherlands"){
      lista <- simplifyCountry(netherlandsin, startDate, endDate)
      if(startDate != fechas[1]){
       unoAntes <- simplifyCountry(netherlandsin, fechas[j-1], fechas[j-1])
      }
    }
    
    if(country == "United Kingdom"){
      lista <- simplifyCountry(ukin, startDate, endDate)
      if(startDate != fechas[1]){
      unoAntes <- simplifyCountry(ukin, fechas[j-1], fechas[j-1])
      }
    }
  }
  else{
    
    lista <- simplifyCountryCases(country, startDate, endDate)
    if(startDate !=fechas[1]){
    unoAntes <- simplifyCountryCases(country, fechas[j-1], fechas[j-1])    
  }
  }
  
  for(j in 1:length(lista$confirmed)){
    if(j==1){
      if(startDate==fechas[1]){
        lista$daily[j] <- 0
      }
      else{
      lista$daily[j] <- lista$confirmed[1]-unoAntes$confirmed
      }
    }
    else{
      lista$daily[j] <- lista$confirmed[j]-lista$confirmed[j-1]
    } 
    
  }
  return(lista)
}


computeDailyDeathsVariation <- function(country, startDate, endDate){
  lista <- list()
  unoAntes <- list()
  j <- match(as.Date(startDate), fechas)
  if(country %in% withProvinces){
    if(country == "Australia"){
      lista <- simplifyCountryDeaths(australiaDin, startDate, endDate)
      if(startDate != fechas[1]){
        unoAntes <- simplifyCountryDeaths(australiaDin, fechas[j-1], fechas[j-1])
      }
      
      }
    if(country=="Canada"){
      lista <- simplifyCountryDeaths(canadaDin, startDate, endDate)
      if(startDate != fechas[1]){
        unoAntes <- simplifyCountryDeaths(canadaDin, fechas[j-1], fechas[j-1])
      }
      
      }
    if(country=="France"){
      lista <- simplifyCountryDeaths(franceDin, startDate, endDate)
      if(startDate != fechas[1]){
        unoAntes <- simplifyCountryDeaths(franceDin, fechas[j-1], fechas[j-1])
      }
      
      }
    if(country=="China"){
      lista <- simplifyCountryDeaths(chinaDin, startDate, endDate)
      if(startDate != fechas[1]){
        unoAntes <- simplifyCountryDeaths(chinaDin, fechas[j-1], fechas[j-1])
      }
      
      }
    if(country=="Netherlands"){
      lista <- simplifyCountryDeaths(netherlandsDin, startDate, endDate)
      if(startDate != fechas[1]){
        unoAntes <- simplifyCountryDeaths(netherlandsDin, fechas[j-1], fechas[j-1])
      }
      
    }
    
    if(country == "United Kingdom"){
      lista <- simplifyCountryDeaths(ukDin, startDate, endDate)
      if(startDate != fechas[1]){
        unoAntes <- simplifyCountryDeaths(ukDin, fechas[j-1], fechas[j-1])
      }
      
      }
  }
  else{
    lista <- simplifyCountryNormal(country, startDate, endDate)
    if(startDate != fechas[1]){
      unoAntes <- simplifyCountryDeaths(ukDin, fechas[j-1], fechas[j-1])
    }
    }
  
  for(j in 1:length(lista$deaths)){
    if(j==1){
      if(startDate==fechas[1]){
        lista$daily[j] <- 0
      }
      else{
      lista$daily[j] <- lista$deaths[1]-unoAntes$confirmed
      }
    }
    else{
      lista$daily[j] <- lista$deaths[j]-lista$deaths[j-1]
    } 
    
  }
  return(lista)
}




setPlotDailyCases <- function(country, startDate, endDate){
  indexStart <- match(as.Date(startDate), fechas)
  indexEnd <- match(as.Date(endDate), fechas)
  tabla <- computeDailyAbsoluteVariation(country, startDate, endDate)
  dates <- as.Date(tabla$date, format= "%m/%d/%y")
  
  hovertextd <- paste("Date: ", tabla$date,"<br>Confirmed cases reported :", tabla$daily)        
  p <- plot_ly(as.data.frame(tabla), type = "scatter", mode="markers+lines", marker=list(color="orange"),showlegend=FALSE)%>% layout(title=list(text=paste(country, "coronavirus daily cases reported"), yanchor="top", pad=list(t=15)), xaxis=list(showticklabels=TRUE))%>%config(displayModeBar=FALSE)%>%add_trace(x=dates, y=tabla$daily, text=hovertextd, hoverinfo="text", line=list(color="green", shape="spline"))
  p
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

getAverages <- function(x, lista){
  promedios <- c()
  i <- match(x, lista)
  if(i == 1){
    promedio <- 0
    list.append(promedios, promedio)
  }
  
  else{
  promedio <- mean(lista[2:i])
  list.append(promedios, promedio)
  }
  
  }
 
getCountryType <- function(country){
  if(country %in% withProvinces){
    countryType <- "with"
  }
  else{
    countryType <- "without"
  }
  return(countryType)
  
}

computeAverages <- function(country, startDate, endDate){
  tabla <- computeDailyAbsoluteVariation(country, startDate, endDate)
  lista <- tabla$daily
  promedios$date <- tabla$date
  promedios$average <- lapply(lista, getAverages, lista = lista)
  return(promedios)
  
}