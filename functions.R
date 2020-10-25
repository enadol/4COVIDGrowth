#functions

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
    if(country=="Denmark"){
      lista <- simplifyCountry(denmarkDin, startDate, endDate)
      listaTotal <- deathsAllDenmark
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
    
    if(country=="Denmark"){
      lista <- simplifyCountry(denmarkin, startDate, endDate)
      j <- match(as.Date(startDate), fechas)
      if(startDate==fechas[1]){
        unoAntes <- 0
      }else{
        unoAntes <- simplifyCountry(denmarkin, fechas[j-1], fechas[j-1])$confirmed
      }
    }
  }
  
  for(day in days){
    i <- match(day, days)
    
    if(startDate == fechas[1]){
      if(i==1){
        lista$rates[i] <- 0    
      }
      else{
        lista$rates[i] <- (lista$confirmed[i]-lista$confirmed[i-1])/lista$confirmed[i-1] * 100
      }
      lista$rates <- lista$rates[1:length(lista$date)]
      
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
    if(country == "Denmark"){
      lista <- simplifyCountry(denmarkDin, startDate, endDate)
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
  
  
  if(country == "Denmark"){
    lista <- simplifyCountry(denmarkin, startDate, endDate)
    if(startDate != fechas[1]){
      unoAntes <- simplifyCountry(denmarkin, fechas[j-1], fechas[j-1])
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
    
    if(country == "Denmark"){
      lista <- simplifyCountryDeaths(denmarkDin, startDate, endDate)
      if(startDate != fechas[1]){
        unoAntes <- simplifyCountryDeaths(denmarkDin, fechas[j-1], fechas[j-1])
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


getAverages <- function(x, listaCasos, listaFechas){
  promedios <- c()
  i <- match(x, listaFechas)
  if(i == 1){
    promedios[i] <- listaCasos[i]/2
    #list.append(promedios, promedio)
  }
  
  else{
    promedios[i] <- mean(unlist(listaCasos[2:i]))
    #list.append(promedios, promedio)
  }
}

computeAverages <- function(country, startDate, endDate){
  promedios <- list()
  tabla <- computeDailyAbsoluteVariation(country, startDate, endDate)
  listaCasosDiarios <- tabla$daily
  listaDates <- tabla$date
  promedios$date <- listaDates
  promedios$average <- lapply(listaDates, getAverages, listaCasos = listaCasosDiarios, listaFechas= listaDates)
  return(promedios)
  
}

computeAveragesDeaths <- function(country, startDate, endDate){
  promedios <- list()
  tabla <- computeDailyDeathsVariation(country, startDate, endDate)
  listaCasosDiarios <- tabla$daily
  listaDates <- tabla$date
  promedios$date <- listaDates
  promedios$average <- lapply(listaDates, getAverages, listaCasos = listaCasosDiarios, listaFechas= listaDates)
  return(promedios)
  
}