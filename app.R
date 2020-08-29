library(shiny)
library(plotly)
library(readr)
library(jsonlite)
library(plotly)
library(dplyr)
library(rlist)
library(shinythemes)



#Load Data
confirmed <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", check.names = FALSE)
deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", check.names = FALSE)
recovered <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv", check.names = FALSE)


tablaconfirmed <- read_csv("https://raw.githubusercontent.com/enadol/covid/master/confirmedrates.csv")

tabladeaths <- read_csv("https://raw.githubusercontent.com/enadol/covid/master/deathrates.csv")

tablagrowth <- jsonlite::read_json("https://raw.githubusercontent.com/enadol/covid/master/input.json")

withProvinces <- c("Australia","Canada", "China", "France", "Netherlands", "United Kingdom")

#cases
francein <- filter(confirmed, `Country/Region` == "France")
chinain <- filter(confirmed, `Country/Region` == "China")
australiain <- filter(confirmed, `Country/Region` == "Australia")
canadain <- filter(confirmed, `Country/Region` == "Canada")
netherlandsin <- filter(confirmed, `Country/Region` == "Netherlands")
ukin <- filter(confirmed, `Country/Region` == "United Kingdom")

days <- colnames(confirmed[1,][5:ncol(confirmed)])
fechas <- as.Date(days, format = "%m/%d/%y")

#deaths
franceDin <- filter(deaths, `Country/Region` == "France")
chinaDin <- filter(deaths, `Country/Region` == "China")
australiaDin <- filter(deaths, `Country/Region` == "Australia")
canadaDin <- filter(deaths, `Country/Region` == "Canada")
netherlandsDin <- filter(deaths, `Country/Region` == "Netherlands")
ukDin <- filter(deaths, `Country/Region` == "United Kingdom")

#recovered
franceRin <- filter(recovered, `Country/Region` == "France")
chinaRin <- filter(recovered, `Country/Region` == "China")
australiaRin <- filter(recovered, `Country/Region` == "Australia")
canadaRin <- filter(recovered, `Country/Region` == "Canada")
netherlandsRin <- filter(recovered, `Country/Region` == "Netherlands")
ukRin <- filter(recovered, `Country/Region` == "United Kingdom")

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
        hovertextd <- paste("Date: ", tabla$date, "<br>Confirmed deaths (absolute): ", tabla$confirmed)
        
        p <- plot_ly(as.data.frame(tabla), type = "scatter", mode="markers+lines", marker=list(color="orange"),showlegend=FALSE)%>%config(displayModeBar=FALSE)%>%add_trace(x=dates, y=~confirmed, text= hovertextd, hoverinfo="text", line=list(color="green", shape="spline"))%>% layout(title=paste(country, "coronavirus deaths"), xaxis=list(showticklabels=TRUE), margin=m)
        p
    }
    else{
        tabla <- simplifyCountryNormal(country, startDate, endDate)
        hovertextd <- paste("Date: ", tabla$date, "<br>Confirmed deaths (absolute): ", tabla$deaths)        
        dates <- as.Date(tabla$date, format= "%m/%d/%y")
        
        
        p <- plot_ly(as.data.frame(tabla), type = "scatter", mode="markers+lines", marker=list(color="orange"),showlegend=FALSE)%>%config(displayModeBar=FALSE)%>%add_trace(x=dates, y=tabla$deaths, text= hovertextd, hoverinfo="text", line=list(color="green", shape="spline"))%>% layout(title=paste(country, "coronavirus deaths"), xaxis=list(showticklabels=TRUE), margin=m)
        p
    }
}

setPlotDeathsGrowth <- function(country, startDate, endDate){
    tabla <- computeDeathGrowth(country, startDate, endDate)
    dates <- as.Date(tabla$date, format= "%m/%d/%y")
    hovertextdgrowth <- paste("Date: ", tabla$date, "<br>Deaths growth: ", format(tabla$dgrowth, digits = 3), " % ")
    
    p <- plot_ly(as.data.frame(tabla), type = "scatter", mode="markers+lines", marker=list(color="orange"),showlegend=FALSE)%>%config(displayModeBar=FALSE)%>%add_trace(x=dates, y=tabla$dgrowth, text= hovertextdgrowth, hoverinfo= "text", line=list(color="green", shape="spline"))%>% layout(title=paste(country, "coronavirus deaths daily % growth"), xaxis=list(showticklabels=TRUE), margin=m)
    p
    
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



setPlotConfirmed <- function(country, startDate, endDate){
    
    tabla <- simplifyCountryCases(country, startDate, endDate)
    hovertextc <- paste("Date: ", tabla$date, "<br>Confirmed cases (absolute): ", tabla$confirmed)
    dates <- as.Date(tabla$date, format= "%m/%d/%y")
    
    p <- plot_ly(as.data.frame(tabla), type = "scatter", mode="markers+lines", marker=list(color="orange"),showlegend=TRUE)%>% layout(title=paste(country, "coronavirus confirmed cases, recovered and active cases *"), xaxis=list(showticklabels=TRUE), margin=m)%>%config(displayModeBar=FALSE)%>%add_trace(x=dates, y=tabla$confirmed, name="Confirmed cases", text= hovertextc, hoverinfo="text", line=list(color="green", shape="spline"))
    p
}

deathsAllFrance <- computeDeathswProvince("France", fechas[1], fechas[length(fechas)])
deathsAllChina <- computeDeathswProvince("China", fechas[1], fechas[length(fechas)])
deathsAllAustralia <- computeDeathswProvince("Australia", fechas[1], fechas[length(fechas)])
deathsAllCanada <- computeDeathswProvince("Canada", fechas[1], fechas[length(fechas)])
deathsAllNetherlands <- computeDeathswProvince("Netherlands", fechas[1], fechas[length(fechas)])
deathsAllUK <- computeDeathswProvince("United Kingdom", fechas[1], fechas[length(fechas)])

m <- list(t=50, pad=5)

#No scientific notation
options(scipen= 999)
options(digits= 3)


#plot example
#ptl <- plot_ly(as.data.frame(italy)) %>% add_lines(x=~date, y=~growth)%>%layout(xaxis=list(showticklabels=FALSE))
setPlot <- function(country, startDate, endDate){
    indexStart <- match(as.Date(startDate), fechas)
    indexEnd <- match(as.Date(endDate), fechas)
    tabla <- NULL
    tabla$growth <- getGrowthALL(country, startDate, endDate)
    tabla$date <- days[indexStart:indexEnd]
    dates <- as.Date(tabla$date, format= "%m/%d/%y")
    
    hovertextcgrowth <- paste("Date: ", tabla$date,"<br>Confirmed cases growth :", format(tabla$growth, nsmall = 3), " % ")        
    p <- plot_ly(as.data.frame(tabla), type = "scatter", mode="markers+lines", marker=list(color="orange"),showlegend=FALSE)%>% layout(title=paste(country, "coronavirus cases daily % growth"), xaxis=list(showticklabels=TRUE), margin=m)%>%config(displayModeBar=FALSE)%>%add_trace(x=dates, y=tabla$growth, text=hovertextcgrowth, hoverinfo="text", line=list(color="green", shape="spline"))
    p
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

setPlotDailyCases <- function(country, startDate, endDate){
    indexStart <- match(as.Date(startDate), fechas)
    indexEnd <- match(as.Date(endDate), fechas)
    tabla <- computeDailyAbsoluteVariation(country, startDate, endDate)
    dates <- as.Date(tabla$date, format= "%m/%d/%y")
    
    hovertextd <- paste("Date: ", tabla$date,"<br>Confirmed cases reported :", tabla$daily)        
    p <- plot_ly(as.data.frame(tabla), type = "scatter", mode="markers+lines", marker=list(color="orange"),showlegend=FALSE)%>% layout(title=paste(country, "coronavirus daily cases reported"), xaxis=list(showticklabels=TRUE), margin=m)%>%config(displayModeBar=FALSE)%>%add_trace(x=dates, y=tabla$daily, text=hovertextd, hoverinfo="text", line=list(color="green", shape="spline"))
    p
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

setPlotDailyDeaths <- function(country, startDate, endDate){
    indexStart <- match(as.Date(startDate), fechas)
    indexEnd <- match(as.Date(endDate), fechas)
    tabla <- computeDailyDeathsVariation(country, startDate, endDate)
    dates <- as.Date(tabla$date, format= "%m/%d/%y")
    
    hovertextd <- paste("Date: ", tabla$date,"<br>Deaths reported :", tabla$daily)        
    p <- plot_ly(as.data.frame(tabla), type = "scatter", mode="markers+lines", marker=list(color="orange"),showlegend=FALSE)%>% layout(title=paste(country, "coronavirus daily deaths reported"), xaxis=list(showticklabels=TRUE), margin=m)%>%config(displayModeBar=FALSE)%>%add_trace(x=dates, y=tabla$daily, text=hovertextd, hoverinfo="text", line=list(color="green", shape="spline"))
    p
}


# User interface ----
ui <- fluidPage(
    
    theme = shinytheme("superhero"),
    #twitter
    tags$head(tags$link(rel="icon", href="favicon.ico")),
    tags$head(tags$meta(name="twitter:card", content="summary_large_image")),
    tags$head(tags$meta(name="twitter:site", content="@EnriqueALopezM")),
    tags$head(tags$meta(name="twitter:title", content="Visual data tools.")),
    tags$head(tags$meta(name="twitter:description", content="COVID-19 daily growth per country")),
    tags$head(tags$meta(name="twitter:creator", content="@EnriqueALopezM")),
    tags$head(tags$meta(name="twitter:image", content="http://www.enadol.de/images/growth.JPG")),
    tags$head(tags$meta(name="keywords", content="coronavirus, COVID-19, virus, confirmed cases, deaths, rates")),
    tags$head(tags$meta(name="description", content="Interactive data visualizaton of COVID-19 absolute number of cases and deaths, as well as growth rates for both. Worldwide.")),
    #facebook
    tags$head(tags$meta(property="og:title", content="COVID-19 daily cases growth per country")),
    tags$head(tags$meta(property="og:type", content="article")),
    tags$head(tags$meta(property="og:url", content="http://data.enadol.de/shiny/growth/")),
    tags$head(tags$meta(property="og:image", content="http://www.enadol.de/images/growth.JPG")),
    tags$head(tags$meta(property="og:description", content="Daily COVID-19 cases growth, per country, as officialy reported")),
    
    
    mainPanel(width = 75,
              titlePanel(h1("COVID-19 EVOLUTION ANALYSIS TOOL"), ("By Enrique A López Magallón")),
              selectInput("var", 
                          label = "Select country:",
                          choices = c(countries),
                          
                          #helpText("Datos acerca del delito de homicidio doloso en México, de 1931 a 2018. Seleccione entre número absoluto o tasa según población. Pase el ratón por la gráfica para obtener los datos de cada año."),
                          selectize = TRUE
                          
              ),dateRangeInput("daterange",label = "Choose date range: " , start = fechas[1], min= fechas[1],end = fechas[length(fechas)], max= fechas[length(fechas)], startview= "month", separator=" to ", autoclose=TRUE),
              
              h3("CORONAVIRUS DAILY CASES AS REPORTED BY COUNTRY"),plotlyOutput("daily"), h3("CORONAVIRUS CONFIRMED CASES, RECOVERED (CUMULATIVE) AND ACTIVE CASES*"), plotlyOutput("confirmed"),h5("* active = confirmed - recovered - deaths. No recovered data available for Netherlands and the United Kingdom"),h3("CORONAVIRUS CASES DAILY % GROWTH"), plotlyOutput("lineasPlot"),h3("CORONAVIRUS DAILY DEATHS AS REPORTED BY COUNTRY"),plotlyOutput("dailyDeaths"),h3("CORONAVIRUS DEATHS (CUMULATIVE)"),plotlyOutput("deathsPlot"),h3("CORONAVIRUS DEATHS DAILY % GROWTH, COMPARED TO THE PREVIOUS DAY"),plotlyOutput("deathsGrowthPlot"),h6("Source: Johns Hopkins University - Tool: Enrique López Magallón - @EnriqueALopezM")
    ))


# Server logic ----
server <- function(input, output) {
    m <- list(t=50, pad=5)
    observeEvent(input$daterange, {
        
        observeEvent(input$var, {
            if(input$var %in% withProvinces){
                tabla <- computeRateswProvince(input$var, input$daterange[1], input$daterange[2])
                dates <- as.Date(tabla$date, format= "%m/%d/%y")
                
                tablaRecovered <- simplifyCountryRecovered(input$var, input$daterange[1], input$daterange[2])
                tablaMuertes <- simplifyCountryMuertes(input$var, input$daterange[1], input$daterange[2])
                                
                active <- data.frame(dates, tabla$confirmed, tablaRecovered$recovered, tablaMuertes$deaths)
                colnames(active) <-  c("dates", "confirmed", "recovered", "deaths")
                active$active <- active$confirmed-active$recovered-active$deaths
                active$average <- rowMeans(active[,c("confirmed", "recovered", "deaths")])
                
                hovertextc <- paste("Date: ", tabla$date, "<br>Confirmed cases (absolute): ", tabla$confirmed)
                hovertextcgrowth <- paste("Date: ", tabla$date,"<br>Confirmed cases growth :", format(tabla$rates, digits = 3), " % ")
                hovertextr <- paste("Date: ", tablaRecovered$date, "<br>Recovered (absolute): ", tablaRecovered$recovered)
                hovertexta <- paste("Date: ", active$date, "<br>Active cases: ", active$active)                

                output$daily <- renderPlotly({setPlotDailyCases(input$var, input$daterange[1], input$daterange[2])})
                output$confirmed <- renderPlotly({ plot_ly(as.data.frame(tabla), type = "scatter", mode="markers+lines", marker=list(color="orange"), showlegend=TRUE)%>% layout(title=paste(input$var, "coronavirus confirmed cases, recovered and active cases *"), xaxis=list(showticklabels=TRUE), margin=m)%>%config(displayModeBar=FALSE)%>%add_trace(x=dates, y=~confirmed, name="Confirmed cases", text= hovertextc, hoverinfo="text", line=list(color="green", shape="spline"))%>% add_trace(data = tablaRecovered, x=dates, y=~recovered, name="Recovered",type="scatter", text=hovertextr, hoverinfo="text", line=list(color="red", shape="spline"), marker=list(color="red"), fill="none", fillcolor='rgba(0,100,80,0.2)')%>%add_trace(data= active, type="scatter", mode="lines", x=dates, y=~active, name="Active cases",text=hovertexta, hoverinfo="text", line=list(color="rgba(0,100,80.0.9)", shape="spline"), marker=list(color="rgba(0,100,80,0.9)"), fill="tozeroy", fillcolor='rgba(0,100,80,0.2)')%>%layout(legend = list(orientation="h"))})
                output$lineasPlot<- renderPlotly({ plot_ly(as.data.frame(tabla), type = "scatter", mode="markers+lines", marker=list(color="orange"),showlegend=FALSE)%>% layout(title=paste(input$var, "coronavirus cases daily % growth"),xaxis=list(showticklabels=TRUE), margin=m)%>%config(displayModeBar=FALSE)%>%add_trace(x=dates, y=~rates, text= hovertextcgrowth, hoverinfo="text", line=list(color="green", shape="spline"))})
                output$dailyDeaths <- renderPlotly({setPlotDailyDeaths(input$var, input$daterange[1], input$daterange[2])})
                output$deathsPlot <- renderPlotly({setPlotDeaths(input$var, input$daterange[1], input$daterange[2])})
                output$deathsGrowthPlot <- renderPlotly({setPlotDeathsGrowth(input$var, input$daterange[1], input$daterange[2])})
                
            }
            else{
                tabla <- simplifyCountryCases(input$var, input$daterange[1], input$daterange[2])
                tablaRecovered <- simplifyCountryNormalRecovered(input$var, input$daterange[1], input$daterange[2])
                tablaMuertes <- simplifyCountryMuertes(input$var, input$daterange[1], input$daterange[2])
                
                dates <- as.Date(tablaRecovered$date, format= "%m/%d/%y")
                
                active <- data.frame(dates, tabla$confirmed, tablaRecovered$recovered, tablaMuertes$deaths)
                colnames(active) <-  c("dates", "confirmed", "recovered", "deaths")
                active$active <- active$confirmed-active$recovered-active$deaths
                active$average <- rowMeans(active[,c("confirmed", "recovered", "deaths")])
                
                hovertexta <- paste("Date: ", active$date, "<br>Active cases: ", active$active)                
                hovertextr <- paste("Date: ", tablaRecovered$date, "<br>Recovered (absolute): ", tablaRecovered$recovered)
                
                output$daily <- renderPlotly({setPlotDailyCases(input$var, input$daterange[1], input$daterange[2])})
                output$confirmed <- renderPlotly({setPlotConfirmed(input$var, input$daterange[1], input$daterange[2])%>% add_trace(data = tablaRecovered, name="Recovered",x=dates, y=~recovered, type="scatter", text=hovertextr, hoverinfo="text", line=list(color="red", shape="spline"), marker=list(color="red"), fill="none", fillcolor='rgba(0,100,80,0.2)')%>%add_trace(data= active, type="scatter", mode="lines", name="Active cases",x=dates, y=~active, text=hovertexta, hoverinfo="text", line=list(color="rgba(0,100,80.0.9)", shape="spline"), marker=list(color="rgba(0,100,80,0.9)"), fill="tozeroy", fillcolor='rgba(0,100,80,0.2)')%>%layout(legend = list(orientation="h"))})
                output$lineasPlot <- renderPlotly({setPlot(input$var, input$daterange[1], input$daterange[2])})
                output$dailyDeaths <- renderPlotly({setPlotDailyDeaths(input$var, input$daterange[1], input$daterange[2])})
                output$deathsPlot <- renderPlotly({setPlotDeaths(input$var, input$daterange[1], input$daterange[2])})
                output$deathsGrowthPlot <- renderPlotly({setPlotDeathsGrowth(input$var, input$daterange[1], input$daterange[2])})
            }
        }
        )
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
