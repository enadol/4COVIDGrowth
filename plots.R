setPlotConfirmed <- function(country, startDate, endDate){
  
  tabla <- simplifyCountryCases(country, startDate, endDate)
  hovertextc <- paste("Date: ", tabla$date, "<br>Confirmed cases (absolute): ", tabla$confirmed)
  dates <- as.Date(tabla$date, format= "%m/%d/%y")
  
  p <- plot_ly(as.data.frame(tabla), type = "scatter", mode="markers+lines", marker=list(color="orange"),showlegend=TRUE)%>% layout(title=paste(country, "coronavirus confirmed cases, recovered and active cases *"), xaxis=list(showticklabels=TRUE), margin=m)%>%config(displayModeBar=FALSE)%>%add_trace(x=dates, y=tabla$confirmed, name="Confirmed cases", text= hovertextc, hoverinfo="text", line=list(color="green", shape="spline"))%>%layout(xaxis=list(title=""))
  p
}

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

setPlotDailyCases <- function(country, startDate, endDate){
  indexStart <- match(as.Date(startDate), fechas)
  indexEnd <- match(as.Date(endDate), fechas)
  tabla <- computeDailyAbsoluteVariation(country, startDate, endDate)
  dates <- as.Date(tabla$date, format= "%m/%d/%y")
  
  hovertextd <- paste("Date: ", tabla$date,"<br>Confirmed cases reported :", tabla$daily)        
  p <- plot_ly(as.data.frame(tabla), type = "scatter", name="Daily Cases", mode="markers+lines", marker=list(color="orange"),showlegend=TRUE)%>% layout(title=paste(country, "coronavirus daily cases reported"), legend=list(orientation="h"), xaxis=list(showticklabels=TRUE), margin=m)%>%config(displayModeBar=FALSE)%>%add_trace(x=dates, y=tabla$daily, text=hovertextd, hoverinfo="text", line=list(color="green", shape="spline"))
  p
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
    if(country=="Denmark"){
      tabla <- simplifyCountry(denmarkDin, startDate, endDate)
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

setPlotDailyDeaths <- function(country, startDate, endDate){
  indexStart <- match(as.Date(startDate), fechas)
  indexEnd <- match(as.Date(endDate), fechas)
  tabla <- computeDailyDeathsVariation(country, startDate, endDate)
  dates <- as.Date(tabla$date, format= "%m/%d/%y")
  
  hovertextd <- paste("Date: ", tabla$date,"<br>Deaths reported :", tabla$daily)        
  p <- plot_ly(as.data.frame(tabla), type = "scatter", mode="markers+lines", marker=list(color="orange"),showlegend=TRUE)%>% layout(title=paste(country, "coronavirus daily deaths reported"), legend=list(orientation="h"), xaxis=list(showticklabels=TRUE), margin=m)%>%config(displayModeBar=FALSE)%>%add_trace(x=dates, y=tabla$daily, name="Daily deaths", text=hovertextd, hoverinfo="text", line=list(color="green", shape="spline"))
  p
}


setPlotCasesAverages <- function(country, startDate, endDate){
  indexStart <- match(as.Date(startDate), fechas)
  indexEnd <- match(as.Date(endDate), fechas)
  tabla <- computeAverages(country, startDate, endDate)
  dates <- as.Date(tabla$date, format= "%m/%d/%y")
  
  hovertextd <- paste("Date: ", tabla$date,"<br>Average to date :", tabla$average)        
  p <- plot_ly(as.data.frame(tablaPromedios), x=dates, y=tabla$average, name="Daily averages", hovertext="hovertextprom", hoverinfo="text", type = "scatter", mode="lines", line=list(color='rgb(22, 96, 167)'), showlegend=FALSE)%>% layout(title=paste(country, "coronavirus daily average of cases reported"), xaxis=list(showticklabels=TRUE), margin=m)%>%config(displayModeBar=FALSE)
  p
}

