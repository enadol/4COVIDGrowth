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

francein <- filter(confirmed, `Country/Region` == "France")
chinain <- filter(confirmed, `Country/Region` == "China")
australiain <- filter(confirmed, `Country/Region` == "Australia")
canadain <- filter(confirmed, `Country/Region` == "Canada")
netherlandsin <- filter(confirmed, `Country/Region` == "Netherlands")
ukin <- filter(confirmed, `Country/Region` == "United Kingdom")

days <- colnames(confirmed[1,][5:ncol(confirmed)])
fechas <- as.Date(days, format = "%m/%d/%y")


franceDin <- filter(deaths, `Country/Region` == "France")
chinaDin <- filter(deaths, `Country/Region` == "China")
australiaDin <- filter(deaths, `Country/Region` == "Australia")
canadaDin <- filter(deaths, `Country/Region` == "Canada")
netherlandsDin <- filter(deaths, `Country/Region` == "Netherlands")
ukDin <- filter(deaths, `Country/Region` == "United Kingdom")

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
        
        p <- plot_ly(as.data.frame(tabla), type = "scatter", mode="line", showlegend=FALSE)%>%config(displayModeBar=FALSE)%>%add_trace(x=dates, y=~confirmed, text= hovertextd, hoverinfo="text", line=list(color="green", shape="spline"))%>% layout(title=paste(country, "coronavirus deaths"), xaxis=list(showticklabels=TRUE))
        p
    }
    else{
        tabla <- simplifyCountryNormal(country, startDate, endDate)
        hovertextd <- paste("Date: ", tabla$date, "<br>Confirmed deaths (absolute): ", tabla$deaths)        
        dates <- as.Date(tabla$date, format= "%m/%d/%y")
        
        
        p <- plot_ly(as.data.frame(tabla), type = "scatter", mode="line", showlegend=FALSE)%>%config(displayModeBar=FALSE)%>%add_trace(x=dates, y=tabla$deaths, text= hovertextd, hoverinfo="text", line=list(color="green", shape="spline"))%>% layout(title=paste(country, "coronavirus deaths"), xaxis=list(showticklabels=TRUE))
        p
    }
}

setPlotDeathsGrowth <- function(country, startDate, endDate){
    tabla <- computeDeathGrowth(country, startDate, endDate)
    dates <- as.Date(tabla$date, format= "%m/%d/%y")
    hovertextdgrowth <- paste("Date: ", tabla$date, "<br>Deaths growth: ", format(tabla$dgrowth, digits = 3), " % ")
    
    p <- plot_ly(as.data.frame(tabla), type = "scatter", mode="line", showlegend=FALSE)%>%config(displayModeBar=FALSE)%>%add_trace(x=dates, y=tabla$dgrowth, text= hovertextdgrowth, hoverinfo= "text", line=list(color="green", shape="spline"))%>% layout(title=paste(country, "coronavirus deaths daily % growth"), xaxis=list(showticklabels=TRUE))
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
        }
        if(country=="Canada"){
            lista <- simplifyCountry(canadain, startDate, endDate)
        }
        if(country=="France"){
            lista <- simplifyCountry(francein, startDate, endDate)
        }
        if(country=="China"){
            lista <- simplifyCountry(chinain, startDate, endDate)
        }
        if(country=="United Kingdom"){
            lista <- simplifyCountry(ukin, startDate, endDate)
        }
        if(country=="Netherlands"){
            lista <- simplifyCountry(netherlandsin, startDate, endDate)
        }
        for(day in days){
            i <- match(day, days)
            if(i==1){
                lista$rates[1] <- 0
                
            }
            else{
                
                lista$rates[i] <- (lista$confirmed[i]-lista$confirmed[i-1])/lista$confirmed[i-1] * 100
                lista$rates <- lista$rates[1:length(lista$date)]
            }  
        }
        return(lista)
    }
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

setPlotConfirmed <- function(country, startDate, endDate){
    
    tabla <- simplifyCountryCases(country, startDate, endDate)
    hovertextc <- paste("Date: ", tabla$date, "<br>Confirmed cases (absolute): ", tabla$confirmed)
    dates <- as.Date(tabla$date, format= "%m/%d/%y")
    
    p <- plot_ly(as.data.frame(tabla), type = "scatter", mode="line", showlegend=FALSE)%>%config(displayModeBar=FALSE)%>%add_trace(x=dates, y=tabla$confirmed, text= hovertextc, hoverinfo="text", line=list(color="green", shape="spline"))%>% layout(title=paste(country, "coronavirus confirmed cases"), xaxis=list(showticklabels=TRUE))
    p
}

deathsAllFrance <- computeDeathswProvince("France", fechas[1], fechas[length(fechas)])
deathsAllChina <- computeDeathswProvince("China", fechas[1], fechas[length(fechas)])
deathsAllAustralia <- computeDeathswProvince("Australia", fechas[1], fechas[length(fechas)])
deathsAllCanada <- computeDeathswProvince("Canada", fechas[1], fechas[length(fechas)])
deathsAllNetherlands <- computeDeathswProvince("Netherlands", fechas[1], fechas[length(fechas)])
deathsAllUK <- computeDeathswProvince("United Kingdom", fechas[1], fechas[length(fechas)])



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
    p <- plot_ly(as.data.frame(tabla), type = "scatter", mode="line", showlegend=FALSE)%>%config(displayModeBar=FALSE)%>%add_trace(x=dates, y=tabla$growth, text=hovertextcgrowth, hoverinfo="text", line=list(color="green", shape="spline"))%>% layout(title=paste(country, "coronavirus cases daily growth"), xaxis=list(showticklabels=TRUE))
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
    
    #facebook
    tags$head(tags$meta(property="og:title", content="COVID-19 daily cases growth per country")),
    tags$head(tags$meta(property="og:type", content="article")),
    tags$head(tags$meta(property="og:url", content="http://data.enadol.de/shiny/growth")),
    tags$head(tags$meta(property="og:image", content="http://www.enadol.de/images/growth.JPG")),
    tags$head(tags$meta(property="og:description", content="Daily COVID-19 cases growth, per country, as officialy reported")),
    
    
    mainPanel(
        titlePanel("COVID-19 EVOLUTION ANALYSIS TOOL"),
        selectInput("var", 
                    label = "Select country:",
                    choices = c(countries),
                    
                    #helpText("Datos acerca del delito de homicidio doloso en México, de 1931 a 2018. Seleccione entre número absoluto o tasa según población. Pase el ratón por la gráfica para obtener los datos de cada año."),
                    selectize = TRUE
                    
        ),dateRangeInput("daterange",label = "Choose date range: " , start = fechas[1], min= fechas[1],end = fechas[length(fechas)], max= fechas[length(fechas)], startview= "month", separator=" to ", autoclose=TRUE),
        
        h3("CORONAVIRUS CONFIRMED CASES"), plotlyOutput("confirmed"),h3("CORONAVIRUS CASES DAILY GROWTH"), plotlyOutput("lineasPlot"),h3("CORONAVIRUS DEATHS"),plotlyOutput("deathsPlot"),h3("CORONAVIRUS DEATHS % DAILY GROWTH, COMPARED TO THE PREVIOUS DAY"),plotlyOutput("deathsGrowthPlot"),h6("Source: Johns Hopkins University - Tool: Enrique López Magallón - @EnriqueALopezM")
    ))


# Server logic ----
server <- function(input, output) {
    observeEvent(input$daterange, {
        
        observeEvent(input$var, {
            if(input$var %in% withProvinces){
                tabla <- computeRateswProvince(input$var, input$daterange[1], input$daterange[2])
                dates <- as.Date(tabla$date, format= "%m/%d/%y")
                
                hovertextc <- paste("Date: ", tabla$date, "<br>Confirmed cases (absolute): ", tabla$confirmed)
                hovertextcgrowth <- paste("Date: ", tabla$date,"<br>Confirmed cases growth :", format(tabla$rates, digits = 3), " % ")
                
                output$confirmed <- renderPlotly({ plot_ly(as.data.frame(tabla), type = "scatter", mode="line", showlegend=FALSE)%>%config(displayModeBar=FALSE)%>%add_trace(x=dates, y=~confirmed, text= hovertextc, hoverinfo="text", line=list(color="green", shape="spline"))%>% layout(title=paste(input$var, "coronavirus confirmed cases"), xaxis=list(showticklabels=TRUE))})
                output$lineasPlot<- renderPlotly({ plot_ly(as.data.frame(tabla), type = "scatter", mode="line", showlegend=FALSE)%>%config(displayModeBar=FALSE)%>%add_trace(x=dates, y=~rates, text= hovertextcgrowth, hoverinfo="text", line=list(color="green", shape="spline"))%>% layout(title=paste(input$var, "coronavirus cases daily growth"), xaxis=list(showticklabels=TRUE))})
                output$deathsPlot <- renderPlotly({setPlotDeaths(input$var, input$daterange[1], input$daterange[2])})
                output$deathsGrowthPlot <- renderPlotly({setPlotDeathsGrowth(input$var, input$daterange[1], input$daterange[2])})
                
            }
            else{
                output$confirmed <- renderPlotly({setPlotConfirmed(input$var, input$daterange[1], input$daterange[2])})
                output$lineasPlot <- renderPlotly({setPlot(input$var, input$daterange[1], input$daterange[2])})
                output$deathsPlot <- renderPlotly({setPlotDeaths(input$var, input$daterange[1], input$daterange[2])})
                output$deathsGrowthPlot <- renderPlotly({setPlotDeathsGrowth(input$var, input$daterange[1], input$daterange[2])})
            }
        }
        )
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
