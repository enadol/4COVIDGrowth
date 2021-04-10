library(shiny)
library(plotly)
library(readr)
library(jsonlite)
library(plotly)
library(dplyr)
library(rlist)
library(shinythemes)
library(shinydashboard)
source("functions.R", local = TRUE)
source("plots.R", local = TRUE)
source("simplify.R", local = TRUE)
#No scientific notation
options(scipen= 999)
options(digits= 3)

#Load Data
confirmed <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", check.names = FALSE)
deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", check.names = FALSE)
recovered <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv", check.names = FALSE)

countries <- as.character(unique(confirmed$`Country/Region`))

tablaconfirmed <- read_csv("https://raw.githubusercontent.com/enadol/covid/master/confirmedrates.csv")

tabladeaths <- read_csv("https://raw.githubusercontent.com/enadol/covid/master/deathrates.csv")

tablagrowth <- jsonlite::read_json("https://raw.githubusercontent.com/enadol/covid/master/input.json")

withProvinces <- c("Australia","Canada", "China", "France", "Netherlands", "United Kingdom", "Denmark")

#cases
francein <- filter(confirmed, `Country/Region` == "France")
chinain <- filter(confirmed, `Country/Region` == "China")
australiain <- filter(confirmed, `Country/Region` == "Australia")
canadain <- filter(confirmed, `Country/Region` == "Canada")
netherlandsin <- filter(confirmed, `Country/Region` == "Netherlands")
ukin <- filter(confirmed, `Country/Region` == "United Kingdom")
denmarkin<- filter(confirmed, `Country/Region` == "Denmark")

days <- colnames(confirmed[1,][5:ncol(confirmed)])
fechas <- as.Date(days, format = "%m/%d/%y")

#deaths
franceDin <- filter(deaths, `Country/Region` == "France")
chinaDin <- filter(deaths, `Country/Region` == "China")
australiaDin <- filter(deaths, `Country/Region` == "Australia")
canadaDin <- filter(deaths, `Country/Region` == "Canada")
netherlandsDin <- filter(deaths, `Country/Region` == "Netherlands")
ukDin <- filter(deaths, `Country/Region` == "United Kingdom")
denmarkDin<- filter(deaths, `Country/Region` == "Denmark")

#recovered
franceRin <- filter(recovered, `Country/Region` == "France")
chinaRin <- filter(recovered, `Country/Region` == "China")
australiaRin <- filter(recovered, `Country/Region` == "Australia")
canadaRin <- filter(recovered, `Country/Region` == "Canada")
netherlandsRin <- filter(recovered, `Country/Region` == "Netherlands")
ukRin <- filter(recovered, `Country/Region` == "United Kingdom")
denmarkRin<- filter(recovered, `Country/Region` == "Denmark")


deathsAllFrance <- computeDeathswProvince("France", fechas[1], fechas[length(fechas)])
deathsAllChina <- computeDeathswProvince("China", fechas[1], fechas[length(fechas)])
deathsAllAustralia <- computeDeathswProvince("Australia", fechas[1], fechas[length(fechas)])
deathsAllCanada <- computeDeathswProvince("Canada", fechas[1], fechas[length(fechas)])
deathsAllNetherlands <- computeDeathswProvince("Netherlands", fechas[1], fechas[length(fechas)])
deathsAllUK <- computeDeathswProvince("United Kingdom", fechas[1], fechas[length(fechas)])
deathsAllDenmark <- computeDeathswProvince("Denmark", fechas[1], fechas[length(fechas)])

m <- list(t=50, pad=5)


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
                          selectize = TRUE),
                          dateRangeInput("daterange",label = "Choose date range: " , start = fechas[1], min= fechas[1],end = fechas[length(fechas)], max= fechas[length(fechas)], startview= "month", separator=" to ", autoclose=TRUE),
              
              fluidRow(column(width=9,
            tabBox(width=900,
              tabPanel("Daily cases reported", h3("CORONAVIRUS DAILY CASES AS REPORTED BY COUNTRY"),plotlyOutput("daily")),
              tabPanel("Confirmed, recovered and active cases", h3("CORONAVIRUS CONFIRMED CASES, RECOVERED (CUMULATIVE) AND ACTIVE CASES*"), plotlyOutput("confirmed"),h5("* active = confirmed - recovered - deaths. No recovered data available for Netherlands, Sweden and the United Kingdom")),
              tabPanel("Cases growth",h3("CORONAVIRUS CASES DAILY % GROWTH"), plotlyOutput("lineasPlot")),
              tabPanel("Daily deaths", h3("CORONAVIRUS DAILY DEATHS AS REPORTED BY COUNTRY"),plotlyOutput("dailyDeaths")),
              tabPanel("Deaths cumulative",h3("CORONAVIRUS DEATHS (CUMULATIVE)"),plotlyOutput("deathsPlot")),
              tabPanel("Deaths daily growth",h3("CORONAVIRUS DEATHS DAILY % GROWTH, COMPARED TO THE PREVIOUS DAY"),plotlyOutput("deathsGrowthPlot")),h6("Source: Johns Hopkins University - Tool: Enrique López Magallón - @EnriqueALopezM")
    )))))


# Server logic ----
server <- function(input, output) {
    m <- list(t=50, pad=5)
    #No scientific notation
    options(scipen= 999)
    options(digits= 3)
    
    observeEvent(input$daterange, {
        
        observeEvent(input$var, {
            #para países con provincias
            if(input$var %in% withProvinces){
                #tablas para data
                tabla <- computeRateswProvince(input$var, input$daterange[1], input$daterange[2])
                dates <- as.Date(tabla$date, format= "%m/%d/%y")
                tablaRecovered <- simplifyCountryRecovered(input$var, input$daterange[1], input$daterange[2])
                tablaMuertes <- simplifyCountryMuertes(input$var, input$daterange[1], input$daterange[2])
                tablaPromedios <- computeAverages(input$var, input$daterange[1], input$daterange[2])
                tablaPromediosMuertes <- computeAveragesDeaths(input$var, input$daterange[1], input$daterange[2])
                
                
                active <- data.frame(dates, tabla$confirmed, tablaRecovered$recovered, tablaMuertes$deaths)
                colnames(active) <-  c("dates", "confirmed", "recovered", "deaths")
                active$active <- active$confirmed-active$recovered-active$deaths
                active$average <- rowMeans(active[,c("confirmed", "recovered", "deaths")])
                
                #textos para infoboxes
                hovertextc <- paste("Date: ", tabla$date, "<br>Confirmed cases (absolute): ", tabla$confirmed)
                hovertextcgrowth <- paste("Date: ", tabla$date,"<br>Confirmed cases growth :", format(tabla$rates, digits = 3), " % ")
                hovertextr <- paste("Date: ", tablaRecovered$date, "<br>Recovered (absolute): ", tablaRecovered$recovered)
                hovertexta <- paste("Date: ", active$date, "<br>Active cases: ", active$active)
                hovertextprom <- paste("Date: ", tablaPromedios$date, "<br>Daily, cumulative average of cases: ", format(tablaPromedios$average, digits = 3))
                hovertextpromD <- paste("Date: ", tablaPromediosMuertes$date, "<br>Daily, cumulative average of deaths reported: ", format(tablaPromediosMuertes$average, digits = 3))                
                
                output$daily <- renderPlotly({setPlotDailyCases(input$var, input$daterange[1], input$daterange[2])%>% add_trace(data = tablaPromedios,text=hovertextprom, hoverinfo="text", x=dates, y=~average, name="Daily average", type="scatter", mode="line", line=list(color='rgb(22,96,167)'), marker=list(color='rgb(22,96,167)'))%>% layout(xaxis= list(showlegend=TRUE), yaxis=list(title=""))})
                output$confirmed <- renderPlotly({ plot_ly(as.data.frame(tabla), type = "scatter", mode="markers+lines", marker=list(color="orange"), showlegend=TRUE)%>% layout(title=paste(input$var, "coronavirus confirmed cases, recovered and active cases *"), xaxis=list(showticklabels=TRUE), margin=m)%>%config(displayModeBar=FALSE)%>%add_trace(x=dates, y=~confirmed, name="Confirmed cases", text= hovertextc, hoverinfo="text", line=list(color="green", shape="spline"))%>%layout(yaxis=list(title=""))%>% add_trace(data = tablaRecovered, x=dates, y=~recovered, name="Recovered",type="scatter", text=hovertextr, hoverinfo="text", line=list(color="red", shape="spline"), marker=list(color="red"), fill="none", fillcolor='rgba(0,100,80,0.2)')%>%add_trace(data= active, type="scatter", mode="lines", x=dates, y=~active, name="Active cases",text=hovertexta, hoverinfo="text", line=list(color="rgba(0,100,80.0.9)", shape="spline"), marker=list(color="rgba(0,100,80,0.9)"), fill="tozeroy", fillcolor='rgba(0,100,80,0.2)')%>%layout(legend = list(orientation="h"))})
                output$lineasPlot<- renderPlotly({ plot_ly(as.data.frame(tabla), type = "scatter", mode="markers+lines", marker=list(color="orange"),showlegend=FALSE)%>% layout(title=paste(input$var, "coronavirus cases daily % growth"),xaxis=list(showticklabels=TRUE), margin=m)%>%config(displayModeBar=FALSE)%>%add_trace(x=dates, y=~rates, text= hovertextcgrowth, hoverinfo="text", line=list(color="green", shape="spline"))%>%layout(yaxis=list(title=""))})
                output$dailyDeaths <- renderPlotly({setPlotDailyDeaths(input$var, input$daterange[1], input$daterange[2])%>%add_trace(data = tablaPromediosMuertes, x=dates, y=~average, name="Daily deaths average", type="scatter", mode="line", line=list(color='rgb(22,96,167)'), marker=list(color='rgb(22,96,167)'),text=hovertextpromD, hoverinfo="text")%>%layout(xaxis= list(showlegend=TRUE), yaxis=list(title=""))})
                output$deathsPlot <- renderPlotly({setPlotDeaths(input$var, input$daterange[1], input$daterange[2])%>%layout(yaxis=list(title=""))})
                output$deathsGrowthPlot <- renderPlotly({setPlotDeathsGrowth(input$var, input$daterange[1], input$daterange[2])})
                
            }
            else{
                tabla <- simplifyCountryCases(input$var, input$daterange[1], input$daterange[2])
                tablaRecovered <- simplifyCountryNormalRecovered(input$var, input$daterange[1], input$daterange[2])
                tablaMuertes <- simplifyCountryMuertes(input$var, input$daterange[1], input$daterange[2])
                tablaPromedios <- computeAverages(input$var, input$daterange[1], input$daterange[2])
                tablaPromediosMuertes <- computeAveragesDeaths(input$var, input$daterange[1], input$daterange[2])                
                
                dates <- as.Date(tablaRecovered$date, format= "%m/%d/%y")
                
                active <- data.frame(dates, tabla$confirmed, tablaRecovered$recovered, tablaMuertes$deaths)
                colnames(active) <-  c("dates", "confirmed", "recovered", "deaths")
                active$date <- dates
                active$active <- active$confirmed-active$recovered-active$deaths
                active$average <- rowMeans(active[,c("confirmed", "recovered", "deaths")])
                
                hovertexta <- paste("Date: ", active$date, "<br>Active cases: ", active$active)                
                hovertextr <- paste("Date: ", tablaRecovered$date, "<br>Recovered (absolute): ", tablaRecovered$recovered)
                hovertextprom <- paste("Date: ", tablaPromedios$date, "<br>Daily, cumulative average of cases: ", format(tablaPromedios$average, digits = 3))                
                hovertextpromD <- paste("Date: ", tablaPromediosMuertes$date, "<br>Daily, cumulative average of deaths reported: ", format(tablaPromediosMuertes$average, digits = 3))                                
                
                output$daily <- renderPlotly({setPlotDailyCases(input$var, input$daterange[1], input$daterange[2])%>% add_trace(data = tablaPromedios, x=dates, y=~average, text=hovertextprom, hoverinfo="text", name="Daily average",  showlabel=TRUE, type="scatter", mode="line", line=list(color='rgb(22,96,167)'), marker=list(color='rgb(22,96,167)'))%>%layout(xaxis= list(showlegend=TRUE), yaxis=list(title=""))})
                output$confirmed <- renderPlotly({setPlotConfirmed(input$var, input$daterange[1], input$daterange[2])%>%layout(yaxis=list(title=""))%>%add_trace(data = tablaRecovered, name="Recovered",x=dates, y=~recovered, type="scatter", text=hovertextr, hoverinfo="text", line=list(color="red", shape="spline"), marker=list(color="red"), fill="none", fillcolor='rgba(0,100,80,0.2)')%>%layout(yaxis=list(title=""))%>%add_trace(data= active, type="scatter", mode="lines", name="Active cases",x=~date, y=~active, text=hovertexta, hoverinfo="text", line=list(color="rgba(0,100,80.0.9)", shape="spline"), marker=list(color="rgba(0,100,80,0.9)"), fill="tozeroy", fillcolor='rgba(0,100,80,0.2)')%>%layout(legend = list(orientation="h"))})
                output$lineasPlot <- renderPlotly({setPlot(input$var, input$daterange[1], input$daterange[2])})
                output$dailyDeaths <- renderPlotly({setPlotDailyDeaths(input$var, input$daterange[1], input$daterange[2])%>% add_trace(data = tablaPromediosMuertes, x=dates, y=~average, text=hovertextpromD, hoverinfo="text", name="Daily deaths average", type="scatter", mode="line", line=list(color='rgb(22,96,167)'), marker=list(color='rgb(22,96,167)'))%>% layout(xaxis= list(showlegend=TRUE), yaxis=list(title=""))})
                output$deathsPlot <- renderPlotly({setPlotDeaths(input$var, input$daterange[1], input$daterange[2])})
                output$deathsGrowthPlot <- renderPlotly({setPlotDeathsGrowth(input$var, input$daterange[1], input$daterange[2])})
            }
        }
        )
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
