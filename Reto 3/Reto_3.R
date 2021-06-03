library(shiny)
library(shinyjs)
library(deSolve)
archivo <- "C:\\Users\\PC\\Documents\\Analisis Numerico\\Reto 3\\DatosVirusCOVID-19.csv"
baseDatos <- read.csv(archivo)
num <- llenarDatos()
numDias <- nrow(baseDatos)
numDias <- numDias-1
Mostrar <- FALSE
Interfaz = fluidPage(useShinyjs(), titlePanel("Virus Covid-19"), sidebarLayout(
  sidebarPanel(
    sliderInput("dias", "DÍAS:", min = 1, max = numDias,value = numDias),
    sliderInput("pobla", "POBLACIÓN:", min = 1, max = num, value = num ),
    sliderInput("transmi", "TASA DE TRANSMISIÓN:", min = 0, max = 1, value = 0.5),
    sliderInput("tasaRecu", "TASA DE RECUPERACIÓN:", min = 0, max = 1, value = 0.1),
    sliderInput("infect", "INFECTADOS:", min = 1, max = num ,value = 1),
    sliderInput("recu", "RECUPERADOS:", min = 0, max = num, value = 0),
  ),mainPanel(
    actionButton("btnReal", "Mostrar Datos Reales"),
    actionButton("btnCalcuSI", "Calcular población Susceptible, Infectada (SI)"),
    actionButton("btnCalcuSIR", "Calcular población Susceptible, Infectada, Recuperada(SIR)"),
    plotOutput("distPlot"),
    plotOutput("distPlot2"),
    plotOutput("distPlotERROR"),
    plotOutput("distPlot2ERROR"),
    plotOutput("distPlot3ERROR"),
  )
)
)
ModeloSIR = function(tiempos, modelo, tasas)
{
  with(as.list(c(modelo, tasas)), 
       {
         r = c((- beta*S*I/(S+I+R)), (beta*S*I/(S+I+R) - gamma*I), (gamma*I))
         r = list(r)
         return(r)
       })
}
ModeloSI = function(tiempos, modelo, tasa)
{
  with(as.list(c(modelo, tasa)), 
       {
         r = c((- beta*S*I/(S+I)), (beta*S*I/(S+I)))
         r = list(r)
         return(r)
       })
}
funcionamiento = function(input, output, session) {
  calcuModeloSI = function(){
    output$distPlot3ERROR = NULL
    tasaTransmi = c(beta = input$transmi)
    letras = c(S = input$pobla, I = input$infect)
    output$distPlot = renderPlot({
      pDias = input$dias
      tiempos = seq(0, pDias, by = 0.1)
      Modelo1SiSimulacion = as.data.frame(ode(y=letras, times=tiempos, func=ModeloSI,parms=tasaTransmi,method = "rk4"))
      attach(Modelo1SiSimulacion)
      plot(tiempos, S, type="l", col="purple", ylim=c(0,sum(letras)), xlab="Días", ylab="Población",main = "Runge Kutta 4")
      lines(tiempos, I, type="l", col="red")
      legend(x = "topright", legend=c("Susceptibles", "Infectados"), col=c("purple", "red"), lty=rep(1, 2)) 
    })
    output$distPlot2ERROR = renderPlot({
      ErroresModeloR = c()
      pDias = input$dias
      i = 1
      tiempos = seq(0, pDias, by = 0.1)
      Modelo2SiSimulacion = as.data.frame(ode(y=letras, times=tiempos, func=ModeloSI,parms=tasaTransmi,method = "euler"))
      Modelo1SiSimulacion = as.data.frame(ode(y=letras, times=tiempos, func=ModeloSI,parms=tasaTransmi,method = "rk4"))
      for (x in Modelo1SiSimulacion$I) {
        i = (i +1)
        ErroresModeloR = c(ErroresModeloR, ( (abs(Modelo2SiSimulacion$I[i]-x) )/Modelo2SiSimulacion$I[i] ) *100 )
      }
      plot(Modelo1SiSimulacion$time , ErroresModeloR, col="red", type="l", xlab="Días", ylab="Error relativo", main = "Error Runge Kutta 4 vs Euler", ylim = c(0,100))
    })
    output$distPlot2 = renderPlot({
      pDias = input$dias
      tiempos = seq(0, pDias, by = 0.1)
      Modelo2SiSimulacion = as.data.frame(ode(y=letras, times=tiempos, func=ModeloSI,parms=tasaTransmi,method = "euler"))
      attach(Modelo2SiSimulacion)
      plot(tiempos, S, type="l", col="purple", ylim=c(0,sum(letras)), xlab="Días", ylab="Población", main = "Euler")
      lines(tiempos, I, type="l", col="red")
      legend(x = "topright", legend=c("Susceptibles", "Infectados"), col=c("purple", "red"), lty=rep(1, 2)) 
    })
    output$distPlotERROR = renderPlot({
      i =1
      ErroresModeloE = c()
      pDias = input$dias
      tiempos = seq(0, pDias, by = 0.1)
      Modelo1SiSimulacion = as.data.frame(ode(y=letras, times=tiempos, func=ModeloSI,parms=tasaTransmi,method = "rk4"))
      Modelo2SiSimulacion = as.data.frame(ode(y=letras, times=tiempos, func=ModeloSI,parms=tasaTransmi,method = "euler"))
      for (x in Modelo1SiSimulacion$S) {
        i = (i +1)
        ErroresModeloE = c(ErroresModeloE, ( (abs(Modelo2SiSimulacion$S[i]-x) )/Modelo2SiSimulacion$S[i] ) *100)
      }
      plot(Modelo1SiSimulacion$time , ErroresModeloE, col="purple", type="l", xlab="Días", ylab="Error relativo", main = "Error Euler vs Runge Kutta 4", ylim = c(0,100))
    })
  }
  VerDatosReales = function(){
    datosNULL()
    output$distPlot = renderPlot({
      pDias = input$dias
      num = llenarDatos()
      pobla = (-88:0)
      Recuperados = baseDatos[["dimessi_guariti"]] 
      Muertos = baseDatos[["deceduti"]] 
      tiempos = seq(0+2, pDias, by = 1)
      pobla[pobla<0] = num
      Susceptible = baseDatos[["totale_positivi"]] 
      Infectados = baseDatos[["totale_positivi"]]
      Susceptible=pobla-(Recuperados+Infectados+Muertos)
      Infectados=Infectados[-(pDias:length(Infectados))]
      Susceptible=Susceptible[-(pDias:length(Susceptible))]
      Muertos=Muertos[-(pDias:length(Muertos))]
      Recuperados=Recuperados[-(pDias:length(Recuperados))]
      plot(tiempos, Susceptible, type="l", col="purple", xlab="Días", ylab="Población",main = "Datos Reales", ylim=c(0,n))
      lines(tiempos, Infectados, type="l", col="red")
      lines(tiempos, Recuperados, type="l", col="green")
      lines(tiempos, Muertos, type="l", col="black")
      legend(x = "topright", legend=c("Susceptibles", "Infectados", "Recuperados", "Muertos"), col=c("purple", "red", "green", "black"), lty=rep(1, 2, 3, 4)) 
    })
    
  }
  calcuModeloSIR = function(){
    letras = c(S = input$pobla, I = input$infect, R = input$recu)
    tasas = c(beta = input$transmi, gamma = input$tasaRecu)
    if(Mostrar != FALSE){
      muestra <- TRUE
    }else if(Mostrar != TRUE){
      toggle("distPlotERROR")
      toggle("distPlot2")
      toggle("distPlot2ERROR")
      muestra <- TRUE
    }
    output$distPlot = renderPlot({
      pDias = input$dias
      tiempos = seq(0, pDias, by = 0.1)
      Modelo1SirSimulacion = as.data.frame(ode(y=letras, times=tiempos, func=ModeloSIR,parms=tasas,method = "rk4"))
      attach(Modelo1SirSimulacion)
      plot(tiempos, S, type="l", col="purple", ylim=c(0,sum(letras)), xlab="Días", ylab="Población",main = "Runge Kutta 4")
      lines(tiempos, I, type="l", col="red")
      lines(tiempos, R, type="l", col="green")
      legend(x = "topright", legend=c("Susceptibles", "Infectados", "Recuperados"), col=c("purple", "red", "green"), lty=rep(1, 2, 3)) 
    })
    output$distPlot2 = renderPlot({
      pDias = input$dias
      tiempos = seq(0, pDias, by = 0.1)
      Modelo2SirSimulacion = as.data.frame(ode(y=letras, times=tiempos, func=ModeloSIR, parms=tasas, method = "euler"))
      attach(Modelo2SirSimulacion)
      plot(tiempos, S, type="l", col="purple", ylim=c(0,sum(letras)), xlab="Días", ylab="Población", main = "Euler")
      lines(tiempos, I, type="l", col="red")
      lines(tiempos, R, type="l", col="green")
      legend(x = "topright", legend=c("Susceptibles", "Infectados", "Recuperados"), col=c("purple", "red","green"), lty=rep(1, 2))
    })
    output$distPlotERROR <- renderPlot({
      i =1
      poblacion = (-88:0)
      num = llenarDatos()
      ErrorPInfectada = c()
      poblacion[poblacion<0] = num
      pDias = input$dias
      tiempos = seq(0, pDias, by = 0.1)
      tiemposAuxiliar = seq(0, (pDias-2), by = 1)
      SusceptiblesT = baseDatos[["totale_positivi"]]       
      InfectadosT = baseDatos[["totale_positivi"]]
      RecuperadosT = baseDatos[["dimessi_guariti"]]
      MuertosT = baseDatos[["deceduti"]] 
      SusceptiblesT=poblacion-(RecuperadosT+InfectadosT+MuertosT)
      InfectadosT=InfectadosT[-(pDias:length(InfectadosT))]
      TeoricoInfectados = InfectadosT
      Modelo2SirSimulacion <- as.data.frame(ode(y=letras, times=tiemposAuxiliar, func=ModeloSIR,parms=tasas,method = "rk4"))
      for (x in TeoricoInfectados) {
        i = (i+1)
        ErrorPInfectada = c(ErrorPInfectada, ( (abs(x-Modelo2SirSimulacion$I[i]) )/x ) *100)
      }
      plot(Modelo2SirSimulacion$time , ErrorPInfectada, col="red", type="l", xlab="Días", ylab="Error relativo", main = "Error de población Infectada")
    })
    output$distPlot2ERROR = renderPlot({
      num = llenarDatos()
      i =1
      poblacion = (-88:0)
      poblacion[poblacion<0] = num
      pDias = input$dias
      tiempos = seq(0, pDias, by = 0.1)
      SusceptiblesT = baseDatos[["totale_positivi"]]         
      InfectadosT = baseDatos[["totale_positivi"]] 
      tiemposAuxiliar = seq(0, (pDias-2), by = 1)
      Modelo2SirSimulacion = as.data.frame(ode(y=letras, times=tiemposAuxiliar, func=ModeloSIR,parms=tasas,method = "rk4"))
      RecuperadosT = baseDatos[["dimessi_guariti"]] 
      MuertosT = baseDatos[["deceduti"]] 
      ErrorSusceptible = c()
      SusceptiblesT=poblacion-(RecuperadosT+InfectadosT+MuertosT)
      SusceptiblesT=SusceptiblesT[-(pDias:length(SusceptiblesT))]
      TeoricoSusceptibles = SusceptiblesT
      for (x in TeoricoSusceptibles) {
        i = (i+1)
        ErrorSusceptible = c(ErrorSusceptible, ( (abs(x-Modelo2SirSimulacion$S[i]) )/x ) *100 )
      }
      plot(Modelo2SirSimulacion$time , ErrorSusceptible, col="purple", type="l", xlab="Días", ylab="Error relativo", main = "Error de población Susceptible")
    })
    output$distPlot3ERROR = renderPlot({
      ErrorRecuperaciones = c() 
      pDias = input$dias
      num = llenarDatos()
      tiempos = seq(0, pDias, by = 0.1)
      i = 1
      poblacion = (-88:0)
      poblacion[poblacion<0] = num
      tiemposAuxiliar = seq(0, (pDias-2), by = 1)
      SusceptiblesT = baseDatos[["totale_positivi"]]       
      InfectadosT = baseDatos[["totale_positivi"]]
      Modelo2SirSimulacion = as.data.frame(ode(y=letras, times=tiemposAuxiliar, func=ModeloSIR,parms=tasas,method = "rk4"))
      RecuperadosT = baseDatos[["dimessi_guariti"]]
      MuertosT = baseDatos[["deceduti"]] 
      SusceptiblesT=poblacion-(RecuperadosT+InfectadosT+MuertosT)
      RecuperadosT=RecuperadosT[-(pDias:length(RecuperadosT))]
      TeoricoR = RecuperadosT
      for (x in TeoricoR) {
        i = (i+1)
        ErrorRecuperaciones = c(ErrorRecuperaciones, ( (abs(x-Modelo2SirSimulacion$R[i]) )/x ) *100 )
      }
      plot(Modelo2SirSimulacion$time , ErrorRecuperaciones, col="green", type="l", xlab="Días", ylab="Error relativo", main = "Error de población Recuperada")
    })
  }
  observeEvent(input$btnCalcuSIR, {
    print(class(input$pobla))
    print(input$pobla)
    calcuModeloSIR()
  })
  observeEvent(input$btnReal, {
    print(class(input$pobla))
    print(input$pobla)
    VerDatosReales()
  })
  observeEvent(input$btnCalcuSI, {
    print(class(input$pobla))
    print(input$pobla)
    calcuModeloSI()
  })
  datosNULL = function(){
    output$distPlot2 = NULL
    output$distPlotERROR = NULL
    output$distPlot2ERROR = NULL
    output$distPlot3ERROR = NULL
  }
}
llenarDatos = function(){
  num = baseDatos[88,"dimessi_guariti"] + baseDatos[88,"deceduti"]+baseDatos[88,"totale_positivi"]
  return (num)
}
shinyApp(ui = Interfaz, server = funcionamiento)