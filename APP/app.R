#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinydashboard)
library(shinythemes)

#App creada para la presentacion de los datos, histograma e imagenes obtenidas
#La app tarda un poco en cargar los datos debido a el procesamiento
#Recomendable ejecutar despues del codigo principal

#Esta parte es el análogo al ui.R
ui <- 
    
    fluidPage(
        
        dashboardPage(
            
            dashboardHeader(title = "Proyecto"),
            
            dashboardSidebar(
                
                sidebarMenu(
                    menuItem("Tabla de datos", tabName = "data_table", icon = icon("table")),
                    menuItem("Histogramas", tabName = "Dashboard", icon = icon("dashboard")),
                    menuItem("Imágen", tabName = "img", icon = icon("file-picture-o"))
                )
                
            ),
            
            dashboardBody(
                
                tabItems(
                    
                    #DATOS
                    tabItem(tabName = "data_table",
                            fluidRow(        
                                titlePanel(h3("Data Table")),
                                dataTableOutput ("data_table")
                            )
                    ), 
                    
                    #Imagenes obteneidad de la regresion lineal
                    tabItem(tabName = "img",
                            fluidRow( 
                                titlePanel(h3("Imagenes obtenidas")),
                                tabsetPanel(
                                    tabPanel("Serie de tiempo",   #Pestaña de Plots
                                             
                                             img( src = "Serie_tiempo.png", 
                                                  height = 500, width = 600) 
                                    )
                                    ,
                                    
                                    tabPanel("Serie de tiempo diferenciada",   #Pestaña de Plots
                                             
                                             img( src = "serie_tiempo_dif.png", 
                                                  height = 500, width = 600)
                                    ),
                                    tabPanel("Forecast_plot",   #Pestaña de Plots
                                             
                                             img( src = "Forecast_plot.png", 
                                                  height = 500, width = 600)
                                    ),
                                    tabPanel("plot",   #Pestaña de Plots
                                             
                                             img( src = "correlo_plot.png", 
                                                  height = 500, width = 600)
                                    )
                                )
                            )
                    ), 
                    
                    # Histograma
                    tabItem(tabName = "Dashboard",
                            fluidRow(
                                titlePanel("Histogramas"), 
                                
                                selectInput("plot_type", "Tipo de Gráfica", 
                                            c("Histogramas generales" = "HistogramG", 
                                              "Histogramas con doble clasificacion" = "HistogramP")),
                                
                                selectInput("x", "Seleccione el valor de X",
                                            choices = c("Municipio","ANIO","SEXO","MODELO")),
                                
                                conditionalPanel(condition = "input.plot_type != 'HistogramG' && input.x != 'MODELO' ", 
                                                 selectInput("zz", "Selecciona la variable de clasifiacion", 
                                                             choices = c("Municipio","SEXO","ANIO")
                                                 )),
                                box(plotOutput("plot1", height = 400))
                                
                            )
                    )
                    
                )
            )
        )
    )

#De aquí en adelante es la parte que corresponde al server

server <- function(input, output) {
    library(ggplot2)
    
    #nombre de las columnas de tipo de vehiculo
    Modelo<-c("AUTOMOVIL","CAMPASAJ","MICROBUS","PASCAMION","OMNIBUS","TRANVIA","CAMIONETA","CAMION","TRACTOR","FERROCARRI","MOTOCICLET","BICICLETA","OTROVEHIC")   
    
    #convierte las columnas en filas
    md2<-gather(data_clean,Modelo,key = "Modelo",value = "Num_accidentes")
    #filtra los datos para eliminar lo valores con 9 y las edades de 0  y 99
    md2<-md2%>%filter(ID_EDAD!=0,Num_accidentes!=0,ID_EDAD!=99)
    #agrupa por modelo y fecha y calcula el numero de elementos
    md2<-md2%>%group_by(Fecha,Modelo)%>%select(Fecha,Modelo,Num_accidentes)%>%summarise(n=n())
    #agrupa por modelo y fecha
    md2<-md2%>%group_by(Modelo,year(Fecha))%>%summarise(n=n())
    
    #Gráfico de Histograma
    output$plot1 <- renderPlot({
        
       if (input$plot_type == "HistogramG") {
           
           
           if(input$x != "MODELO"){
               x <- data_clean[,input$x]
               ggplot(data_clean, aes(factor(x))) + 
                   geom_bar(col= "black", fill = "blue", stat = "count")+
                   theme_light() + 
                   theme(axis.text.x = element_text(angle=45, hjust=1))+
                   xlab(input$x) + ylab("Numero de accidentes")
               
           }
            else{
                ggplot(data = md2, aes(factor(Modelo),n)) +
                    geom_col(col="black")+
                    theme(axis.text.x = element_text(angle=45, hjust=1))+ ggtitle("Histograma de Accidentes por tipo de vehiculo y año") +
                    xlab("Tipo de Vehiculo") +ylab("Numero de accidentes")
                
            }
            
        }  else  {
            
            if(input$x != "MODELO"){
            x <- data_clean[,input$x]
            
            ggplot(data_clean, aes(factor(x))) + 
                geom_bar(aes(fill = factor(data_clean[,input$zz])),position = "dodge",col="black")+
                theme_light() + 
                theme(axis.text.x = element_text(angle=45, hjust=1))+
                xlab(input$x) + ylab("Numero de accidentes") +labs(fill = input$zz)
            }
            
            else{
                ggplot(data = md2, aes(factor(Modelo),n)) +
                    geom_col(aes(fill=factor(`year(Fecha)`)),position ="dodge",col="black")+
                    theme(axis.text.x = element_text(angle=45, hjust=1))+ ggtitle("Histograma de Accidentes por tipo de vehiculo y año") +
                    xlab("Tipo de Vehiculo") +labs(fill = "YEAR")+ylab("Numero de accidentes")#+facet_wrap(.~`year(Fecha)`,scales = "free_x")+theme(axis.text.x = element_text(angle=45, hjust=1))
                
            }
            
        }   
        
    })
    
    #Data Table
    output$data_table <- renderDataTable( {select(data_clean, ANIO,AUTOMOVIL:PASCAMION,CAMIONETA,CAMION,MOTOCICLET,SEXO,ID_EDAD)}, 
                                          options = list(aLengthMenu = c(5,20,30),
                                                         iDisplayLength = 5)
    )
    
}


shinyApp(ui, server)