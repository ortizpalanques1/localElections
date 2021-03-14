#### Libraries ####
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(tidyr)
library(haven)
library(ggrepel)
library(leaflet)
library(sp)
library(forcats)
load("data/huescar.RData")
test <- c(colnames(huescarS20191110[2:(ncol(huescarS20191110)-4)]))
# Define UI for application that draws a histogram
ui <-dashboardPage(
    dashboardHeader(title="HUESCAR"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Indicadores", tabName = "indicadores", icon = icon("dashboard")),
            menuItem("Voto según Ingreso", icon = icon("th"), tabName = "cruce",
                     badgeLabel = "new", badgeColor = "green")
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "indicadores",
                    fluidRow(
                        column(width = 12,
                               fluidRow(
                                   column(4,
                                          leafletOutput("municipio"
                                                        #, height = 380
                                          )
                                   ),
                                   column(2,
                                          valueBoxOutput(width=12,"distritotexto"),
                                          valueBoxOutput(width=12,"secciontexto"),
                                          valueBoxOutput(width=12,"proporcional")
                                   ),
                                   column(3,
                                          plotOutput("fueIng")
                                   ),
                                   column(3,
                                          fluidRow(
                                              plotOutput("ingreso", height = 250)
                                          ),
                                          fluidRow(
                                              plotOutput("tramos", height = 150)
                                          ))
                               )
                        )
                    ),
                    fluidRow(
                        box(
                            title = "Resultado de las Municipales 2019", 
                            status = "primary", 
                            solidHeader = TRUE,
                            width = 4, 
                            height = 320,
                            plotOutput("mun2019")
                        ),
                        box(
                            title = "Resultado de las Generales 11/2019",
                            status = "danger",
                            solidHeader = TRUE,
                            width = 4, 
                            height = 320,
                            plotOutput("gen022019")),
                        box(
                            title = "Diferencia Porcentual entre elecciones",
                            status = "success",
                            solidHeader = TRUE,
                            width = 4, 
                            height = 320,
                            plotOutput("comparo"))
                    ),
                    fluidRow(
                        valueBoxOutput("PartMun",width = 4),
                        valueBoxOutput("PartGen",width = 4),
                        valueBoxOutput("PartDif",width = 4)
                    )
            ),
            tabItem(tabName = "cruce",
                    h2("Rendimiento del partido de acuerdo al ingreso medio. Elecciones Generales de noviembre de 2019"),
                    fluidRow(
                        column(3,
                               selectInput("select", h3("Partidos"), 
                                           choices  = sort(test,decreasing = FALSE), selected = 1))
                    ),
                    fluidRow(
                        plotOutput("selected_var")
                    )
            )
        )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    # AREA PARA EL REACTIVE
    # REACTIVE PESTAÑA No. 1.
    ccaa <- reactiveValues(clickedShape=NULL)
    observeEvent(input$municipio_shape_click,{
        ccaa$clickedShape <- input$municipio_shape_click})
    estaComunidad <- reactive({ccaa$clickedShape$id})
    # REACTIVE PESTAÑA NO. 2.
    partySelected <- reactive({})
    # PESTAÑA NO. 1.    
    # MAPA DEL MUNICIPIO
    output$municipio <- renderLeaflet({
        #nombreSeccion <- paste0("Sección\n",huescar04$COD_SEC)
        leaflet() %>% 
            addTiles() %>% 
            setView(lng = -2.539594, lat = 37.809157, zoom = 16) %>% 
            addPolygons(
                data = huescar04,
                layerId = ~id,
                color = "blue",
                fillOpacity = 0.0
                ,
                #popup = nombreSeccion,
                highlight=highlightOptions(
                    weight=5,
                    color = "#fdcf76",
                    fillColor = "#fdcf76",
                    fillOpacity = 0.1
                )
            )
    })
    # NOMBRE DE DISTRITO
    output$distritotexto <- renderValueBox({
        if(is.null(estaComunidad())){
            elLugarS3 <- "101"
        } else {
            elLugarS3 <- estaComunidad()
        }
        valueBox(
            paste0("D 0",substr(elLugarS3, start = 1, stop = 1)), "Distrito censal", icon = icon("list"),
            color = "navy"
        )
    })
    # NOMBRE DE SECCION
    output$secciontexto <- renderValueBox({
        if(is.null(estaComunidad())){
            elLugarS3 <- "101"
        } else {
            elLugarS3 <- estaComunidad()
        }
        valueBox(
            paste0("S ",substr(elLugarS3, start = 2, stop = 3)), "Sección censal", icon = icon("list"),
            color = "navy"
        )
    })
    # PROPORCION DE VOTOS
    output$proporcional <- renderValueBox({
        if(is.null(estaComunidad())){
            elLugarS3 <- "101"
        } else {
            elLugarS3 <- estaComunidad()
        }
        totalInscritos <- sum(huescarS20191110$censoINE, rm.na=TRUE)
        proporcion <- huescarS20191110 %>% 
            dplyr::filter(ID2019==elLugarS3) %>%
            mutate(Propor=round(censoINE/totalInscritos*100,2)) %>% 
            pull(Propor)
        valueBox(
            paste0(proporcion, " %"), "Porcentaje en el municipio", icon = icon("list"),
            color = "navy"
        )
    })
    # FUENTES DE INGRESO
    output$fueIng <- renderPlot({
        if(is.null(estaComunidad())){
            elLugarS3 <- "101"
        } else {
            elLugarS3 <- estaComunidad()
        }
        fuentes <- c("Salario","Pensión","Por Desempleo","Otra Prestación","Otro Ingreso")
        colorFuente <- c("#024959","#bf5b04","#d9b29c","#a66249","#8c0303")
        color_table <- tibble(fuentes,colorFuente)
        graphFueIng <- fuenteIngresoS %>% 
            filter(id==elLugarS3) %>% 
            select(c(2,5,8,11,14)) %>% 
            gather("Ingreso", "Porcentaje",1:ncol(.)) %>% 
            mutate(Positio=Porcentaje/2 + c(0, cumsum(Porcentaje)[-length(Porcentaje)]))
        graphFueIng <- cbind(graphFueIng,fuentes)
        graphFueIng$Ingreso <- as.factor(graphFueIng$Ingreso)
        graphFueIng$Ingreso <- factor(graphFueIng$Ingreso, levels=graphFueIng$Ingreso)
        graphFueIng$fuentes <- factor(graphFueIng$fuentes, levels=color_table$fuentes)
        graphico04 <- ggplot(graphFueIng, aes(x="",y=Porcentaje,fill=fuentes))+
            geom_bar(width = 1, stat = "identity", position = position_stack(),alpha=0.6)+
            geom_text_repel(aes(y = 100-Positio, 
                                label = paste0(fuentes,"\n",Porcentaje)), size=4, fontface="bold")+
            scale_fill_manual(values = color_table$colorFuente)+
            labs(title = "Fuentes de Ingreso",
                 subtitle = "Porcentaje de la población",
                 caption = "Fuente: INE")+
            coord_polar("y", start=0)+
            theme(
                plot.title = element_text(size = 14, face = "bold"),
                legend.position = "none",
                axis.title = element_blank(),
                axis.text = element_blank(),
                panel.grid=element_blank(),
                panel.border=element_blank(),
                panel.background=element_rect(colour="white", fill="white"),
                plot.background = element_rect(colour="white", fill="white")
            )
        graphico04
    }, height = 400)
    # RENTA MEDIA
    output$ingreso <- renderPlot({
        if(is.null(estaComunidad())){
            elLugarS3 <- "101"
        } else {
            elLugarS3 <- estaComunidad()
        }
        ingresoAnual <- rentaMediaS %>% 
            dplyr::filter(id==elLugarS3) %>% 
            dplyr::select(starts_with("RMH")) %>% 
            gather("Annus","Ingreso",1:ncol(.)) %>% 
            mutate(Year=as.numeric(substr(Annus, nchar(Annus)-4+1, nchar(Annus))),
                   Sector="Sección") 
        ingresoHuescar <- rentaMediaHuescar %>% 
            dplyr::select(starts_with("RMH")) %>% 
            gather("Annus","Ingreso",1:ncol(.)) %>% 
            mutate(Year=as.numeric(substr(Annus, nchar(Annus)-4+1, nchar(Annus))),
                   Sector="Huéscar")
        ingresoAnual <- rbind(ingresoAnual,ingresoHuescar)
        graphico05 <- ggplot(ingresoAnual, aes(x=Year,y=Ingreso))+
            geom_line(aes(color=Sector,linetype=Sector),size=2)+ 
            scale_x_continuous(breaks = seq(2015, 2017, by = 1))+
            labs(title="Renta media por hogar en €"
                 #, caption="Fuente: INE"
            )+
            ylim(min(ingresoAnual$Ingreso),max(ingresoAnual$Ingreso)+350)+
            #xlim(min(ingresoAnual$Year)-0.25, max(ingresoAnual$year+0.25))+
            geom_text(aes(label=Ingreso),vjust = -1.5)+
            scale_color_manual(values = c("#024959", "#8c0303"))+
            theme(axis.text.x=element_text(face="bold", colour="black", size=12),
                  axis.text.y=element_blank(),
                  axis.title=element_blank(),
                  axis.ticks=element_blank(),
                  panel.grid=element_blank(),
                  panel.border=element_blank(),
                  panel.background=element_rect(colour="#d9b29c", fill="#d9b29c"),
                  plot.title = element_text(size=14,face="bold"),
                  plot.background = element_rect(colour="#d9b29c", fill="#d9b29c"),
                  legend.position = "bottom",
                  legend.text = element_text(size=12, face="bold"),
                  legend.title = element_blank(),
                  legend.background = element_rect(fill="#d9b29c"),
                  legend.key = element_rect(fill="#d9b29c",color = NA)
            )
        graphico05
    }, height = 250)
    # INGRESO POR TRAMOS DE EUROS
    output$tramos <- renderPlot({
        if(is.null(estaComunidad())){
            elLugarS3 <- "101"
        } else {
            elLugarS3 <- estaComunidad()
        }
        colorTramos <- c("#024959","#bf5b04","#a66249","#8c0303")
        tramosIngreso <- ingresoTramos %>% 
            dplyr::filter(id==elLugarS3) %>% 
            dplyr::select(ends_with("2017")) %>% 
            dplyr::select(starts_with("menos")) 
        tramosIngresoHuescar <- ingresoTramosHuescar %>% 
            dplyr::select(ends_with("2017")) %>% 
            dplyr::select(starts_with("menos")) 
        tramosText <- c("menos de\n5000€", "de 5001\na 7500€", "de 7501\na 10000€", "más de\n10001€")
        tramosSecc <- c(tramosIngreso$menos50002017, tramosIngreso$menos75002017-tramosIngreso$menos50002017,tramosIngreso$menos100002017-tramosIngreso$menos75002017, 100-tramosIngreso$menos100002017)
        tramosHues <- c(tramosIngresoHuescar$menos50002017, tramosIngresoHuescar$menos75002017-tramosIngresoHuescar$menos50002017,tramosIngresoHuescar$menos100002017-tramosIngresoHuescar$menos75002017, 100-tramosIngresoHuescar$menos100002017)
        tramosGraph <- cbind("Indicador"=tramosText,"Seccion"=tramosSecc,"Huescar"=tramosHues)
        tramosGraph <- as.data.frame(tramosGraph)
        tramosGraph$Seccion <- as.numeric(as.character(tramosGraph$Seccion))
        tramosGraph$Huescar <- as.numeric(as.character(tramosGraph$Huescar))
        tramosGraph <- pivot_longer(tramosGraph,-Indicador,names_to = "Sector",values_to = "Tramos")
        tramosColor <- tibble(tramosText,colorTramos)
        tramosGraph$Indicador <- factor(tramosGraph$Indicador,levels=unique(tramosColor$tramosText))
        tramosGraph$Indicador <- fct_rev(tramosGraph$Indicador)
        tramosSeccCum <- cumsum(tramosSecc)
        tramosHuesCum <- cumsum(tramosHues)
        tramosCUMSUM <- c(matrix(c(tramosSeccCum, tramosHuesCum), 2, byrow = T))
        tramosGraph$cumulative <- tramosCUMSUM
        tramosGraph$Positio <- tramosGraph$cumulative-(tramosGraph$Tramos*0.5)
        graphico06 <- ggplot(tramosGraph, aes(x=Sector,y=Tramos,fill=Indicador))+
            geom_col()+
            geom_text(aes(y=tramosGraph$Positio,label=Tramos), color="white", fontface="bold")+
            labs(
                title="Distribuión del Ingreso (Porcentaje)"
            )+
            scale_fill_manual(values = tramosColor$colorTramos)+
            coord_flip()+
            theme(
                plot.title = element_text(size = 14, face = "bold", color="black"),
                legend.position = "right",
                axis.title = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_text(face = "bold", color = "black"),
                panel.grid=element_blank(),
                panel.border=element_blank(),
                panel.background=element_rect(colour="white", fill="white"),
                plot.background = element_rect(colour="white", fill="white"),
                legend.title = element_blank()
            )
        graphico06
    }, height = 150)
    # ELECCION MUNICIPAL 2019
    output$mun2019 <- renderPlot({
        if(is.null(estaComunidad())){
            elLugarS3 <- "101"
        } else {
            elLugarS3 <- estaComunidad()
        }
        graphMun2019 <- huescarS20190428 %>%
            dplyr::filter(ID2019==elLugarS3) %>% 
            dplyr::select(2:14) %>% 
            gather("Partido", "Votos", 1:13) %>% 
            mutate(Porciento=round((Votos/sum(Votos)*100),2)) %>% 
            arrange(desc(Votos)) %>% 
            mutate(Decisor=ifelse(as.numeric(row.names(.))<6,row.names(.),6)) %>% 
            mutate(Partido=ifelse(Decisor<6,Partido,"Otros")) %>% 
            group_by(Partido) %>% 
            summarise(Votos=sum(Votos),Porciento=sum(Porciento))
        graphMun2019 <- left_join(graphMun2019,partyColor, by=c("Partido"="nominaColor"))
        colorMun <- setNames(as.character(graphMun2019$ideaColor), graphMun2019$Partido)
        graphico01 <- ggplot(graphMun2019, aes(x=reorder(Partido, -Porciento), y=Porciento, fill=Partido, label=Porciento)) +
            geom_col()+
            geom_text(aes(y=10),size=8, color="black",fontface="bold")+
            scale_fill_manual(values = colorMun)+
            theme(axis.text.x=element_text(face="bold", colour="black", size=12),
                  axis.text.y=element_blank(),
                  axis.title=element_blank(),
                  axis.ticks=element_blank(),
                  panel.grid=element_blank(),
                  panel.border=element_blank(),
                  panel.background=element_rect(colour="white", fill="white"),
                  plot.background = element_rect(colour="white", fill="white"),
                  legend.position = "none")
        
        graphico01
    }, height = 250)
    # ELECCION GENERAL 2019
    output$gen022019 <- renderPlot({
        if(is.null(estaComunidad())){
            elLugarS3 <- "101"
        } else {
            elLugarS3 <- estaComunidad()
        }
        graphGen2019 <- huescarS20191110 %>%
            dplyr::filter(ID2019==elLugarS3) %>% 
            dplyr::select(2:15) %>% 
            gather("Partido", "Votos", 1:14) %>% 
            mutate(Porciento=round((Votos/sum(Votos)*100),2)) %>% 
            arrange(desc(Votos)) %>% 
            mutate(Decisor=ifelse(as.numeric(row.names(.))<6,row.names(.),6)) %>% 
            mutate(Partido=ifelse(Decisor<6,Partido,"Otros")) %>% 
            group_by(Partido) %>% 
            summarise(Votos=sum(Votos),Porciento=sum(Porciento))
        graphGen2019 <- left_join(graphGen2019,partyColor, by=c("Partido"="nominaColor"))
        colorGen <- setNames(as.character(graphGen2019$ideaColor), graphGen2019$Partido)
        graphico02 <- ggplot(graphGen2019, aes(x=reorder(Partido, -Porciento), y=Porciento, fill=Partido, label=Porciento)) +
            geom_col()+
            geom_text(aes(y=10),size=8, color="black",fontface="bold")+
            scale_fill_manual(values = colorGen)+
            theme(axis.text.x=element_text(face="bold", colour="black", size=12),
                  axis.text.y=element_blank(),
                  axis.title=element_blank(),
                  axis.ticks=element_blank(),
                  panel.grid=element_blank(),
                  panel.border=element_blank(),
                  panel.background=element_rect(colour="white", fill="white"),
                  #plot.title = element_text(size=20,face="bold"),
                  plot.background = element_rect(colour="white", fill="white"),
                  legend.position = "none")
        
        graphico02
    }, height = 250)
    # COMPARACIÓN
    output$comparo <- renderPlot({
        #AÑADIDO
        if(is.null(estaComunidad())){
            elLugarS3 <- "101"
        } else {
            elLugarS3 <- estaComunidad()
        }
        graphMun2019 <- huescarS20190428 %>%
            dplyr::filter(ID2019==elLugarS3) %>% 
            dplyr::select(2:14) %>% 
            gather("Partido", "Votos", 1:13) %>% 
            mutate(Porciento=round((Votos/sum(Votos)*100),2)) %>% 
            arrange(desc(Votos)) %>% 
            mutate(Decisor=ifelse(as.numeric(row.names(.))<6,row.names(.),6)) %>% 
            mutate(Partido=ifelse(Decisor<6,Partido,"Otros")) %>% 
            group_by(Partido) %>% 
            summarise(Votos=sum(Votos),Porciento=sum(Porciento))
        graphMun2019 <- left_join(graphMun2019,partyColor, by=c("Partido"="nominaColor"))
        graphGen2019 <- huescarS20191110 %>%
            dplyr::filter(ID2019==elLugarS3) %>% 
            dplyr::select(2:15) %>% 
            gather("Partido", "Votos", 1:14) %>% 
            mutate(Porciento=round((Votos/sum(Votos)*100),2)) %>% 
            arrange(desc(Votos)) %>% 
            mutate(Decisor=ifelse(as.numeric(row.names(.))<6,row.names(.),6)) %>% 
            mutate(Partido=ifelse(Decisor<6,Partido,"Otros")) %>% 
            group_by(Partido) %>% 
            summarise(Votos=sum(Votos),Porciento=sum(Porciento))
        graphGen2019 <- left_join(graphGen2019,partyColor, by=c("Partido"="nominaColor"))
        #FIN AÑADIDO
        graphComparar01 <- graphMun2019
        graphComparar01$Partido <- ifelse(graphComparar01$Partido=="PODEMOSIU","PODEMOS",graphComparar01$Partido)
        graphComparar02 <- graphGen2019
        graphComparar02$Partido <- ifelse(graphComparar02$Partido=="PODEMOSIULVCA","PODEMOS",graphComparar02$Partido)
        graphComparar <- full_join(graphComparar01,graphComparar02,by="Partido")
        graphComparar <- graphComparar %>% 
            mutate(VOTOS=Votos.y-Votos.x,
                   PORCENT=round(Porciento.y-Porciento.x,2))
        colorGen <- setNames(as.character(graphComparar$ideaColor.y), graphComparar$Partido)
        graphico03 <- ggplot(graphComparar)+
            geom_col(aes(x=reorder(Partido,-Porciento.x), y=PORCENT, fill=Partido), alpha=0.7)+
            geom_text(aes(x=Partido, y=-1, label=PORCENT),size=6, color="black",fontface="bold")+
            scale_fill_manual(values = colorGen)+
            theme(axis.text.x=element_text(face="bold", colour="black", size=12),
                  axis.text.y=element_blank(),
                  axis.title=element_blank(),
                  axis.ticks=element_blank(),
                  panel.grid=element_blank(),
                  panel.border=element_blank(),
                  panel.background=element_rect(colour="white", fill="white"),
                  #plot.title = element_text(size=20,face="bold"),
                  plot.background = element_rect(colour="white", fill="white"),
                  legend.position = "none")
        graphico03
    }, height = 250)
    # PARTICIPACIÓN MUNICIPAL
    output$PartMun <- renderValueBox({
        if(is.null(estaComunidad())){
            elLugarS3 <- "101"
        } else {
            elLugarS3 <- estaComunidad()
        }
        partMunX <- huescarS20190428 %>% 
            filter(ID2019==elLugarS3) %>% 
            summarise(Participan=round(validos/censoINE*100),2) %>% 
            pull(Participan)
        valueBox(
            subtitle = "PORCENTAJE DE PARTICIPACIÓN (votos válidos/censo del INE)",
            paste0(partMunX, "%"), 
            icon = shiny::icon("bar-chart"),
            color = "blue"
        )
    })
    # PARTICIPACION GENERAL
    output$PartGen <- renderValueBox({
        if(is.null(estaComunidad())){
            elLugarS3 <- "101"
        } else {
            elLugarS3 <- estaComunidad()
        }
        partGenX <- huescarS20191110 %>% 
            filter(ID2019==elLugarS3) %>% 
            summarise(Participan=round(validos/censoINE*100),2) %>% 
            pull(Participan)
        valueBox(
            subtitle = "PORCENTAJE DE PARTICIPACIÓN (votos válidos/censo del INE)",
            paste0(partGenX, "%"), 
            icon = shiny::icon("bar-chart"),
            color = "red"
        )
    })
    # PARTICIPACION DIFERENCIA
    output$PartDif <- renderValueBox({
        if(is.null(estaComunidad())){
            elLugarS3 <- "101"
        } else {
            elLugarS3 <- estaComunidad()
        }
        partMunX <- huescarS20190428 %>% 
            filter(ID2019==elLugarS3) %>% 
            summarise(Participan=round(validos/censoINE*100),2) %>% 
            pull(Participan)
        partGenX <- huescarS20191110 %>% 
            filter(ID2019==elLugarS3) %>% 
            summarise(Participan=round(validos/censoINE*100),2) %>% 
            pull(Participan)
        diferencia <- partGenX-partMunX
        valueBox(
            subtitle = "DIFERENCIA EN LA PARTICIPACIÓN (General - Municipal)",
            paste0(diferencia, "%"), 
            icon = shiny::icon("bar-chart"),
            color = "green"
        )
    })
    # PESTAÑA No. 2.
    # GRAFICO DE RENDIMIENTO
    output$selected_var <- renderPlot({ 
        elPartido <- input$select
        partidoIngreso <- huescarS20191110 %>% 
            dplyr::select(ID2019,all_of(elPartido),validos) %>%
            mutate(estePartido=round(.[,2]/validos*100,2)) %>% 
            left_join(.,rentaMediaS[,c(1,5)], by=c("ID2019"="id"))
        graphico07 <- ggplot(partidoIngreso,aes(x=RMH2017,y=estePartido[,1]))+
            geom_point(colour="#024959", alpha=0.5,size=(partidoIngreso[[2]])/2)+
            geom_point(colour="#8c0303",size=4)+
            geom_text(
                label=paste0("Sec.",substring(partidoIngreso$ID2019,2,3),"\n Votos:",partidoIngreso[[2]]), 
                size=6, 
                fontface="bold",
                color="black")+
            xlim(min(partidoIngreso$RMH2017)-400, max(partidoIngreso$RMH2017)+400)+
            ylim(min(partidoIngreso$estePartido[,1])*0.85, max(partidoIngreso$estePartido[,1])*1.05)+
            ylab("Porcentaje")+
            xlab("Renta media por Hogar 2017")+
            #scale_size(range = c(1,5))+
            theme(
                axis.text = element_text(color = "black", face = "bold", size=10),
                axis.title = element_text(size = 16, face = "bold", colour = "black"),
                legend.position="none",
                panel.grid.major =element_line(colour="#bf5b04"),
                #panel.border=element_rect(colour="#bf5b04"),
                panel.background=element_rect(colour="#bf5b04", fill=alpha("#d9b29c", 0.4)),
                plot.background = element_rect(colour="#bf5b04", fill="white")
        )
        graphico07
    }, height = 400)
}
# Run the application 
shinyApp(ui = ui, server = server)