{'rrr'
  suppressMessages({
    library(plyr); library(dplyr)
    library(crosstalk)
    library(AMR)
    library(data.table)
    library(DT)
    library(ggridges)
    library(lubridate)
    library(plotly)
    library(qicharts2)
    library(rintrojs)
    library(shiny)
    library(shinyBS)
    library(shinycssloaders)
    library(shinydashboard)
    library(shinyjs)
    library(shinyWidgets) 
    library(survival)
    library(ggpubr)
    library(survminer) 
    library(tidyverse)
    library(viridis)
    library(zoo)
    library(readxl)
    # library(xlsx)
    library(aws.s3)
    library(scales)
    library(promises)
    library(future)
    plan(multiprocess)
    library(highcharter)
    library(rpivotTable)
    library(shinyjqui)
    Sys.setenv("AWS_ACCESS_KEY_ID" = 'AKIAUNQVTGJ3IUAN447L',
               "AWS_SECRET_ACCESS_KEY" = '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',
               "AWS_DEFAULT_REGION" = "us-east-2")
    #load('wd.RData')
    #s3load(paste0(uploadwd,'/MapInfo.RData'), bucket = 'bbsscm')
    if(grepl('genomma',tolower(uploadwd))){
      x <- inventoryData$Tienda
      Encoding(x) <- "UTF-8"
      x <- iconv(x, "UTF-8", "UTF-8",sub='') ## replace any non UTF-8 by ''
      inventoryData$Tienda <- x
      inventoryData$Proveedor[grepl('batres',tolower(inventoryData$Tienda)) & !grepl('aguilar',tolower(inventoryData$Tienda))] <- 'Batres'
      inventoryData$Proveedor[inventoryData$Proveedor=='Galeno' & nchar(inventoryData$Tienda)>4] <- 'Cruz Verde'
      inventoryData$Proveedor[grepl('^[G]+[0-9]+$',inventoryData$Tienda)] <- 'Galeno'
    }
    inventoryData$Sucursal <- inventoryData$Tienda
    library(DBI)
    library(odbc)
    library(shinyalert)
    library(plyr)
    library(rintrojs)
    library(readxl)
    #library(xlsx)
    library(htmlwidgets)
    try(rm('con'),silent=TRUE);try({con <- dbConnect(odbc(),Driver      = "FreeTDS",Database    = 'DB_BBS',Uid         = 'blueballoon',Pwd         = 'bbs@db#pass!',Server      = '184.168.194.60',Port        = 1433,TDS_Version = 7.4)},silent = TRUE);if(!exists('con')){con <- dbConnect(odbc(),Driver      = "SQL Server",Database    = 'DB_BBS',Uid         = 'blueballoon',Pwd         = 'bbs@db#pass!',Server      = '184.168.194.60',Port        = 1433,TDS_Version = 7.4)}
    ProveedoresHoyValue <- FALSE
    if(exists('DiasVisitaProveedores')){
      ProveedoresHoyValue <- TRUE
    }
    cliente <- gsub('SCM','',gsub('/','',gsub('/srv/shiny-server/','',wd)))
    if(exists('DiasVisitaProveedores')){
      DiasVisitaProveedores$Cliente <- cliente
      try(DiasVisitaProveedores2 <- DBI::dbGetQuery(con, paste("select * from [blueballoon].[DiasVisitaProveedores] WHERE Cliente =",paste("'",cliente,"'",sep=''))))
      if(exists('DiasVisitaProveedores2')){
        DiasVisitaProveedores <- DiasVisitaProveedores2
      }
      for(j in 1:ncol(DiasVisitaProveedores)){
        DiasVisitaProveedores[,j] <- as.character(DiasVisitaProveedores[,j])
      }
      DiasVisitaProveedores$Si <- as.character(apply( DiasVisitaProveedores[ , 1:ncol(DiasVisitaProveedores)] , 1 , paste , collapse = "-" ))
      DiasVisitaProveedores <- DiasVisitaProveedores[!duplicated(DiasVisitaProveedores$Si),] %>% select(-Si)
      for(j in weekdays(seq.Date(today(),today()+6,'day'))){
        try(DiasVisitaProveedores[,j][is.na(DiasVisitaProveedores[,j])] <- 0,silent=T)
      }
      DiasVisitaProveedores2 <- DiasVisitaProveedores
    }
  })
  inventoryData$Costo[inventoryData$Costo>1000] <- 0
  try(system.time({
    TiendasNuevas <- DBI::dbGetQuery(con, paste("select * from [blueballoon].[TiendasNuevasSCM] where wd ='",wd,"'",sep=''))
    TiendasNuevas <- TiendasNuevas[!duplicated(TiendasNuevas$Tienda),]
    TiendasNuevas <- filter(TiendasNuevas, !Tienda %in% inventoryData$Sucursal)
    for(tn in TiendasNuevas$Tienda){
      ss <- filter(TiendasNuevas, Tienda == tn)$Espejo
      idTN <- filter(inventoryData, Sucursal==ss)
      if(nrow(idTN)>0){
        idTN$Sucursal <- tn
        idTN$Tienda <- tn
        idTN$Inventario <- 0
        idTN$InventarioTotalDias <- round(idTN$Inventario/idTN$Prediccion.1.Semana*7)
        idTN$InventarioTotalDias[idTN$Inventario==0] <- 0
        idTN$InventarioTotalDias[idTN$Prediccion.1.Semana==0] <- idTN$Inventario[idTN$Prediccion.1.Semana==0]
        idTN$NivelDeInventario <- 'SIN VENTA RECURRENTE'
        idTN$NivelDeInventario[idTN$InventarioTotalDias >= idTN$InventarioDeseado] <- 'BUEN INVENTARIO'
        idTN$NivelDeInventario[idTN$InventarioTotalDias > 2*idTN$InventarioDeseado] <- 'EXCESO INVENTARIO'
        idTN$NivelDeInventario[idTN$InventarioTotalDias < idTN$InventarioDeseado] <- 'REORDEN'
        idTN$NivelDeInventario[idTN$InventarioTotalDias < idTN$Lead.Time] <- 'BAJO INVENTARIO'
        idTN$NivelDeInventario[idTN$InventarioTotalDias==0] <- 'QUIEBRE'
        
        idTN$OrdenDeCompra <- 'NO'
        idTN$OrdenDeCompra[idTN$NivelDeInventario=='REORDEN'] <- 'SI'
        idTN$OrdenDeCompra[idTN$NivelDeInventario=='SIN VENTA RECURRENTE'] <- 'REVISAR'
        idTN$OrdenDeCompra[idTN$NivelDeInventario=='QUIEBRE'] <- 'SI - URGENTE'
        idTN$OrdenDeCompra[idTN$NivelDeInventario=='BAJO INVENTARIO'] <- 'SI - URGENTE'
        
        
        idTN <- idTN %>% mutate(CompraUrgente=ifelse(InventarioDeseado>InventarioTotalDias,(InventarioDeseado-InventarioTotalDias)*(Unidades/7),0),
                                CompraPronto=0,#ifelse((Inventario+Transito)<(Prediccion.1.Semana+Unidades), Prediccion.1.Semana-ifelse((Inventario-Unidades)>0,Inventario-Unidades,0)-Transito,0),
                                CompraBien=0)#ifelse((Inventario+Transito)<(Prediccion.2.Semanas+Prediccion.1.Semana+Unidades), Prediccion.2.Semanas-ifelse((Inventario-Unidades)>0,Inventario-Unidades,0)-Transito,0))
        idTN <- idTN %>% mutate(SobreInventario=ifelse(InventarioTotalDias>InventarioDeseado*2, (InventarioTotalDias-InventarioDeseado*2)*Prediccion.1.Semana,0))
        idTN <- idTN[!duplicated(idTN),]
        idTN$CompraUrgente[is.na(idTN$CompraUrgente)] <- 0
        idTN$Minimo[is.na(idTN$Minimo) | idTN$Minimo==0] <- 1
        idTN[(idTN$Inventario==0 & idTN$CompraUrgente==0),'CompraUrgente'] <- idTN[(idTN$Inventario==0 & idTN$CompraUrgente==0),'Minimo']
        idTN$CompraUrgente[idTN$InventarioTotalDias==0 & idTN$CompraUrgente==0] <- idTN$Minimo[idTN$InventarioTotalDias==0 & idTN$CompraUrgente==0]
        idTN$CompraUrgente[is.na(idTN$CompraUrgente)] <- idTN$Minimo[is.na(idTN$CompraUrgente)]
        idTN$CompraUrgente[(idTN$Inventario==0 & idTN$CompraUrgente<idTN$Minimo)] <- idTN$Minimo[(idTN$Inventario<=0 & idTN$CompraUrgente<idTN$Minimo)]
        
        idTN <- mutate(idTN,CompraUrgente=round((CompraUrgente/Minimo)+0.2))
        idTN$CompraUrgente <- idTN$CompraUrgente*idTN$Minimo
        idTN <- idTN[!duplicated(idTN[,c('Producto','Tienda','weekyr')]),]
        idTN$CompraUrgente[idTN$Inventario==0 & idTN$CompraUrgente==0] <- idTN$Minimo[idTN$Inventario==0 & idTN$CompraUrgente==0]
        inventoryData <- rbind(inventoryData, idTN)
      }
    }
    
    try({
      DinCom <- DBI::dbGetQuery(con, paste("select * from [blueballoon].[DinamicasComercialesSCM] where wd ='",wd,"'",sep=''));
      inventoryData <- merge(inventoryData, dplyr::rename(DinCom, Sku=Producto) %>% mutate(FechaFin=ymd(FechaFin)) %>% filter(FechaFin <= today()) %>% select(Tienda, Sku, Aumento), all.x=TRUE)
      inventoryData$Aumento[is.na(inventoryData$Aumento)] <- 0
      try(inventoryData$InventarioDeseado <- inventoryData$InventarioDeseado + inventoryData$Aumento,silent=TRUE)
      
      try(rm('con'),silent=TRUE);try({con <- dbConnect(odbc(),Driver      = "FreeTDS",Database    = 'DB_BBS',Uid         = 'blueballoon',Pwd         = 'bbs@db#pass!',Server      = '184.168.194.60',Port        = 1433,TDS_Version = 7.4)},silent = TRUE);if(!exists('con')){con <- dbConnect(odbc(),Driver      = "ODBC Driver 17 for SQL Server",Database    = 'DB_BBS',Uid         = 'blueballoon',Pwd         = 'bbs@db#pass!',Server      = '184.168.194.60',Port        = 1433,TDS_Version = 7.4)}
      # TiendasNuevas <- data.frame(Tienda=as.character('Prueba'), Espejo=as.character('F145'), FechaHora=now(), Usuario=as.character('rodrigo'))
      # for(j in 1:ncol(TiendasNuevas)){
      #   TiendasNuevas[,j] <- as.character(TiendasNuevas[,j])
      # }
      # DBI::dbCreateTable(con, 'TiendasNuevasSCM', TiendasNuevas, row.names = NULL)
      # DBI::dbAppendTable(con, 'TiendasNuevasSCM', TiendasNuevas, row.names = NULL)
      inventoryData$Costo[inventoryData$Costo>1000] <- 0
      try(system.time({
        inventoryData$InventarioTotalDias <- round(inventoryData$Inventario/inventoryData$Prediccion.1.Semana*7)
        inventoryData$InventarioTotalDias[inventoryData$Inventario==0] <- 0
        inventoryData$InventarioTotalDias[inventoryData$Prediccion.1.Semana==0] <- inventoryData$Inventario[inventoryData$Prediccion.1.Semana==0]
        inventoryData$NivelDeInventario <- 'SIN VENTA RECURRENTE'
        inventoryData$NivelDeInventario[inventoryData$InventarioTotalDias >= inventoryData$InventarioDeseado] <- 'BUEN INVENTARIO'
        inventoryData$NivelDeInventario[inventoryData$InventarioTotalDias > 2*inventoryData$InventarioDeseado] <- 'EXCESO INVENTARIO'
        inventoryData$NivelDeInventario[inventoryData$InventarioTotalDias < inventoryData$InventarioDeseado] <- 'REORDEN'
        inventoryData$NivelDeInventario[inventoryData$InventarioTotalDias < inventoryData$Lead.Time] <- 'BAJO INVENTARIO'
        inventoryData$NivelDeInventario[inventoryData$InventarioTotalDias==0] <- 'QUIEBRE'
        
        inventoryData$OrdenDeCompra <- 'NO'
        inventoryData$OrdenDeCompra[inventoryData$NivelDeInventario=='REORDEN'] <- 'SI'
        inventoryData$OrdenDeCompra[inventoryData$NivelDeInventario=='SIN VENTA RECURRENTE'] <- 'REVISAR'
        inventoryData$OrdenDeCompra[inventoryData$NivelDeInventario=='QUIEBRE'] <- 'SI - URGENTE'
        inventoryData$OrdenDeCompra[inventoryData$NivelDeInventario=='BAJO INVENTARIO'] <- 'SI - URGENTE'
        
        
        inventoryData <- inventoryData %>% mutate(CompraUrgente=ifelse(InventarioDeseado>InventarioTotalDias,(InventarioDeseado-InventarioTotalDias)*(Unidades/7),0),
                                                  CompraPronto=0,#ifelse((Inventario+Transito)<(Prediccion.1.Semana+Unidades), Prediccion.1.Semana-ifelse((Inventario-Unidades)>0,Inventario-Unidades,0)-Transito,0),
                                                  CompraBien=0)#ifelse((Inventario+Transito)<(Prediccion.2.Semanas+Prediccion.1.Semana+Unidades), Prediccion.2.Semanas-ifelse((Inventario-Unidades)>0,Inventario-Unidades,0)-Transito,0))
        inventoryData <- inventoryData %>% mutate(SobreInventario=ifelse(InventarioTotalDias>InventarioDeseado*2, (InventarioTotalDias-InventarioDeseado*2)*Prediccion.1.Semana,0))
        inventoryData <- inventoryData[!duplicated(inventoryData),]
        inventoryData$CompraUrgente[is.na(inventoryData$CompraUrgente)] <- 0
        inventoryData$Minimo[is.na(inventoryData$Minimo) | inventoryData$Minimo==0] <- 1
        inventoryData[(inventoryData$Inventario==0 & inventoryData$CompraUrgente==0),'CompraUrgente'] <- inventoryData[(inventoryData$Inventario==0 & inventoryData$CompraUrgente==0),'Minimo']
        inventoryData$CompraUrgente[inventoryData$InventarioTotalDias==0 & inventoryData$CompraUrgente==0] <- inventoryData$Minimo[inventoryData$InventarioTotalDias==0 & inventoryData$CompraUrgente==0]
        
        inventoryData$Costo[is.na(inventoryData$Costo)] <- 0
        inventoryData$CompraUrgente[is.na(inventoryData$CompraUrgente)] <- inventoryData$Minimo[is.na(inventoryData$CompraUrgente)]
        inventoryData$CompraUrgente[(inventoryData$Inventario==0 & inventoryData$CompraUrgente<inventoryData$Minimo)] <- inventoryData$Minimo[(inventoryData$Inventario<=0 & inventoryData$CompraUrgente<inventoryData$Minimo)]
        inventoryData$Costo <- round(inventoryData$Costo,2)
        # if(file.exists('tiendasactivas.RData')){ #tiendasactivas.R
        #   source('tiendasactivas.R')
        #   inventoryData <- filter(inventoryData,Sucursal %in% unique(substring(tiendasactivas,1,4)))
        # }
        inventoryData <- mutate(inventoryData,CompraUrgente=round((CompraUrgente/Minimo)+0.2))
        inventoryData$CompraUrgente <- inventoryData$CompraUrgente*inventoryData$Minimo
        
        inventoryData <- inventoryData[!duplicated(inventoryData[,c('Producto','Tienda','weekyr')]),]
        inventoryData$CompraUrgente[inventoryData$Inventario==0 & inventoryData$CompraUrgente==0] <- inventoryData$Minimo[inventoryData$Inventario==0 & inventoryData$CompraUrgente==0]
      }))
      # Inactivos <- data.frame(Tienda=as.character('Prueba'), Producto="7401001500316", FechaHora=now(), Usuario=as.character('rodrigo'))
      # for(j in 1:ncol(Inactivos)){
      #   Inactivos[,j] <- as.character(Inactivos[,j])
      # }
      Inactivos <- DBI::dbGetQuery(con, paste("select * from [blueballoon].[InactivosSCM] where wd ='",wd,"'",sep=''))
      inventoryData <- inventoryData %>% mutate(Si=paste(Sucursal,Sku)) %>% filter(!Si %in% paste(Inactivos$Tienda, Inactivos$Producto)) %>% select(-Si)
    })
    
  }))
  Inactivos <- DBI::dbGetQuery(con, paste("select * from [blueballoon].[InactivosSCM] where wd ='",wd,"'",sep=''))
  inventoryData <- inventoryData %>% mutate(Si=paste(Sucursal,Sku)) %>% filter(!Si %in% paste(Inactivos$Tienda, Inactivos$Producto)) %>% select(-Si)
  for(dfi in ls()){
    dfig <- get(dfi)
    if(class(dfig)=='data.frame'){
      for(j in 1:ncol(dfig)){
        if(class(dfig[,j]) %in% c('factor','character')){
          dfig[,j] <- iconv(dfig[,j], 'UTF-8', 'ASCII')
        }
      }
      assign(dfi,dfig)
    }
  }
}
dplyr_solution <- function(dat, vars){
  .vars <- paste(vars, collapse = ",")
  vars2 <- c(vars, 'Inventario')
  .vars2 <- paste(vars2, sep="-")
  result <- dat %>%
    mutate_(xaxis = paste0('paste(', .vars, ', sep="-")'),
            info = paste0('paste(', .vars2, ', sep="-")'))
  return(result)
}

ui <- dashboardPage(
  
  skin = "black",
  title = "Realize",
  
  # HEADER ------------------------------------------------------------------
  
  dashboardHeader(
    title =''
  ),
  
  # SIDEBAR -----------------------------------------------------------------
  
  dashboardSidebar(
    span(img(src = base64enc::dataURI(file="scm.png", mime="image/png"), height = 125)),
    tags$br(),collapsed = F,
    #div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
    
    sidebarMenu(
      menuItem(text = 'Resumen', tabName = 'resumen'),
      menuItem(text = 'Análisis',tabName =  'analisis'),
      menuItem(text = 'Inventarios',tabName =  'inventarios'),
      menuItem(text = 'Compras',tabName =  'compras',
               menuSubItem('Resumen', tabName = 'resumenCompras', icon = NULL),
               menuSubItem('Programación', tabName = 'programacionCompras',icon = NULL)),
      menuItem(text = 'Ajustes', tabName = 'ajustes',
               menuSubItem('Generales', tabName = 'generalesAjustes', icon = NULL),
               menuSubItem('Inventarios', tabName = 'inventariosAjustes', icon = NULL),
               menuSubItem('Compras', tabName = 'comprasAjustes', icon = NULL))
      
    ) #sidebar menu
  ),
  
  
  # BODY --------------------------------------------------------------------
  
  dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet", 
        type = "text/css", 
        href = "style.css"),
      HTML(
        "
        <style>
        [asideMenuToggler]='false'
        </style>
        <style>
        #sidebarCollapsed {
            background-color: #3A56A1;
        }
        </style>
        <style>.navbar-brand-img{
        min-height: 100px;
        }
        h6{
        font-size: 18px;
        }
        </style>
        <style>
         h1 {
        font-family: 'Roboto';
        font-weight: bold;
        font-size:40px;
        color: #3a56a1;
         }
      p {
        font-family: 'Roboto';
        font-weight: regular;
        font-size:16px;
        color: #333333;
      }
       h5 {
        font-family: 'Roboto';
        font-weight: regular;
        font-size:16px;
        
       }
      .btn.checkbtn.btn-primary {
        background-color:#ebebeb;
        border-color:#ebebeb;margin:5px;
        color: #333333;width:100%;
      }
        </style>
        <script>
        var socket_timeout_interval
        var n = 0
        $(document).on('shiny:connected', function(event) {
        socket_timeout_interval = setInterval(function(){
        Shiny.onInputChange('count', n++)
        }, 15000)
        });
        $(document).on('shiny:disconnected', function(event) {
        clearInterval(socket_timeout_interval)
        });
        </script>
        "
      ),
      tags$style("#topScroll > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody {
                 transform:rotateX(180deg);
                 }
                 #topScroll > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody table{
                 transform:rotateX(180deg);
                 }")
    ),
    
    useShinyjs(),
    introjsUI(),
    
    # MAIN BODY ---------------------------------------------------------------
    textOutput("keepAlive"),
    
    tags$br(),
    tabItems(
      #   textOutput("clicked"),
      #   textOutput("clickedplus"),
      #   # jqui_sortable(
      tabItem('resumen',
              fluidRow(
                column(width = 12,
                       column(12,
                              div(style = "float: right;",
                                  uiOutput('filtrosUI'),
                                  tags$br(),
                                  
                              )),
                       
                       column(width = 6,
                              uiOutput('boxNivelesGlobal')),
                       column(width = 6,
                              uiOutput('boxServicioTienda')),
                       column(width = 6,
                              uiOutput('boxOportunidadInfoBox')),
                       column(width = 6,
                              infoBox(
                                title = "Buen Nivel de Inventario",
                                value = textOutput("comprabien"), #%>% withSpinner(type = 4,color = "#d33724",size = 0.7),
                                width=NULL,
                                icon = icon("store") 
                              )),
                       column(width = 12,
                              uiOutput('box9'))
                       
                )
              )
      ),
      tabItem(tabName = 'analisis',
              fluidRow(
                column(12,
                       div(style = "float: right;",
                           uiOutput('filtrosUI2')
                           
                       ))
              )
      ),
      tabItem(tabName = 'inventarios',
              fluidRow(
                column(12,
                       div(style = "float: right;",
                           uiOutput('filtrosUI3')
                           
                           
                       ),
                       column(width = 12,
                              infoBox(
                                title = "Buen Nivel de Inventario",
                                value = textOutput("comprabien2"), #%>% withSpinner(type = 4,color = "#d33724",size = 0.7),
                                width=6,
                                icon = icon("store") 
                              )),
                       tags$br(),
                       column(width = 12,
                              uiOutput('box5')
                       )
                )
              )
      ),
      tabItem(tabName = 'resumenCompras',
              fluidRow(
                column(width = 12,
                       fluidRow(
                         infoBox(
                           title = "Compra Urgente",
                           value = textOutput("compraurgente") ,#%>% withSpinner(type = 4,color = "#d33724",size = 0.7),
                           width=4,
                           icon = icon("store") 
                         ),
                         infoBox(
                           title = "Compra Pronto",
                           value = textOutput("comprapronto"), #%>% withSpinner(type = 4,color = "#d33724",size = 0.7),
                           width=4,
                           icon = icon("store") 
                         ),
                       ),
                       tags$br(),tags$br(),
                       fluidRow(
                         div(
                           style = "float: right;",
                           dropdown(width = '600px',status = 'primary',
                                    size = "default",label = 'FILTRAR',
                                    icon = icon("gear", class = "opt"), 
                                    up = FALSE,
                                    right = T,
                                    fluidRow(
                                      column(width = 12,
                                             checkboxGroupInput("Tipo", "Tipo de Compra:",
                                                                c("Urgente" = "CompraUrgente",
                                                                  "Pronto" = "CompraPronto",
                                                                  "Bien" = "CompraBien"), inline = TRUE),
                                             selectInput("Inventario", "Nivel de Inventario:",
                                                         c("Todos"="Todos",
                                                           "Sin Inventario" = "Sin",
                                                           "Con Inventario" = "Con"), selected ="Todos"),
                                             column(
                                               numericInput('LimitePresupuesto','Limite de Presupuesto', min=0, value = 0),
                                               width=6
                                             ),
                                             column(
                                               numericInput('DiasDeInventarioAdicionales','Dias De Inventario Adicionales', min=0, value = 0),
                                               width=6
                                             ),
                                             column(
                                               textInput('TipoOrden','Tipo de Orden de Compra',value = 'ORDEN DE COMPRA'),
                                               width=6
                                             ),
                                             column(
                                               textInput('TipoPrecio','Tipo de Precio',value = ''),
                                               width=6
                                             ),
                                             column(
                                               uiOutput('AutorizadoPorUI'),
                                               width=6
                                             ),
                                             column(
                                               textInput('Observaciones','Observaciones',value = ''),
                                               width=6
                                             ),
                                             column(
                                               checkboxInput('DescargaPorProveedor','Descargar por proveedor',value = TRUE),
                                               width=6
                                             ),
                                             column(
                                               pickerInput('ProveedoresHoy','Dias de Pedido',multiple=T,
                                                           options = list(
                                                             `actions-box` = TRUE, `live-search`=TRUE, liveSearchStyle	='contains',
                                                             liveSearchNormalize=TRUE),
                                                           choices=c('Lunes','Martes','Miercoles','Jueves','Viernes','Sabado','Todos'),
                                                           selected=c('Lunes','Martes','Miercoles','Jueves','Viernes','Sabado','Todos')[wday(now()-hours(8))-1]
                                               ),
                                               # checkboxInput('ProveedoresHoy','Proveedores que reciben ordenes hoy',value = ProveedoresHoyValue),
                                               width=6
                                             ),
                                             pickerInput('ProveedorGlobalCompras','Cadena',multiple=T,
                                                         options = list(
                                                           `actions-box` = TRUE, `live-search`=TRUE, liveSearchStyle	='contains', liveSearchNormalize=TRUE), choices=unique(inventoryData$Proveedor), selected=unique(inventoryData$Proveedor)),
                                             pickerInput('TiendaGlobalCompras','Tienda',multiple=T,
                                                         options = list(
                                                           `actions-box` = TRUE, `live-search`=TRUE, liveSearchStyle	='contains', liveSearchNormalize=TRUE), choices=unique(inventoryData$Tienda), selected=unique(inventoryData$Tienda)),
                                             pickerInput('ProductoGlobalCompras','Producto',multiple=T,
                                                         options = list(
                                                           `actions-box` = TRUE, `live-search`=TRUE, liveSearchStyle	='contains', liveSearchNormalize=TRUE), choices=unique(inventoryData$Producto), selected=unique(inventoryData$Producto)),
                                             pickerInput('CategoriaGlobalCompras','Categoria',multiple=T,
                                                         options = list(
                                                           `actions-box` = TRUE, `live-search`=TRUE, liveSearchStyle	='contains', liveSearchNormalize=TRUE), choices=unique(inventoryData$Categoria), selected=unique(inventoryData$Categoria))
                                             
                                      )
                                    )
                           )
                         )),
                       tags$br(), tags$br(),
                       column(
                         width=12,
                         uiOutput('boxcomprasData')
                       )
                )
              )
      ),
      tabItem(tabName = 'programacionCompras',
              fluidRow(
                column(width = 12,
                       infoBox(
                         title = "Costo Total de Orden de Compra",
                         value = textOutput("CostoCompra"), #%>% withSpinner(type = 4,color = "#d33724",size = 0.7),
                         width=6,
                         icon = icon("tag")
                       ),
                       tags$br(),tags$br(),
                       column(width = 12,
                              uiOutput('boxordenesData'),
                              
                       )
                )
              )),
      tabItem(tabName = 'generalesAjustes'),
      tabItem(tabName = 'inventariosAjustes'),
      tabItem(tabName = 'comprasAjustes',
              uiOutput('boxcomprasAjustes'))
      
    )
  )
  
)

server <- function(input, output, session) {

  output$filtrosUI <- renderUI({
    dropdown(label = 'FILTRAR',status = 'primary',
             fluidRow(
               column(12,
               pickerInput('ProveedorGlobal','Cadena',multiple=T,
                           options = list(
                             `actions-box` = TRUE, `live-search`=TRUE, liveSearchStyle	='contains', liveSearchNormalize=TRUE), choices=unique(inventoryData$Proveedor), selected=unique(inventoryData$Proveedor)),
               pickerInput('TiendaGlobal','Tienda',multiple=T,
                           options = list(
                             `actions-box` = TRUE, `live-search`=TRUE, liveSearchStyle	='contains', liveSearchNormalize=TRUE), choices=unique(inventoryData$Tienda), selected=unique(inventoryData$Tienda)),
               pickerInput('ProductoGlobal','Producto',multiple=T,
                           options = list(
                             `actions-box` = TRUE, `live-search`=TRUE, liveSearchStyle	='contains', liveSearchNormalize=TRUE), choices=unique(inventoryData$Producto), selected=unique(inventoryData$Producto)),
               pickerInput('CategoriaGlobal','Categoria',multiple=T,
                           options = list(
                             `actions-box` = TRUE, `live-search`=TRUE, liveSearchStyle	='contains', liveSearchNormalize=TRUE), choices=unique(inventoryData$Categoria), selected=unique(inventoryData$Categoria))
               
             )
            ),
             size = "default",right = T,width = '475px',
             icon = icon("gear", class = "opt"), 
             up = FALSE
    )
  })
  output$filtrosUI2 <- renderUI({
    dropdown(label = 'FILTRAR',status = 'primary',
             fluidRow(
               pickerInput('ProveedorGlobal','Cadena',multiple=T,
                           options = list(
                             `actions-box` = TRUE, `live-search`=TRUE, liveSearchStyle	='contains', liveSearchNormalize=TRUE), choices=unique(inventoryData$Proveedor), selected=unique(inventoryData$Proveedor)),
               pickerInput('TiendaGlobal','Tienda',multiple=T,
                           options = list(
                             `actions-box` = TRUE, `live-search`=TRUE, liveSearchStyle	='contains', liveSearchNormalize=TRUE), choices=unique(inventoryData$Tienda), selected=unique(inventoryData$Tienda)),
               pickerInput('ProductoGlobal','Producto',multiple=T,
                           options = list(
                             `actions-box` = TRUE, `live-search`=TRUE, liveSearchStyle	='contains', liveSearchNormalize=TRUE), choices=unique(inventoryData$Producto), selected=unique(inventoryData$Producto)),
               pickerInput('CategoriaGlobal','Categoria',multiple=T,
                           options = list(
                             `actions-box` = TRUE, `live-search`=TRUE, liveSearchStyle	='contains', liveSearchNormalize=TRUE), choices=unique(inventoryData$Categoria), selected=unique(inventoryData$Categoria))
               
             ),
             size = "default",right = T,
             icon = icon("gear", class = "opt"), 
             up = FALSE
    )
  })
  output$filtrosUI3 <- renderUI({
    dropdown(label = 'FILTRAR',status = 'primary',
             fluidRow(
               column(12,
               selectInput('ColYbox5', label = tags$b(tags$span('Agrupacion', style = "font-size:18px")),
                           choices = c('Producto','Categoria','Proveedor', 'Tienda'),  
                           selected = 'Producto'),  
             )),
             size = "default",right = T,
             icon = icon("gear", class = "opt"), 
             up = FALSE
    )
  })
  # DEFINE SETS -------------------------------------------------  
  
  # UI - GENERAL --------------------------------------------------------------
  
  
  #show intro modal
  
  
  observeEvent(input$intro,{
    removeModal()
  })
  
  # show intro tour
  observeEvent(input$intro,
               introjs(session, options = list(scrollX = TRUE, "nextLabel" = "Continue",
                                               "prevLabel" = "Previous",
                                               "doneLabel" = "Alright. Let's go"))
  )
  
  # use action buttons as tab selectors
  update_all <- function(x) {
    updateSelectInput(session, "tab",
                      choices = c("", "comercial", "Antimicrobial consumption", "AnalisisCompras", "Outcome"),
                      label = "",
                      selected = x
    )
  }
  
  
  # update confirm button
  
  observeEvent(input$confirm, {
    updateButton(
      session, 
      inputId = "confirm", 
      label = "Aplicar Filtro", 
      icon = icon("bar-chart-o"), 
      style = "primary")
  })
  
  observeEvent("", {
    hide("tab")
  })
  
  # DYNAMIC RENDER RULES ----------------------------------------------------
  
  observeEvent("", {
    hide('keepAlive')
    hide("AnalisisCompras_panel")
    hide("comercial_panel")
    hide("odontologos_panel")
    hide('resumen_panel')
    # shinyjs::show("comercial_panel")
  }, once = TRUE)
  
  observeEvent(input$comercial, {
    hide("AnalisisCompras_panel")
    hide("odontologos_panel")
    hide('resumen_panel')
    shinyjs::show("comercial_panel")
  })
  
  observeEvent(input$AnalisisCompras, {
    print('AnalisisCompras')
    hide("comercial_panel")
    hide('resumen_panel')
    hide("odontologos_panel")
    shinyjs::show("AnalisisCompras_panel")
  })
  
  observeEvent(input$odontologos, {
    hide("AnalisisCompras_panel")
    hide('resumen_panel')
    hide("comercial_panel")
    shinyjs::show("odontologos_panel")
  })
  
  observeEvent(input$resumen, {
    hide("AnalisisCompras_panel")
    hide('odontologos_panel')
    hide("comercial_panel")
    shinyjs::show("resumen_panel")
  })
  # show active button with color
  
  observeEvent(input$tab, {
    x <- input$tab
    updateButton(session, "comercial", style = {
      if (x == "comercial") {
        paste("warning")
      } else {
        paste("success")
      }
    })
    updateButton(session, "AnalisisCompras", style = {
      if (x == "AnalisisCompras") {
        paste("warning")
      } else {
        paste("success")
      }
    })
  })
  
  # REACTIVE DF
  # Supply Chain ------------------------------------------------------------
  inventoryDataUpdate <- reactive({
    try(
      if(exists('DiasVisitaProveedores')){
        daysFilter <- c(unique(weekdays(seq.Date(ymd('2020/03/02'),(ymd('2020/03/02')+6), 'day'))),'Todos')[which(c('Lunes','Martes','Miercoles','Jueves','Viernes','Sabado','Todos') %in% input$ProveedoresHoy)]
        dayToday <- daysFilter
        #print(dayToday)
        if(!'Todos' %in% dayToday){
          DiasVisitaProveedores <- DiasVisitaProveedores[complete.cases(DiasVisitaProveedores),]
          DiasVisitaProveedores <- DiasVisitaProveedores[DiasVisitaProveedores[,dayToday]==1,]
          #print(dim(inventoryData))
          # inventoryData <- inventoryData[with(inventoryData, paste(toupper(gsub("[^[:alnum:]]","",Proveedor)),toupper(gsub("[^[:alnum:]]","",Sucursal)))) %in% with(DiasVisitaProveedores, paste(toupper(gsub("[^[:alnum:]]","",Proveedor)),toupper(gsub("[^[:alnum:]]","",Sucursal)))),]
          
          # TEMPORAL
          # inventoryData2 <- filter(inventoryData, Proveedor %in% DiasVisitaProveedores$Proveedor)
          # if(nrow(inventoryData2)>0){
          #   inventoryData <- inventoryData2
          # }
          # TEMPORAL
          
        }
      }
    )
    #print('idu5')
    try(if(!is.null(input$ResumenFiltrosTable_rows_selected)){
      d <- input$ResumenFiltrosTable_rows_selected
    })
    if(!exists('d')){
      d <- 1:nrow(filtrosDT)
    }
    print(d)
    result <- inventoryData %>% filter(Clasificacion %in% filtrosDT$Clasificacion[d], Proveedor %in% as.character(input$ProveedorGlobal), Tienda %in% as.character(input$TiendaGlobal), Producto %in% as.character(input$ProductoGlobal), Categoria %in% as.character(input$CategoriaGlobal))
    # print(paste('Clasificacion',table(filter(inventoryData, Clasificacion %in% filtrosDT$Clasificacion[d])$Proveedor)))
    # print(paste('Proveedor',table(filter(inventoryData, Proveedor %in% as.character(input$ProveedorGlobal)[d])$Proveedor)))
    # print(paste('Tienda',table(filter(inventoryData, Tienda %in% as.character(input$TiendaGlobal)[d])$Proveedor)))
    # print(paste('Producto',table(filter(inventoryData, Producto %in% as.character(input$ProductoGlobal)[d])$Proveedor)))
    # print(paste('Categoria',table(filter(inventoryData, Categoria %in% as.character(input$CategoriaGlobal)[d])$Proveedor)))
    # print('update start')
    # print(table(filter(inventoryData, Clasificacion %in% filtrosDT$Clasificacion[d])$Proveedor))
    # print(table(filter(inventoryData, Proveedor %in% as.character(input$ProveedorGlobal))$Proveedor))
    # print(table(filter(inventoryData, Tienda %in% as.character(input$TiendaGlobal))$Proveedor))
    # print(table(filter(inventoryData, Producto %in% as.character(input$ProductoGlobal))$Proveedor))
    # print(table(filter(inventoryData, Categoria %in% as.character(input$CategoriaGlobal))$Proveedor))
    # print('update end')
    print(table(result$Proveedor))
    result
  })
  
  output$ResumenFiltrosTable <- renderDataTable({
    datatable(filtrosDT,rownames = F, escape = F,filter = 'top',
              options = list(
                pageLength = 5,scrollX = TRUE, dom = 'tp'
              ))
  })
  
  # niveles de inventario global ----
  
  output$boxNivelesGlobal <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        title = 'Niveles de inventario global',
        id = "boxNivelesGlobal",
        width = NULL,
        height = 450,
        
        tabPanel(
          title = "Grafica",
          tags$br(),
          div(
            withSpinner(
              highchartOutput('boxNivelesGlobalPlot', height = 300) %>% withSpinner(type = 4,color = "#d33724",size = 0.7),
              type = 4,
              color = "#d33724",
              size = 0.7
            ),
            
          )
        ),
        tabPanel(
          title = "Tabla",
          div(
            withSpinner(
              DT::dataTableOutput('boxNivelesGlobalDT') %>% withSpinner(type = 4,color = "#d33724",size = 0.7),
              type = 4,
              color = "#d33724",
              size = 0.7
            ),
            downloadBttn(outputId = "boxNivelesGlobaldtdescarga", label = "", color = 'primary', style = 'material-flat', size = 'xs')
          )
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em;",
          conditionalPanel(
            "input.boxNivelesGlobal == 'Grafica'",
            actionBttn(
              inputId = "NivelesGlobalgraficaplus",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em;",
          conditionalPanel(
            "input.boxNivelesGlobal == 'Tabla'",
            actionBttn(
              inputId = "NivelesGlobaldtplus",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        )
        
      )
    )
  })
  
  
  
  observeEvent((input$NivelesGlobalgraficaplus), {
    showModal(modalDialog(
      highchartOutput('NivelesGlobalPlot2', height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  observeEvent((input$NivelesGlobaldtplus), {
    showModal(modalDialog(
      DT::dataTableOutput('NivelesGlobalDT2', height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  
  
  output$boxNivelesGlobalPlot <- renderHighchart({
    print('Clasificacion Inventario 0')
    # try(trackFun(session$user,'resumen'))
    provs <- filter(provs, Proveedor %in% as.character(input$ProveedorGlobal))
    # inventoryData <- inventoryDataUpdate()
    try(if(!is.null(input$ResumenFiltrosTable_rows_selected)){
      d <- input$ResumenFiltrosTable_rows_selected
    })
    if(!exists('d')){
      d <- 1:nrow(filtrosDT)
    }
    print(d)
    Sivec <- with(inventoryData %>% filter(Clasificacion %in% filtrosDT$Clasificacion[d]), paste(Sku, Tienda))
    inventoryData <- filter(inventoryDataUpdate() %>% mutate(Si=paste(Sku,Tienda))) %>% filter(Si %in% Sivec) %>% select(-Si)
    
    cats <- select(inventoryData, Producto, Categoria, Subcategoria)
    cats <- cats[!duplicated(cats$Producto),]
    cats$Producto <- toupper(gsub("[^[:alnum:]]","",cats$Producto))
    print('Clasificacion Inventario 1')
    try(d <- event_data(event = "plotly_click", source = "subset"))
    if(!exists('d')){
      d <- NULL
    }
    print('Clasificacion Inventario 2')
    print(d)
    # if(input$diasInvResetNivelesGlobal){ 
    #   d <- NULL
    # }
    # if (!is.null(d)){
    #   dkey <- as.character(unlist(d$key))
    #   inventoryData$Cat <- 'SobreInventario'
    #   inventoryData$Cat[inventoryData$InventarioTotalDias<=1] <- 'SinInventario'
    #   inventoryData$Cat[inventoryData$InventarioTotalDias>=2 & inventoryData$InventarioTotalDias<=4] <- 'SubInventario'
    #   inventoryData$Cat[inventoryData$InventarioTotalDias>=5 & inventoryData$InventarioTotalDias<=9] <- 'BuenInventario'
    #   inventoryData$Cat[inventoryData$InventarioTotalDias>=10 & inventoryData$InventarioTotalDias<=15] <- 'LeveSobreInventario'
    #   inventoryData <- filter(inventoryData, Cat %in% dkey)
    #   print(dim(inventoryData))
    #   ppRR$Cat <- 'SobreInventario'
    #   ppRR$Cat[ppRR$DiasInventario<=1] <- 'SinInventario'
    #   ppRR$Cat[ppRR$DiasInventario>=2 & ppRR$DiasInventario<=4] <- 'SubInventario'
    #   ppRR$Cat[ppRR$DiasInventario>=5 & ppRR$DiasInventario<=9] <- 'BuenInventario'
    #   ppRR$Cat[ppRR$DiasInventario>=10 & ppRR$DiasInventario<=15] <- 'LeveSobreInventario'
    #   ppRR <- filter(ppRR, Cat %in% dkey[1]) %>% select(-Cat)
    #   # print(dim(ppRR))
    # }
    print('Clasificacion Inventario 3')
    inventoryData <- filter(inventoryData, Tienda %in% as.character(input$TiendaGlobal), Producto %in% as.character(input$ProductoGlobal))
    ppRR <- filter(ppRR, Tienda %in% as.character(input$TiendaGlobal), Producto %in% as.character(input$ProductoGlobal))
    ppRR$Producto <- toupper(gsub("[^[:alnum:]]","",ppRR$Producto))
    ppRR <- merge(ppRR,cats, all.x=T) # %>% filter(Tienda %in% as.character(input$TiendaGlobal))
    ppRR$Cat <- 'SobreInventario'
    ppRR$Cat[ppRR$DiasInventario<=1] <- 'SinInventario'
    ppRR$Cat[ppRR$DiasInventario>=2 & ppRR$DiasInventario<=4] <- 'SubInventario'
    ppRR$Cat[ppRR$DiasInventario>=5 & ppRR$DiasInventario<=9] <- 'BuenInventario'
    ppRR$Cat[ppRR$DiasInventario>=10 & ppRR$DiasInventario<=15] <- 'LeveSobreInventario'
    ppRR$Color <- 'rgb(254,99,99)'
    ppRR$Color[ppRR$DiasInventario>=2 & ppRR$DiasInventario<=4] <- 'rgb(254,200,99)'
    ppRR$Color[ppRR$DiasInventario>=5 & ppRR$DiasInventario<=9] <- 'rgb(50, 255, 150)'
    ppRR$Color[ppRR$DiasInventario>=10 & ppRR$DiasInventario<=15] <- 'rgb(254,200,99)'
    ppRR$n <- 1
    print('Clasificacion Inventario 4')
    # try(trackFun(session$user,'resumen'))
    mm <- data.frame(table(ppRR$DiasInventario))
    mm$Var1 <- as.numeric(as.character(mm$Var1))
    mm$Color <- 'rgb(254,99,99)'
    mm$Color[mm$Var1>=2 & mm$Var1<=4] <- 'rgb(254,200,99)'
    mm$Color[mm$Var1>=5 & mm$Var1<=9] <- 'rgb(50, 255, 150)'
    mm$Color[mm$Var1>=10 & mm$Var1<=15] <- 'rgb(254,200,99)'
    print('Clasificacion Inventario 3')
    hcData <- table(ppRR$Cat) %>% data.frame()
    plot <- hcData %>%
      hchart(
        "pie", hcaes(x = Var1, y = Freq),
        name = "Cantidad"
      ) %>% hc_colors(c("#58B96E", "#3A56A1", "#D15454", '#304680', '#EDAA42')) 
    output$boxNivelesGlobalPlot <- renderHighchart({
      plot
    })
    output$NivelesGlobalPlot2 <- renderHighchart({
      plot
    })
    #%>% hc_plotOptions(pie = list(color = 'red'))
    dtDF <- mm %>% select(-Color)
    output$boxNivelesGlobalDT <- renderDT({
      datatable(dtDF,
                rownames = F, escape = F,filter = 'top',
                options = list(
                  scrollX = TRUE, scrollY = '175px'
                )
      )
      
    })
    output$boxNivelesGlobalDT2 <- renderDT({
      datatable(dtDF,
                rownames = F, escape = F,filter = 'top',
                options = list(
                  scrollX = TRUE
                )
      )
      
    })
    output$boxNivelesGlobalDT2 <- renderDT({
      datatable(dtDF,
                rownames = F, escape = F,filter = 'top',
                options = list(
                  scrollX = TRUE
                )
      )
      
    })
    output$boxNivelesGlobaldtdescarga <- downloadHandler(
      filename = function(){"NivelesInventarioGlobal.csv"},
      content = function(fname){
        write_csv(dtDF, fname)
      }
    )
    future({plot})
    
  })
  
  
  output$boxServicioTienda <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        title = 'Niveles de servicio por tienda',
        id = "boxServicioTienda",
        width = NULL,
        height = 450,
        
        tabPanel(
          title = "Grafica",
          div(
            withSpinner(
              highchartOutput('boxServicioTiendaPlot', height = 300),
              type = 4,
              color = "#d33724",
              size = 0.7
            ),
            
          )
        ),
        tabPanel(
          title = "Tabla",
          div(
            withSpinner(
              DT::dataTableOutput('boxServicioTiendaDT'),
              type = 4,
              color = "#d33724",
              size = 0.7
            ),
            downloadBttn(outputId = "boxServicioTiendadtdescarga", label = "", color = 'primary', style = 'material-flat', size = 'xs')
          )
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em;",
          conditionalPanel(
            "input.boxServicioTienda == 'Grafica'",
            actionBttn(
              inputId = "boxServicioTiendagraficaplus",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em;",
          conditionalPanel(
            "input.boxServicioTienda == 'Tabla'",
            actionBttn(
              inputId = "boxServicioTiendadtplus",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        )
        
      )
    )
  })
  observeEvent((input$boxServicioTiendagraficaplus), {
    showModal(modalDialog(
      highchartOutput('boxServicioTiendaPlot2', height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  observeEvent((input$boxServicioTiendadtplus), {
    showModal(modalDialog(
      DT::dataTableOutput('boxServicioTiendaDT2', height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  
  output$boxServicioTiendaPlot <- renderHighchart({
    provs <- filter(provs, Proveedor %in% as.character(input$ProveedorGlobal))
    
    inventoryDataTemp <- inventoryDataUpdate()
    if(nrow(inventoryDataTemp) > 0 ){
      inventoryData <- inventoryDataTemp
    }
    inventoryData <- merge(inventoryData, provs)
    ppRR <- merge(ppRR, provs)
    cats <- select(inventoryData, Producto, Categoria, Subcategoria)
    cats <- cats[!duplicated(cats$Producto),]
    cats$Producto <- toupper(gsub("[^[:alnum:]]","",cats$Producto))
    # d <- event_data(event = "plotly_click", source = "subset")
    # if(input$diasInvResetServicioTienda){ 
    #   d <- NULL
    # }
    # if (!is.null(d)){
    #   dkey <- as.character(unlist(d$key))
    #   inventoryData$Cat <- 'SobreInventario'
    #   inventoryData$Cat[inventoryData$InventarioTotalDias<=1] <- 'SinInventario'
    #   inventoryData$Cat[inventoryData$InventarioTotalDias>=2 & inventoryData$InventarioTotalDias<=4] <- 'SubInventario'
    #   inventoryData$Cat[inventoryData$InventarioTotalDias>=5 & inventoryData$InventarioTotalDias<=9] <- 'BuenInventario'
    #   inventoryData$Cat[inventoryData$InventarioTotalDias>=10 & inventoryData$InventarioTotalDias<=15] <- 'LeveSobreInventario'
    #   inventoryData <- filter(inventoryData, Cat %in% dkey)
    #   print(dim(inventoryData))
    #   ppRR$Cat <- 'SobreInventario'
    #   ppRR$Cat[ppRR$DiasInventario<=1] <- 'SinInventario'
    #   ppRR$Cat[ppRR$DiasInventario>=2 & ppRR$DiasInventario<=4] <- 'SubInventario'
    #   ppRR$Cat[ppRR$DiasInventario>=5 & ppRR$DiasInventario<=9] <- 'BuenInventario'
    #   ppRR$Cat[ppRR$DiasInventario>=10 & ppRR$DiasInventario<=15] <- 'LeveSobreInventario'
    #   ppRR <- filter(ppRR, Cat %in% dkey[1]) %>% select(-Cat)
    #   # print(dim(ppRR))
    # }
    inventoryData <- filter(inventoryData, Tienda %in% as.character(input$TiendaGlobal), Producto %in% as.character(input$ProductoGlobal))
    ppRR <- filter(ppRR, Tienda %in% as.character(input$TiendaGlobal), Producto %in% as.character(input$ProductoGlobal))
    ppRR$Producto <- toupper(gsub("[^[:alnum:]]","",ppRR$Producto))
    ppRR <- merge(ppRR,cats, all.x=T) # %>% filter(Tienda %in% as.character(input$TiendaGlobal))
    ppRR$Cat <- 'SobreInventario'
    ppRR$Cat[ppRR$DiasInventario<=1] <- 'SinInventario'
    ppRR$Cat[ppRR$DiasInventario>=2 & ppRR$DiasInventario<=4] <- 'SubInventario'
    ppRR$Cat[ppRR$DiasInventario>=5 & ppRR$DiasInventario<=9] <- 'BuenInventario'
    ppRR$Cat[ppRR$DiasInventario>=10 & ppRR$DiasInventario<=15] <- 'LeveSobreInventario'
    ppRR$Color <- 'rgb(254,99,99)'
    ppRR$Color[ppRR$DiasInventario>=2 & ppRR$DiasInventario<=4] <- 'rgb(254,200,99)'
    ppRR$Color[ppRR$DiasInventario>=5 & ppRR$DiasInventario<=9] <- 'rgb(50, 255, 150)'
    ppRR$Color[ppRR$DiasInventario>=10 & ppRR$DiasInventario<=15] <- 'rgb(254,200,99)'
    ppRR$n <- 1
    
    
    boxServicioTienda <- aggregate(OrdenDeCompra~Tienda, data=inventoryData, FUN=function(x){table(x)['NO']/length(x)*100}) # %>% filter(Tienda %in% as.character(input$TiendaGlobal), Producto %in% as.character(input$ProductoGlobal))
    boxServicioTienda$OrdenDeCompra[is.na(boxServicioTienda$OrdenDeCompra)] <- 0
    boxServicioTienda$Color <- '#32ff96'#'rgb(50, 255, 150)'
    boxServicioTienda$Color[boxServicioTienda$OrdenDeCompra<70] <- '#fe6363'#rgb(254,99,99)'
    
    plot <- highchart() %>%
      hc_chart(event= list(
        selection = JS(
          "function selectPointsByDrag(e) {
                   
                   // Select points
                   Highcharts.each(this.series, function (series) {
                   Highcharts.each(series.points, function (point) {
                   if (point.x >= e.xAxis[0].min && point.x <= e.xAxis[0].max &&
                   point.y >= e.yAxis[0].min && point.y <= e.yAxis[0].max) {
                   point.select(true, true);
                   }
                   });
                   });
                   
                   // Fire a custom event
                   Highcharts.fireEvent(this, 'selectedpoints', { points: this.getSelectedPoints() });
                   
                   return false; // Don't zoom
  }"
        )
      ),
      zoomType= "xy") %>%
      hc_add_series(boxServicioTienda, type="column",
                    hcaes(x = Tienda, y = OrdenDeCompra, color = Color), name = "Dias de Inventario")  %>%
      # hc_add_series(name = "Dias de Inventario", data = boxServicioTienda$OrdenDeCompra, color = boxServicioTienda$Color)  %>%
      hc_add_series(name = 'Inventario Objetivo', 75, type = "line") %>%
      hc_title(
        text = '',
        margin = 20,
        align = "left",
        style = list(useHTML = TRUE) #color = "#22A884", 
      ) %>%
      hc_xAxis(title = list(text = "Tienda"), categories = boxServicioTienda$Tienda  #,
               # opposite = TRUE,
               # plotLines = list(
               #   list(label = list(text = "This is a plotLine"),
               #        color = "#'FF0000",
               #        width = 2,
               #        value = 5.5))
      ) %>% 
      hc_yAxis(title = list(text = "%")#,
               # opposite = TRUE,
               # minorTickInterval = "auto",
               # minorGridLineDashStyle = "LongDashDotDot",
               # showFirstLabel = FALSE,
               # showLastLabel = FALSE,
               # plotBands = list(
               #   list(from = 25, to = 80, color = "rgba(100, 0, 0, 0.1)",
               #        label = list(text = "This is a plotBand")))
      ) %>%
      hc_colors(c("#999999", "#E69F00")) %>%
      # hc_legend(
      #   align = "left",
      #   verticalAlign = "top",
      #   layout = "vertical",
      #   x = 0,
      #   y = 100
      # ) %>% 
      hc_tooltip(
        crosshairs = TRUE,
        borderWidth = 5,
        sort = TRUE,
        table = TRUE
      )
    output$boxServicioTiendaPlot2 <- renderHighchart({
      plot
    })
    output$boxServicioTiendaDT <- renderDT({
      datatable(boxServicioTienda %>% select(-Color),
                rownames = F, escape = F,filter = 'top',
                options = list(
                  scrollX = TRUE, scrollY = '150px'
                )
      )
      
      
    })
    output$boxServicioTiendaDT2 <- renderDT({
      datatable(boxServicioTienda%>% select(-Color),
                rownames = F, escape = F,filter = 'top',
                options = list(
                  scrollX = TRUE
                )
      )
      
      
    })
    output$boxServicioTiendadtdescarga <- downloadHandler(
      filename = function(){"ServicioPorTienda.csv"},
      content = function(fname){
        write_csv(boxServicioTienda, fname)
      }
    )
    future({plot})
  })
  
  
  
  # UI - BOX 1 ------------------------------------------------------------------
  output$box1 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        title = 'Ventas',
        id = "box1",
        width = NULL,
        height = 400,
        tabPanel(
          title = "Grafica",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              dateRangeInput('VentaFilter', 'Fecha', min = today()-7, start = today()-7,
                             max = today(), end = today()),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          highchartOutput('box1Plot', height = 300) %>% withSpinner(type = 4,color = "#d33724",size = 0.7)
        ),
        tabPanel(
          title = "Tabla",
          div(
            style = "position:absolute;left:0.5em;bottom: 0.5em;",
            dropdown(
              downloadButton(outputId = "box1dtdescarga", label = "Descargar datos"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          DT::dataTableOutput('box1DT') %>% withSpinner(type = 4,color = "#d33724",size = 0.7)
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em;",
          conditionalPanel(
            "input.box1 == 'Grafica'",
            actionBttn(
              inputId = "box1graficaplus",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em;",
          conditionalPanel(
            "input.box1 == 'Tabla'",
            actionBttn(
              inputId = "box1dtplus",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        )
      )
    )
  })
  observeEvent((input$box1dtplus), {
    #box1DT <- alca
    showModal(modalDialog(
      DTOutput('box1dtplusoutput', height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  observeEvent((input$box1graficaplus), {
    print('modal')
    showModal(modalDialog(
      highchartOutput('box1graficaplusoutput'),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  output$box1Plot <- renderHighchart({
    tiendas <- unique(filter(provs, Proveedor %in% as.character(input$ProveedorGlobal))$Tienda)
    plotData2 <- filter(plotData2, Tienda %in% tiendas) #AQUI
    try(if(!is.null(input$ResumenFiltrosTable_rows_selected)){
      plotData2 <- plotData2 %>% filter(Sku %in% unique(inventoryDataUpdate()$Sku))
    })
    ventasMes <- aggregate(.~Mes, plotData2 %>% select(Mes, Venta, VentaAP), sum)
    print(1)
    ventasMes$Variacion <- paste0(round((ventasMes$Venta/ventasMes$VentaAP)*100,2),'%')
    ventasMes$Peso <- ventasMes$Venta/sum(ventasMes$Venta)
    ventasMes <- ventasMes %>% mutate(Peso=paste0(round(Peso*100,2),'%')) #arrange(ventasMes, desc(Peso))
    # plot <-  plot_ly(ventasMes, x = ~Mes, y = ~Venta,
    #                  hoverinfo = "text",hovertext = paste0(paste(ventasMes$Mes, year(today()), sep='-'), ' Q', prettyNum(round(ventasMes$Venta,2), big.mark = ',', big.interval = 3)),
    #                  name = paste('Ventas por Mes', year(today())), type = 'scatter', mode = 'lines+markers') %>%
    #   add_trace(y = ~VentaAP,
    #             hoverinfo = "text",hovertext =  paste0(paste(ventasMes$Mes, year(today())-1, sep='-'), ' Q', prettyNum(round(ventasMes$VentaAP,2), big.mark = ',', big.interval = 3)),
    #             name = paste('Ventas por Mes', year(today())-1), mode = 'lines+markers') %>%
    #   layout(hovermode = "x unified", xaxis=list(title='Mes'),yaxis=list(title='Q'),title=paste('Ventas por Mes', year(today()), 'vs', year(today())-1))
    plot <- hchart(
      ventasMes, "line", name = paste('Ventas',year(today())),
      hcaes(x = Mes, y = Venta),
      event= list(
        selection = JS(
          "function selectPointsByDrag(e) {
          
          // Select points
          Highcharts.each(this.series, function (series) {
          Highcharts.each(series.points, function (point) {
          if (point.x >= e.xAxis[0].min && point.x <= e.xAxis[0].max &&
          point.y >= e.yAxis[0].min && point.y <= e.yAxis[0].max) {
          point.select(true, true);
          }
          });
          });
          
          // Fire a custom event
          Highcharts.fireEvent(this, 'selectedpoints', { points: this.getSelectedPoints() });
          
          return false; // Don't zoom
  }"
        )
      ),
      zoomType= "xy") %>%
      hc_add_series(name = paste('Ventas',year(today())-1), ventasMes$VentaAP, type = "line")%>% 
      hc_title(
        text = paste('Ventas por Mes', year(today()), 'vs', year(today())-1),
        margin = 20,
        align = "left",
        style = list(useHTML = TRUE) #color = "#22A884", 
      ) %>%
      hc_xAxis(categories = ventasMes$Mes, title = list(text = "Mes")#,
               # opposite = TRUE,
               # plotLines = list(
               #   list(label = list(text = "This is a plotLine"),
               #        color = "#'FF0000",
               #        width = 2,
               #        value = 5.5))
      ) %>% 
      hc_yAxis(title = list(text = "Ventas (Q)")#,
               # opposite = TRUE,
               # minorTickInterval = "auto",
               # minorGridLineDashStyle = "LongDashDotDot",
               # showFirstLabel = FALSE,
               # showLastLabel = FALSE,
               # plotBands = list(
               #   list(from = 25, to = 80, color = "rgba(100, 0, 0, 0.1)",
               #        label = list(text = "This is a plotBand")))
      ) %>%
      hc_colors(c("#999999", "#E69F00")) %>%
      hc_legend(
        align = "left",
        verticalAlign = "top",
        layout = "vertical",
        x = 0,
        y = 100
      ) %>% 
      hc_tooltip(
        crosshairs = TRUE,
        borderWidth = 5,
        sort = TRUE,
        table = TRUE
      )
    ventasMes2 <- ventasMes
    names(ventasMes2)[2:3] <- c(paste0('Ventas',year(today())),paste0('Ventas',year(today())-1))
    for(j in 2:3){
      ventasMes2[,j] <- paste0('Q',prettyNum(round(ventasMes2[,j],2),big.mark = ',', big.interval = 3))
    }
    dtDF <- ventasMes2
    output$box1DT <- renderDT({
      datatable(dtDF,
                rownames = F, escape = F,filter = 'top',
                options = list(
                  scrollX = TRUE, scrollY = '150px'
                )
      )
    })
    output$box1graficaplusoutput <- renderHighchart({
      future({plot %>% 
          hc_plotOptions(series = list(events = list(click = JS("function(event) {Shiny.onInputChange('box1clickplus',event.point.name);}"))))})
    })
    output$box1dtplusoutput <- renderDT({
      datatable(dtDF,
                rownames = F, escape = F,filter = 'top',
                options = list(
                  scrollX = TRUE
                )
      )
    })
    future({plot %>% 
        hc_plotOptions(series = list(events = list(click = JS("function(event) {Shiny.onInputChange('box1click',event.point.name);}"))))})
  })
  
  # UI - BOX 2 ------------------------------------------------------------------
  output$box2 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        title = 'Ventas por Producto',
        id = "box2",
        width = NULL,
        height = 400,
        tabPanel(
          title = "Grafica",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              dateRangeInput('VentaFilter', 'Fecha', min = today()-7, start = today()-7,
                             max = today(), end = today()),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          highchartOutput('box2Plot', height = 300) %>% withSpinner(type = 4,color = "#d33724",size = 0.7)
        ),
        tabPanel(
          title = "Tabla",
          div(
            style = "position:absolute;left:0.5em;bottom: 0.5em;",
            dropdown(
              downloadButton(outputId = "box2dtdescarga", label = "Descargar datos"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          DT::dataTableOutput('box2DT') %>% withSpinner(type = 4,color = "#d33724",size = 0.7)
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em;",
          conditionalPanel(
            "input.box2 == 'Grafica'",
            actionBttn(
              inputId = "box2graficaplus",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em;",
          conditionalPanel(
            "input.box2 == 'Tabla'",
            actionBttn(
              inputId = "box2dtplus",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        )
      )
    )
  })
  observeEvent((input$box2graficaplus), {
    showModal(modalDialog(
      highchartOutput('box2Plot2', height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  observeEvent((input$box2dtplus), {
    showModal(modalDialog(
      DT::dataTableOutput('box2DT2', height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  output$box2Plot <- renderHighchart({
    tiendas <- unique(filter(provs, Proveedor %in% as.character(input$ProveedorGlobal))$Tienda)
    
    
    try(if(!is.null(input$ResumenFiltrosTable_rows_selected)){
      d <- input$ResumenFiltrosTable_rows_selected
    })
    if(!exists('d')){
      d <- 1:nrow(filtrosDT)
    }
    print(d)
    Sivec <- with(inventoryData %>% filter(Clasificacion %in% filtrosDT$Clasificacion[d]), paste(Sku, Tienda))
    plotData2 <- filter(plotData2 %>% mutate(Si=paste(Sku,Tienda)), Tienda %in% tiendas) %>% filter(Si %in% Sivec) %>% select(-Si)
    
    box2 <- aggregate(.~Producto, plotData2 %>% select(Producto, Venta, VentaAP), sum)
    box2$Variacion <- paste0(round((box2$Venta/box2$VentaAP)*100,2),'%')
    box2$Peso <- box2$Venta/sum(box2$Venta)
    box2 <- box2 %>% mutate(Peso=paste0(round(Peso*100,2),'%')) #arrange(box2, desc(Peso))
    box22 <- box2
    names(box22)[2:3] <- c(paste0('Ventas',year(today())),paste0('Ventas',year(today())-1))
    for(j in 2:3){
      box22[,j] <- paste0('Q',prettyNum(round(box22[,j],2),big.mark = ',', big.interval = 3))
    }
    # plot <- plot_ly(box2, x = ~Producto, y = ~Venta,
    #                 hoverinfo = "text",hovertext = paste0(paste(box2$Producto, year(today()), sep='-'), ' Q', prettyNum(round(box2$Venta,2), big.mark = ',', big.interval = 3)),
    #                 name = paste('Ventas por Producto', year(today())), type = 'scatter', mode = 'lines+markers') %>%
    #   add_trace(y = ~VentaAP,
    #             hoverinfo = "text",hovertext =  paste0(paste(box2$Producto, year(today())-1, sep='-'), 'Q', prettyNum(round(box2$VentaAP,2), big.mark = ',', big.interval = 3)),
    #             name = paste('Ventas por Producto', year(today())-1), mode = 'lines+markers') %>%
    #   layout(hovermode = "x unified", xaxis=list(title='Producto'),yaxis=list(title='Q'),title=paste('Ventas por Producto', year(today()), 'vs', year(today())-1))
    
    plot <- hchart(
      box2, "line", name = paste('Ventas',year(today())),
      hcaes(x = Producto, y = Venta)) %>%
      hc_add_series(name = paste('Ventas',year(today())-1), box2$VentaAP, type = "line")%>% 
      hc_title(
        text = paste('Ventas por Producto', year(today()), 'vs', year(today())-1),
        margin = 20,
        align = "left",
        style = list(useHTML = TRUE) #color = "#22A884", 
      ) %>%
      hc_xAxis(title = list(text = "Producto")#,
               # opposite = TRUE,
               # plotLines = list(
               #   list(label = list(text = "This is a plotLine"),
               #        color = "#'FF0000",
               #        width = 2,
               #        value = 5.5))
      ) %>% 
      hc_yAxis(title = list(text = "Ventas (Q)")#,
               # opposite = TRUE,
               # minorTickInterval = "auto",
               # minorGridLineDashStyle = "LongDashDotDot",
               # showFirstLabel = FALSE,
               # showLastLabel = FALSE,
               # plotBands = list(
               #   list(from = 25, to = 80, color = "rgba(100, 0, 0, 0.1)",
               #        label = list(text = "This is a plotBand")))
      ) %>%
      hc_colors(c("#999999", "#E69F00")) %>%
      hc_legend(
        align = "left",
        verticalAlign = "top",
        layout = "vertical",
        x = 0,
        y = 100
      ) %>% 
      hc_tooltip(
        crosshairs = TRUE,
        borderWidth = 5,
        sort = TRUE,
        table = TRUE
      )
    
    output$box2DT <- renderDT({
      output$box2DT2 <- renderDT({
        # names(box2)[2:3] <- c(paste0('Ventas',year(today())),paste0('Ventas',year(today())-1))
        # for(j in 2:3){
        #   box2[,j] <- paste0('Q',prettyNum(round(box2[,j],2),big.mark = ',', big.interval = 3))
        # }
        datatable(box2,
                  rownames = F, escape = F,filter = 'top',
                  options = list(
                    scrollX = TRUE, scrollY = '400px'
                  )
        )
      })
      datatable(box22,
                rownames = F, escape = F,filter = 'top',
                options = list(
                  scrollX = TRUE, scrollY = '90px'
                )
      )
    })
    output$box2Plot2 <- renderHighchart({
      future({plot})
    })
    future({plot})
  })
  
  # UI - BOX 3 ------------------------------------------------------------------
  output$box3 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        title = 'Ventas por Tienda',
        id = "box3",
        width = NULL,
        height = 400,
        tabPanel(
          title = "Grafica",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              dateRangeInput('VentaFilter', 'Fecha', min = today()-7, start = today()-7,
                             max = today(), end = today()),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          highchartOutput('box3Plot', height = 300) %>% withSpinner(type = 4,color = "#d33724",size = 0.7)
        ),
        tabPanel(
          title = "Tabla",
          div(
            style = "position:absolute;left:0.5em;bottom: 0.5em;",
            dropdown(
              downloadButton(outputId = "box3dtdescarga", label = "Descargar datos"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          DT::dataTableOutput('box3DT') %>% withSpinner(type = 4,color = "#d33724",size = 0.7)
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em;",
          conditionalPanel(
            "input.box3 == 'Grafica'",
            actionBttn(
              inputId = "box3graficaplus",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em;",
          conditionalPanel(
            "input.box3 == 'Tabla'",
            actionBttn(
              inputId = "box3dtplus",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        )
      )
    )
  })
  observeEvent((input$box3graficaplus), {
    showModal(modalDialog(
      highchartOutput('box3Plot2', height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  observeEvent((input$box3dtplus), {
    showModal(modalDialog(
      DT::dataTableOutput('box3DT2', height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  output$box3Plot <- renderHighchart({
    tiendas <- unique(filter(provs, Proveedor %in% as.character(input$ProveedorGlobal))$Tienda)
    try(if(!is.null(input$ResumenFiltrosTable_rows_selected)){
      d <- input$ResumenFiltrosTable_rows_selected
    })
    if(!exists('d')){
      d <- 1:nrow(filtrosDT)
    }
    print(d)
    Sivec <- with(inventoryData %>% filter(Clasificacion %in% filtrosDT$Clasificacion[d]), paste(Sku, Tienda))
    plotData2 <- filter(plotData2 %>% mutate(Si=paste(Sku,Tienda)), Tienda %in% tiendas) %>% filter(Si %in% Sivec) %>% select(-Si)
    
    
    box3 <- aggregate(.~Tienda, plotData2 %>% select(Tienda, Venta, VentaAP), sum)
    box3$Variacion <- box3$Venta/box3$VentaAP
    box3$Peso <- box3$Venta/sum(box3$Venta)
    box3 <- box3 #arrange(box3, desc(Peso))
    box32 <- box3 %>% mutate(Peso=paste0(round(Peso*100,2),'%'), Variacion=paste0(round(Variacion*100,2),'%'))
    names(box32)[2:3] <- c(paste0('Ventas',year(today())),paste0('Ventas',year(today())-1))
    for(j in 2:3){
      box32[,j] <- paste0('Q',prettyNum(round(box32[,j],2),big.mark = ',', big.interval = 3))
    }
    # plot <- plot_ly(box3, x = ~Tienda, y = ~Venta,
    #                 hoverinfo = "text",hovertext = paste0(paste(box3$Tienda, year(today()), sep='-'), ' Q', prettyNum(round(box3$Venta,2), big.mark = ',', big.interval = 3)),
    #                 name = paste('Ventas por Tienda', year(today())), type = 'scatter', mode = 'lines+markers') %>%
    #   add_trace(y = ~VentaAP,
    #             hoverinfo = "text",hovertext =  paste0(paste(box3$Tienda, year(today())-1, sep='-'), 'Q', prettyNum(round(box3$VentaAP,2), big.mark = ',', big.interval = 3)),
    #             name = paste('Ventas por Tienda', year(today())-1), mode = 'lines+markers') %>%
    #   layout(hovermode = "x unified", xaxis=list(title='Tienda'),yaxis=list(title='Q'),title=paste('Ventas por Tienda', year(today()), 'vs', year(today())-1))
    plot <- hchart(
      box3, "line", name = paste('Ventas',year(today())),
      hcaes(x = Tienda, y = Venta)) %>%
      hc_add_series(name = paste('Ventas',year(today())-1), box3$VentaAP, type = "line")%>% 
      hc_title(
        text = paste('Ventas por Tienda', year(today()), 'vs', year(today())-1),
        margin = 20,
        align = "left",
        style = list(useHTML = TRUE) #color = "#22A884", 
      ) %>%
      hc_xAxis(title = list(text = "Tienda")#,
               # opposite = TRUE,
               # plotLines = list(
               #   list(label = list(text = "This is a plotLine"),
               #        color = "#'FF0000",
               #        width = 2,
               #        value = 5.5))
      ) %>% 
      hc_yAxis(title = list(text = "Ventas (Q)")#,
               # opposite = TRUE,
               # minorTickInterval = "auto",
               # minorGridLineDashStyle = "LongDashDotDot",
               # showFirstLabel = FALSE,
               # showLastLabel = FALSE,
               # plotBands = list(
               #   list(from = 25, to = 80, color = "rgba(100, 0, 0, 0.1)",
               #        label = list(text = "This is a plotBand")))
      ) %>%
      hc_colors(c("#999999", "#E69F00")) %>%
      hc_legend(
        align = "left",
        verticalAlign = "top",
        layout = "vertical",
        x = 0,
        y = 100
      ) %>% 
      hc_tooltip(
        crosshairs = TRUE,
        borderWidth = 5,
        sort = TRUE,
        table = TRUE
      )
    print('box3 plot')
    output$box3DT <- renderDT({
      output$box3DT2 <- renderDT({
        print(box3)
        datatable(box3,
                  rownames = F, escape = F,filter = 'top',
                  options = list(
                    scrollX = TRUE, scrollY = '400px'
                  )
        )
      })
      print('box3 dt')
      datatable(box32,
                rownames = F, escape = F,filter = 'top',
                options = list(
                  scrollX = TRUE, scrollY = '90px'
                )
      )
    })
    output$box3Plot2 <- renderHighchart({
      future({plot})
    })
    future({plot})
  })
  
  # UI - BOX 4 ------------------------------------------------------------------
  output$box4 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        title = 'Ventas por Proveedor',
        id = "box4",
        width = NULL,
        height = 400,
        tabPanel(
          title = "Grafica",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              dateRangeInput('VentaFilter', 'Fecha', min = today()-7, start = today()-7,
                             max = today(), end = today()),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          highchartOutput('box4Plot', height = 300) %>% withSpinner(type = 4,color = "#d33724",size = 0.7)
        ),
        tabPanel(
          title = "Tabla",
          div(
            style = "position:absolute;left:0.5em;bottom: 0.5em;",
            dropdown(
              downloadButton(outputId = "box4dtdescarga", label = "Descargar datos"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          DT::dataTableOutput('box4DT') %>% withSpinner(type = 4,color = "#d33724",size = 0.7)
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em;",
          conditionalPanel(
            "input.box4 == 'Grafica'",
            actionBttn(
              inputId = "box4graficaplus",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em;",
          conditionalPanel(
            "input.box4 == 'Tabla'",
            actionBttn(
              inputId = "box4dtplus",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        )
      )
    )
  })
  observeEvent((input$box4graficaplus), {
    showModal(modalDialog(
      highchartOutput('box4Plot2', height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  observeEvent((input$box4dtplus), {
    showModal(modalDialog(
      DT::dataTableOutput('box4DT2', height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  output$box4Plot <- renderHighchart({
    tiendas <- unique(filter(provs, Proveedor %in% as.character(input$ProveedorGlobal))$Tienda)
    
    try(if(!is.null(input$ResumenFiltrosTable_rows_selected)){
      d <- input$ResumenFiltrosTable_rows_selected
    })
    if(!exists('d')){
      d <- 1:nrow(filtrosDT)
    }
    print(d)
    Sivec <- with(inventoryData %>% filter(Clasificacion %in% filtrosDT$Clasificacion[d]), paste(Sku, Tienda))
    plotData2 <- filter(plotData2 %>% mutate(Si=paste(Sku,Tienda)), Tienda %in% tiendas) %>% filter(Si %in% Sivec) %>% select(-Si)
    
    
    box4 <- aggregate(.~Proveedor, plotData2 %>% select(Proveedor, Venta, VentaAP), sum)
    box4$Variacion <- paste0(round((box4$Venta/box4$VentaAP)*100,2),'%')
    box4$Peso <- box4$Venta/sum(box4$Venta)
    box4 <- box4 %>% mutate(Peso=paste0(round(Peso*100,2),'%')) #arrange(box4, desc(Peso))
    box42 <- box4
    names(box42)[2:3] <- c(paste0('Ventas',year(today())),paste0('Ventas',year(today())-1))
    for(j in 2:3){
      box42[,j] <- paste0('Q',prettyNum(round(box42[,j],2),big.mark = ',', big.interval = 3))
    }
    # plot <- plot_ly(box4, x = ~Proveedor, y = ~Venta,
    #                 hoverinfo = "text",hovertext = paste0(paste(box4$Proveedor, year(today()), sep='-'), ' Q', prettyNum(round(box4$Venta,2), big.mark = ',', big.interval = 3)),
    #                 name = paste('Ventas por Proveedor', year(today())), type = 'scatter', mode = 'lines+markers') %>%
    #   add_trace(y = ~VentaAP,
    #             hoverinfo = "text",hovertext =  paste0(paste(box4$Proveedor, year(today())-1, sep='-'), 'Q', prettyNum(round(box4$VentaAP,2), big.mark = ',', big.interval = 3)),
    #             name = paste('Ventas por Proveedor', year(today())-1), mode = 'lines+markers') %>%
    #   layout(hovermode = "x unified", xaxis=list(title='Proveedor'),yaxis=list(title='Q'),title=paste('Ventas por Proveedor', year(today()), 'vs', year(today())-1))
    
    plot <- hchart(
      box4, "line", name = paste('Ventas',year(today())),
      hcaes(x = Proveedor, y = Venta)) %>%
      hc_add_series(name = paste('Ventas',year(today())-1), box4$VentaAP, type = "line")%>% 
      hc_title(
        text = paste('Ventas por Proveedor', year(today()), 'vs', year(today())-1),
        margin = 20,
        align = "left",
        style = list(useHTML = TRUE) #color = "#22A884", 
      ) %>%
      hc_xAxis(title = list(text = "Proveedor")#,
               # opposite = TRUE,
               # plotLines = list(
               #   list(label = list(text = "This is a plotLine"),
               #        color = "#'FF0000",
               #        width = 2,
               #        value = 5.5))
      ) %>% 
      hc_yAxis(title = list(text = "Ventas (Q)")#,
               # opposite = TRUE,
               # minorTickInterval = "auto",
               # minorGridLineDashStyle = "LongDashDotDot",
               # showFirstLabel = FALSE,
               # showLastLabel = FALSE,
               # plotBands = list(
               #   list(from = 25, to = 80, color = "rgba(100, 0, 0, 0.1)",
               #        label = list(text = "This is a plotBand")))
      ) %>%
      hc_colors(c("#999999", "#E69F00")) %>%
      hc_legend(
        align = "left",
        verticalAlign = "top",
        layout = "vertical",
        x = 0,
        y = 100
      ) %>% 
      hc_tooltip(
        crosshairs = TRUE,
        borderWidth = 5,
        sort = TRUE,
        table = TRUE
      )
    
    output$box4DT <- renderDT({
      output$box4DT2 <- renderDT({
        # names(box4)[2:3] <- c(paste0('Ventas',year(today())),paste0('Ventas',year(today())-1))
        # for(j in 2:3){
        #   box4[,j] <- paste0('Q',prettyNum(round(box4[,j],2),big.mark = ',', big.interval = 3))
        # }
        datatable(box4,
                  rownames = F, escape = F,filter = 'top',
                  options = list(
                    scrollX = TRUE, scrollY = '400px'
                  )
        )
      })
      datatable(box42,
                rownames = F, escape = F,filter = 'top',
                options = list(
                  scrollX = TRUE, scrollY = '90px'
                )
      )
    })
    output$box4Plot2 <- renderHighchart({
      future({plot})
    })
    future({plot})
  })
  
  # UI - BOX 5 ------------------------------------------------------------------
  output$box5 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        title = 'Nivel de Inventario vs Ideal',
        id = "box5",
        width = NULL,
        height = 450,
        tabPanel(
          title = "Grafica",
          tags$br(),
          div(
            highchartOutput('box5Plot', height = 300) %>% withSpinner(type = 4,color = "#d33724",size = 0.7)
          )
        ),
        tabPanel(
          title = "Tabla",
          div(
            style = "position:absolute;left:0.5em;bottom: 0.5em;",
            downloadBttn(outputId = "box5dtdescarga", label = "", color = 'primary', style = 'material-flat', size = 'xs')
            
          ),
          DT::dataTableOutput('box5DT') %>% withSpinner(type = 4,color = "#d33724",size = 0.7)
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em;",
          conditionalPanel(
            "input.box5 == 'Grafica'",
            actionBttn(
              inputId = "box5graficaplus",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em;",
          conditionalPanel(
            "input.box5 == 'Tabla'",
            actionBttn(
              inputId = "box5dtplus",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        )
      )
    )
  })
  observeEvent((input$box5dtplus), {
    #box5DT <- alca
    showModal(modalDialog(
      DTOutput('box5dtplusoutput', height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  observeEvent((input$box5graficaplus), {
    print('modal')
    showModal(modalDialog(
      highchartOutput('box5graficaplusoutput'),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  output$box5Plot <- renderHighchart({
    # inventoryData <- inventoryDataUpdate()
    # filter(, Tienda %in% input$TiendasO, Sku %in% prods, Producto %in% input$SkuInv, Categoria %in% input$InvCat)
    # try(trackFun(session$user,'Inventarios'))
    
    try(if(!is.null(input$ResumenFiltrosTable_rows_selected)){
      d <- input$ResumenFiltrosTable_rows_selected
    })
    if(!exists('d')){
      d <- 1:nrow(filtrosDT)
    }
    print(d)
    Sivec <- with(inventoryData %>% filter(Clasificacion %in% filtrosDT$Clasificacion[d]), paste(Sku, Tienda))
    plotData <- filter(inventoryData %>% mutate(Si=paste(Sku,Tienda))) %>% filter(Si %in% Sivec) %>% select(-Si)
    
    # plotData <- inventoryData# %>% filter(Producto %in% input$SkuInv, Tienda %in% input$TiendasO)
    
    # if(input$ColYbox5check){
    #   kk <- c('Tienda',input$ColYbox5)
    #   ColYbox5 <- ifelse(input$ColYbox5=='Tienda','Tienda',paste(input$ColYbox5,'Tienda',sep='+'))
    #   plotForm <- as.formula(paste('.',ColYbox5,sep='~'))
    # }
    # if(!input$ColYbox5check){
    kk <- c(input$ColYbox5)
    ColYbox5 <- input$ColYbox5
    plotForm <- as.formula(paste('.',ColYbox5,sep='~'))
    # }
    kk <- kk[!duplicated(kk)]
    plotData <- plotData[order(plotData[,ColYbox5]),]
    tiendas <- unique(filter(provs, Proveedor %in% as.character(input$ProveedorGlobal))$Tienda)
    
    
    plotData2 <- aggregate(plotForm, data=select(plotData, c('Costo','Unidades','Inventario','Prediccion.1.Semana','InventarioTotalDias','InventarioDeseado',kk)), FUN=sum)
    plotData <- aggregate(plotForm, data=select(plotData, c('Costo','Unidades','Inventario','Prediccion.1.Semana','Prediccion.2.Semanas','Prediccion.3.Semanas','Transito',kk)), FUN=sum)
    plotData2 <- dplyr_solution(dat = plotData2, vars = kk)
    # plot <- plot_ly(plotData2 %>% arrange(xaxis), x = ~xaxis, y = ~InventarioTotalDias, type = 'bar', name = 'Dias de Inventario') %>%
    #   add_trace(y = ~InventarioDeseado, name = 'Inventario Objetivo', type = 'scatter', mode = 'lines') %>%
    #   layout(yaxis = list(title = 'Dias de Inventario vs Inventario Deseado'),xaxis = list(title = ColYbox5), barmode = 'stack',
    #          hovermode = 'compare') %>% config(displayModeBar = F)
    plot <- highchart() %>%
      hc_chart(type="column",
               event= list(
                 selection = JS(
                   "function selectPointsByDrag(e) {
                   
                   // Select points
                   Highcharts.each(this.series, function (series) {
                   Highcharts.each(series.points, function (point) {
                   if (point.x >= e.xAxis[0].min && point.x <= e.xAxis[0].max &&
                   point.y >= e.yAxis[0].min && point.y <= e.yAxis[0].max) {
                   point.select(true, true);
                   }
                   });
                   });
                   
                   // Fire a custom event
                   Highcharts.fireEvent(this, 'selectedpoints', { points: this.getSelectedPoints() });
                   
                   return false; // Don't zoom
  }"
                 )
               ),
               zoomType= "xy") %>%
      hc_add_series(name = "Dias de Inventario", data = plotData2$InventarioTotalDias)  %>%
      hc_add_series(name = 'Inventario Objetivo', plotData2$InventarioDeseado, type = "line") %>% 
      hc_title(
        text = paste('Ventas por',ColYbox5),
        margin = 20,
        align = "left",
        style = list(useHTML = TRUE) #color = "#22A884", 
      ) %>%
      hc_xAxis(categories = plotData2$xaxis, title = list(text = ColYbox5)#,
               # opposite = TRUE,
               # plotLines = list(
               #   list(label = list(text = "This is a plotLine"),
               #        color = "#'FF0000",
               #        width = 2,
               #        value = 5.5))
      ) %>% 
      hc_yAxis(title = list(text = "Ventas (Q)")#,
               # opposite = TRUE,
               # minorTickInterval = "auto",
               # minorGridLineDashStyle = "LongDashDotDot",
               # showFirstLabel = FALSE,
               # showLastLabel = FALSE,
               # plotBands = list(
               #   list(from = 25, to = 80, color = "rgba(100, 0, 0, 0.1)",
               #        label = list(text = "This is a plotBand")))
      ) %>%
      hc_colors(c("#999999", "#E69F00")) %>%
      hc_legend(
        align = "left",
        verticalAlign = "top",
        layout = "vertical",
        x = 0,
        y = 100
      ) %>% 
      hc_tooltip(
        crosshairs = TRUE,
        borderWidth = 5,
        sort = TRUE,
        table = TRUE
      )
    dtDF <- plotData2
    output$box5DT <- renderDT({
      datatable(dtDF,
                rownames = F, escape = F,filter = 'top',
                options = list(
                  scrollX = TRUE, scrollY = '200px'
                )
      )
      
    })
    output$box5dtdescarga <- downloadHandler(
      filename = function(){"ServicioPorTienda.csv"},
      content = function(fname){
        write_csv(boxServicioTienda, fname)
      }
    )
    output$box5graficaplusoutput <- renderHighchart({
      future({plot})
    })
    output$box5dtplusoutput <- renderDT({
      datatable(dtDF,
                rownames = F, escape = F,filter = 'top',
                options = list(
                  scrollX = TRUE
                )
      )
    })
    future({plot})
  })
  
  # UI - BOX 6 ------------------------------------------------------------------
  output$box6 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        title = 'Clasificacion Inventario',
        id = "box6",
        width = NULL,
        height = 400,
        tabPanel(
          title = "Grafica",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              checkboxInput('diasInvReset', 'Resetear Datos', value = F),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          plotlyOutput('box6Plot', height = 300) %>% withSpinner(type = 4,color = "#d33724",size = 0.7)
        ),
        tabPanel(
          title = "Tabla",
          div(
            style = "position:absolute;left:0.5em;bottom: 0.5em;",
            dropdown(
              downloadButton(outputId = "box6dtdescarga", label = "Descargar datos"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          DT::dataTableOutput('box6DT') %>% withSpinner(type = 4,color = "#d33724",size = 0.7)
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em;",
          conditionalPanel(
            "input.box6 == 'Grafica'",
            actionBttn(
              inputId = "box6graficaplus",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em;",
          conditionalPanel(
            "input.box6 == 'Tabla'",
            actionBttn(
              inputId = "box6dtplus",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        )
      )
    )
  })
  observeEvent((input$box6dtplus), {
    #box6DT <- alca
    showModal(modalDialog(
      DTOutput('box6dtplusoutput', height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  observeEvent((input$box6graficaplus), {
    print('modal')
    showModal(modalDialog(
      plotlyOutput('box6graficaplusoutput'),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  output$box6Plot <- renderPlotly({
    print('Clasificacion Inventario 0')
    # try(trackFun(session$user,'resumen'))
    provs <- filter(provs, Proveedor %in% as.character(input$ProveedorGlobal))
    # inventoryData <- inventoryDataUpdate()
    try(if(!is.null(input$ResumenFiltrosTable_rows_selected)){
      d <- input$ResumenFiltrosTable_rows_selected
    })
    if(!exists('d')){
      d <- 1:nrow(filtrosDT)
    }
    print(d)
    Sivec <- with(inventoryData %>% filter(Clasificacion %in% filtrosDT$Clasificacion[d]), paste(Sku, Tienda))
    inventoryData <- filter(inventoryDataUpdate() %>% mutate(Si=paste(Sku,Tienda))) %>% filter(Si %in% Sivec) %>% select(-Si)
    
    cats <- select(inventoryData, Producto, Categoria, Subcategoria)
    cats <- cats[!duplicated(cats$Producto),]
    cats$Producto <- toupper(gsub("[^[:alnum:]]","",cats$Producto))
    print('Clasificacion Inventario 1')
    try(d <- event_data(event = "plotly_click", source = "subset"))
    if(!exists('d')){
      d <- NULL
    }
    print('Clasificacion Inventario 2')
    print(d)
    if(input$diasInvReset){ 
      d <- NULL
    }
    if (!is.null(d)){
      dkey <- as.character(unlist(d$key))
      inventoryData$Cat <- 'SobreInventario'
      inventoryData$Cat[inventoryData$InventarioTotalDias<=1] <- 'SinInventario'
      inventoryData$Cat[inventoryData$InventarioTotalDias>=2 & inventoryData$InventarioTotalDias<=4] <- 'SubInventario'
      inventoryData$Cat[inventoryData$InventarioTotalDias>=5 & inventoryData$InventarioTotalDias<=9] <- 'BuenInventario'
      inventoryData$Cat[inventoryData$InventarioTotalDias>=10 & inventoryData$InventarioTotalDias<=15] <- 'LeveSobreInventario'
      inventoryData <- filter(inventoryData, Cat %in% dkey)
      print(dim(inventoryData))
      ppRR$Cat <- 'SobreInventario'
      ppRR$Cat[ppRR$DiasInventario<=1] <- 'SinInventario'
      ppRR$Cat[ppRR$DiasInventario>=2 & ppRR$DiasInventario<=4] <- 'SubInventario'
      ppRR$Cat[ppRR$DiasInventario>=5 & ppRR$DiasInventario<=9] <- 'BuenInventario'
      ppRR$Cat[ppRR$DiasInventario>=10 & ppRR$DiasInventario<=15] <- 'LeveSobreInventario'
      ppRR <- filter(ppRR, Cat %in% dkey[1]) %>% select(-Cat)
      # print(dim(ppRR))
    }
    print('Clasificacion Inventario 3')
    inventoryData <- filter(inventoryData, Tienda %in% as.character(input$TiendaGlobal), Producto %in% as.character(input$ProductoGlobal))
    ppRR <- filter(ppRR, Tienda %in% as.character(input$TiendaGlobal), Producto %in% as.character(input$ProductoGlobal))
    ppRR$Producto <- toupper(gsub("[^[:alnum:]]","",ppRR$Producto))
    ppRR <- merge(ppRR,cats, all.x=T) # %>% filter(Tienda %in% as.character(input$TiendaGlobal))
    ppRR$Cat <- 'SobreInventario'
    ppRR$Cat[ppRR$DiasInventario<=1] <- 'SinInventario'
    ppRR$Cat[ppRR$DiasInventario>=2 & ppRR$DiasInventario<=4] <- 'SubInventario'
    ppRR$Cat[ppRR$DiasInventario>=5 & ppRR$DiasInventario<=9] <- 'BuenInventario'
    ppRR$Cat[ppRR$DiasInventario>=10 & ppRR$DiasInventario<=15] <- 'LeveSobreInventario'
    ppRR$Color <- 'rgb(254,99,99)'
    ppRR$Color[ppRR$DiasInventario>=2 & ppRR$DiasInventario<=4] <- 'rgb(254,200,99)'
    ppRR$Color[ppRR$DiasInventario>=5 & ppRR$DiasInventario<=9] <- 'rgb(50, 255, 150)'
    ppRR$Color[ppRR$DiasInventario>=10 & ppRR$DiasInventario<=15] <- 'rgb(254,200,99)'
    ppRR$n <- 1
    print('Clasificacion Inventario 4')
    # try(trackFun(session$user,'resumen'))
    mm <- data.frame(table(ppRR$DiasInventario))
    mm$Var1 <- as.numeric(as.character(mm$Var1))
    mm$Color <- 'rgb(254,99,99)'
    mm$Color[mm$Var1>=2 & mm$Var1<=4] <- 'rgb(254,200,99)'
    mm$Color[mm$Var1>=5 & mm$Var1<=9] <- 'rgb(50, 255, 150)'
    mm$Color[mm$Var1>=10 & mm$Var1<=15] <- 'rgb(254,200,99)'
    print('Clasificacion Inventario 3')
    plot <-  plot_ly(ppRR %>% arrange(Color), labels = ~Cat, type = 'pie', key=~Cat, source = "subset", # %>% filter(Tienda %in% as.character(input$TiendaGlobal), Producto %in% as.character(input$ProductoGlobal))
                     textposition = 'inside',
                     textinfo = 'label+percent',
                     # insidetextfont = list(color = '#FFFFFF'),
                     hoverinfo = 'text',
                     text = ~Cat,
                     marker = list(colors = ~Color),
                     #The 'pull' attribute can also be used to create space between the sectors
                     showlegend = FALSE) %>%
      layout(font=list(
        family = "sans serif",
        size = 18),
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% config(displayModeBar = F)
    dtDF <- mm %>% select(-Color)
    output$box6DT <- renderDT({
      datatable(dtDF,
                rownames = F, escape = F,filter = 'top',
                options = list(
                  scrollX = TRUE, scrollY = '150px'
                )
      )
    })
    output$box6graficaplusoutput <- renderPlotly({
      future({plot})
    })
    output$box6dtplusoutput <- renderDT({
      datatable(dtDF,
                rownames = F, escape = F,filter = 'top',
                options = list(
                  scrollX = TRUE
                )
      )
    })
    future({plot})
  })
  
  
  # UI - BOX 7 ------------------------------------------------------------------
  output$box7 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        title = 'Dias de Inventario',
        id = "box7",
        width = NULL,
        height = 400,
        tabPanel(
          title = "Grafica",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              dateRangeInput('DiasInvFilter', 'Fecha', min = today()-7, start = today()-7,
                             max = today(), end = today()),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          plotlyOutput('box7Plot', height = 300) %>% withSpinner(type = 4,color = "#d33724",size = 0.7)
        ),
        tabPanel(
          title = "Tabla",
          div(
            style = "position:absolute;left:0.5em;bottom: 0.5em;",
            dropdown(
              downloadButton(outputId = "box7dtdescarga", label = "Descargar datos"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          DT::dataTableOutput('box7DT') %>% withSpinner(type = 4,color = "#d33724",size = 0.7)
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em;",
          conditionalPanel(
            "input.box7 == 'Grafica'",
            actionBttn(
              inputId = "box7graficaplus",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em;",
          conditionalPanel(
            "input.box7 == 'Tabla'",
            actionBttn(
              inputId = "box7dtplus",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        )
      )
    )
  })
  observeEvent((input$box7dtplus), {
    #box7DT <- alca
    showModal(modalDialog(
      DTOutput('box7dtplusoutput', height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  observeEvent((input$box7graficaplus), {
    print('modal')
    showModal(modalDialog(
      plotlyOutput('box7graficaplusoutput'),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  output$box7Plot <- renderPlotly({
    # try(trackFun(session$user,'resumen'))
    
    try(if(!is.null(input$ResumenFiltrosTable_rows_selected)){
      d <- input$ResumenFiltrosTable_rows_selected
    })
    if(!exists('d')){
      d <- 1:nrow(filtrosDT)
    }
    print(d)
    Sivec <- with(inventoryData %>% filter(Clasificacion %in% filtrosDT$Clasificacion[d]), paste(Producto, Tienda))
    ppRR <- filter(ppRR %>% mutate(Si=paste(Producto,Tienda))) %>% filter(Si %in% Sivec) %>% select(-Si)
    
    mm <- data.frame(table(ppRR$DiasInventario))
    mm$Var1 <- as.numeric(as.character(mm$Var1))
    mm$Color <- 'rgb(254,99,99)'
    mm$Color[mm$Var1>=2 & mm$Var1<=4] <- 'rgb(254,200,99)'
    mm$Color[mm$Var1>=5 & mm$Var1<=9] <- 'rgb(50, 255, 150)'
    mm$Color[mm$Var1>=10 & mm$Var1<=15] <- 'rgb(254,200,99)'
    plot <- plot_ly(data=mm,x = ~Var1, y=~Freq, type = 'bar',
                    name = 'Dias de Inventario', marker = list(color =  ~Color)) %>%
      layout(title = '',yaxis = list(title = 'Frecuencia'),
             xaxis = list(title = 'Dias de Inventario')) %>% config(displayModeBar = F)
    dtDF <- mm %>% select(DiasInventario=1,Cantidad=2)
    output$box7DT <- renderDT({
      datatable(dtDF,
                rownames = F, escape = F,filter = 'top',
                options = list(
                  scrollX = TRUE, scrollY = '150px'
                )
      )
    })
    output$box7graficaplusoutput <- renderPlotly({
      future({plot})
    })
    output$box7dtplusoutput <- renderDT({
      datatable(dtDF,
                rownames = F, escape = F,filter = 'top',
                options = list(
                  scrollX = TRUE
                )
      )
    })
    future({plot})
  })
  
  # UI - BOX 8 ------------------------------------------------------------------
  output$box8 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        title = 'Dias de Inventario',
        id = "box8",
        width = NULL,
        height = 400,
        tabPanel(
          title = "Pivot",
          # div(
          #   style = "position: absolute; left: 0.5em; bottom: 0.5em;",
          #   dropdown(
          #     dateRangeInput('DiasInvFilter', 'Fecha', min = today()-7, start = today()-7,
          #                    max = today(), end = today()),
          #     size = "xs",
          #     icon = icon("gear", class = "opt"), 
          #     up = TRUE
          #   )
          # ),
          rpivotTableOutput('box8Plot', height = '300px') %>% withSpinner(type = 4,color = "#d33724",size = 0.7)
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em;",
          conditionalPanel(
            "input.box8 == 'Pivot'",
            actionBttn(
              inputId = "box8graficaplus",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        )
      )
    )
  })
  output$box8Plot <- renderRpivotTable({
    print('pivot')
    rpivotTable(inventoryDataUpdate() %>% select(Tienda, Sku, Proveedor, Producto, Clasificacion, Prediccion.Esta.Semana=Unidades, Categoria, Subcategoria, Prediccion.1.Semana, RotacionPromedio=Prom, Inventario, Minimo, Lead.Time, InventarioDeseado, Transito, InventarioTotalDias, NivelDeInventario, CompraUrgente, Costo), height = '300px')
  })
  observeEvent((input$box8graficaplus), {
    print('modal')
    showModal(modalDialog(
      renderRpivotTable({
        rpivotTable(inventoryDataUpdate() %>% select(Tienda, Sku, Proveedor, Producto, Clasificacion, Prediccion.Esta.Semana=Unidades, Categoria, Subcategoria, Prediccion.1.Semana, RotacionPromedio=Prom, Inventario, Minimo, Lead.Time, InventarioDeseado, Transito, InventarioTotalDias, NivelDeInventario, CompraUrgente, Costo), height = '650px')
      }),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  
  # UI - BOX 9 ------------------------------------------------------------------
  output$box9 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        title = '',
        id = "box9",
        width = NULL,
        height = 450,
        
        tabPanel(
          title = "Grafica",
          div(
            withSpinner(
              highchartOutput('box9Plot', height = 300) %>% withSpinner(type = 4,color = "#d33724",size = 0.7),
              type = 4,
              color = "#d33724",
              size = 0.7
            ),
            
          )
        ),
        tabPanel(
          title = "Tabla",
          div(
            withSpinner(
              DT::dataTableOutput('box9DT') %>% withSpinner(type = 4,color = "#d33724",size = 0.7),
              type = 4,
              color = "#d33724",
              size = 0.7
            ),
            downloadBttn(outputId = "box9dtdescarga", label = "", color = 'primary', style = 'material-flat', size = 'xs')
          )
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em;",
          conditionalPanel(
            "input.box9 == 'Grafica'",
            actionBttn(
              inputId = "box9graficaplus",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em;",
          conditionalPanel(
            "input.box9 == 'Tabla'",
            actionBttn(
              inputId = "box9dtplus",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        )
        
      )
    )
  })
  
  
  
  observeEvent((input$box9graficaplus), {
    showModal(modalDialog(
      highchartOutput('box9Plot2', height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  observeEvent((input$box9dtplus), {
    showModal(modalDialog(
      DT::dataTableOutput('box9DT2', height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  output$box9Plot <- renderHighchart({
    OpCol <- 'OportunidadIngresos'
    OpInfo <- gsub('Oportunidad','',OpCol)
    
    inventoryData <- inventoryData %>% filter(Proveedor %in% as.character(input$ProveedorGlobal), Tienda %in% as.character(input$TiendaGlobal), Categoria %in% as.character(input$CategoriaGlobal), Producto %in% as.character(input$ProductoGlobal)) %>% dplyr::rename(Prediccion=Unidades) %>% mutate(OportunidadUnidades=Prediccion-Inventario)
    inventoryData$OportunidadUnidades[inventoryData$OportunidadUnidades<0] <- 0
    inventoryData$OportunidadIngresos <- inventoryData$OportunidadUnidades*inventoryData$Costo*1.4
    inventoryData$Ingresos <- inventoryData$Prediccion*inventoryData$Costo*1.4
    
    plotData <- rbind(
      mutate(inventoryData, ProyeccionVenta=Ingresos, Info='Proyeccion Venta con Inventario Optimo'),
      mutate(inventoryData, ProyeccionVenta=Ingresos-OportunidadIngresos, Info='Proyeccion Venta Actual'),
      mutate(inventoryData, ProyeccionVenta=OportunidadIngresos, Info='Proyeccion Venta Perdida')
    )
    if(OpInfo=='Unidades'){
      plotData <- rbind(
        mutate(inventoryData, ProyeccionVenta=Prediccion, Info='Proyeccion Venta con Inventario Optimo'),
        mutate(inventoryData, ProyeccionVenta=Prediccion-OportunidadUnidades, Info='Proyeccion Venta Actual'),
        mutate(inventoryData, ProyeccionVenta=OportunidadUnidades, Info='Proyeccion Venta Perdida')
      )
    }
    
    plotData <- aggregate(ProyeccionVenta~., select(plotData, ProyeccionVenta, Info, weekyr), sum)
    
    # plot <- plotData %>%
    #   plot_ly(
    #     x = ~weekyr, 
    #     y = ~ProyeccionVenta,
    #     split = ~Info,
    #     # frame = ~frame, 
    #     type = 'scatter',
    #     mode = 'lines+markers',
    #     line = list(simplyfy = F)
    #   ) %>% layout(title='Proyeccion de Venta por Escenario',
    #                xaxis = list(
    #                  title = "Semana",
    #                  zeroline = F
    #                ),
    #                yaxis = list(
    #                  title = OpInfo,
    #                  zeroline = F
    #                )
    #   ) %>% config(displayModeBar = F)
    
    plot <- highchart() %>%
      hc_chart(event= list(
        selection = JS(
          "function selectPointsByDrag(e) {
          
          // Select points
          Highcharts.each(this.series, function (series) {
          Highcharts.each(series.points, function (point) {
          if (point.x >= e.xAxis[0].min && point.x <= e.xAxis[0].max &&
          point.y >= e.yAxis[0].min && point.y <= e.yAxis[0].max) {
          point.select(true, true);
          }
          });
          });
          
          // Fire a custom event
          Highcharts.fireEvent(this, 'selectedpoints', { points: this.getSelectedPoints() });
          
          return false; // Don't zoom
  }"
        )
      ),
      zoomType= "xy") %>%
      hc_xAxis(title = list(text = "Semana"),categories =unique(plotData$weekyr)) %>%
      hc_add_series(name = "Proyeccion de Venta con Inventario Actual", data = plotData$ProyeccionVenta[plotData$Info=='Proyeccion Venta Actual'], type = 'line') %>%
      hc_add_series(name = "Proyeccion de Venta con Inventario Optimo", data = plotData$ProyeccionVenta[plotData$Info=='Proyeccion Venta con Inventario Optimo'], type = 'line') %>%
      # hc_add_series(name = "Proyeccion de Venta con Inventario Optimo", data = plotData$ProyeccionVenta[plotData$Info=='Proyeccion Venta con Inventario Optimo'], type = 'line') %>%
      hc_title(
        text = '',
        margin = 20,
        align = "left",
        style = list(useHTML = TRUE) #color = "#22A884", 
      ) %>% 
      hc_yAxis(title = list(text = "Ventas (Q)")#,
               # opposite = TRUE,
               # minorTickInterval = "auto",
               # minorGridLineDashStyle = "LongDashDotDot",
               # showFirstLabel = FALSE,
               # showLastLabel = FALSE,
               # plotBands = list(
               #   list(from = 25, to = 80, color = "rgba(100, 0, 0, 0.1)",
               #        label = list(text = "This is a plotBand")))
      ) %>%
      hc_colors(c("#999999", "#E69F00")) %>%
      # hc_legend(
      #   align = "left",
      #   verticalAlign = "top",
      #   layout = "vertical",
      #   x = 0,
      #   y = 100
      # ) %>% 
      hc_tooltip(
        crosshairs = TRUE,
        borderWidth = 5,
        sort = TRUE,
        table = TRUE
      )
    
    output$box9DT <- renderDataTable({
      datatable(plotData,
                rownames = F, escape = F,filter = 'top',
                options = list(
                  scrollX = TRUE, pageLength =5 #scrollY = '400px'
                ))
    })
    output$box9DT2 <- renderDataTable({
      datatable(plotData,
                rownames = F, escape = F,filter = 'top',
                options = list(
                  scrollX = TRUE
                ))
    })
    output$box9Plot2 <- renderHighchart({
      future(plot)
    })
    future(plot)
  })
  
  # UI - BOX 10 ------------------------------------------------------------------
  output$box10 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        title = 'Nivel de Servicio por Tienda',
        id = "box10",
        width = NULL,
        height = 400,
        tabPanel(
          title = "Grafica",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              dateRangeInput('VentaFilter', 'Fecha', min = today()-7, start = today()-7,
                             max = today(), end = today()),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          highchartOutput('box10Plot', height = 300) %>% withSpinner(type = 4,color = "#d33724",size = 0.7)
        ),
        tabPanel(
          title = "Tabla",
          div(
            style = "position:absolute;left:0.5em;bottom: 0.5em;",
            dropdown(
              downloadButton(outputId = "box10dtdescarga", label = "Descargar datos"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          DT::dataTableOutput('box10DT') %>% withSpinner(type = 4,color = "#d33724",size = 0.7)
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em;",
          conditionalPanel(
            "input.box10 == 'Grafica'",
            actionBttn(
              inputId = "box10graficaplus",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em;",
          conditionalPanel(
            "input.box10 == 'Tabla'",
            actionBttn(
              inputId = "box10dtplus",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        )
      )
    )
  })
  observeEvent((input$box10graficaplus), {
    showModal(modalDialog(
      highchartOutput('box10Plot2', height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  observeEvent((input$box10dtplus), {
    showModal(modalDialog(
      DT::dataTableOutput('box10DT2', height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  output$box10Plot <- renderHighchart({
    provs <- filter(provs, Proveedor %in% as.character(input$ProveedorGlobal))
    inventoryData <- inventoryDataUpdate()
    inventoryData <- merge(inventoryData, provs)
    ppRR <- merge(ppRR, provs)
    cats <- select(inventoryData, Producto, Categoria, Subcategoria)
    cats <- cats[!duplicated(cats$Producto),]
    cats$Producto <- toupper(gsub("[^[:alnum:]]","",cats$Producto))
    # d <- event_data(event = "plotly_click", source = "subset")
    # if(input$diasInvReset){ 
    #   d <- NULL
    # }
    # if (!is.null(d)){
    #   dkey <- as.character(unlist(d$key))
    #   inventoryData$Cat <- 'SobreInventario'
    #   inventoryData$Cat[inventoryData$InventarioTotalDias<=1] <- 'SinInventario'
    #   inventoryData$Cat[inventoryData$InventarioTotalDias>=2 & inventoryData$InventarioTotalDias<=4] <- 'SubInventario'
    #   inventoryData$Cat[inventoryData$InventarioTotalDias>=5 & inventoryData$InventarioTotalDias<=9] <- 'BuenInventario'
    #   inventoryData$Cat[inventoryData$InventarioTotalDias>=10 & inventoryData$InventarioTotalDias<=15] <- 'LeveSobreInventario'
    #   inventoryData <- filter(inventoryData, Cat %in% dkey)
    #   print(dim(inventoryData))
    #   ppRR$Cat <- 'SobreInventario'
    #   ppRR$Cat[ppRR$DiasInventario<=1] <- 'SinInventario'
    #   ppRR$Cat[ppRR$DiasInventario>=2 & ppRR$DiasInventario<=4] <- 'SubInventario'
    #   ppRR$Cat[ppRR$DiasInventario>=5 & ppRR$DiasInventario<=9] <- 'BuenInventario'
    #   ppRR$Cat[ppRR$DiasInventario>=10 & ppRR$DiasInventario<=15] <- 'LeveSobreInventario'
    #   ppRR <- filter(ppRR, Cat %in% dkey[1]) %>% select(-Cat)
    #   # print(dim(ppRR))
    # }
    inventoryData <- filter(inventoryData, Tienda %in% as.character(input$TiendaGlobal), Producto %in% as.character(input$ProductoGlobal))
    ppRR <- filter(ppRR, Tienda %in% as.character(input$TiendaGlobal), Producto %in% as.character(input$ProductoGlobal))
    ppRR$Producto <- toupper(gsub("[^[:alnum:]]","",ppRR$Producto))
    ppRR <- merge(ppRR,cats, all.x=T) # %>% filter(Tienda %in% as.character(input$TiendaGlobal))
    ppRR$Cat <- 'SobreInventario'
    ppRR$Cat[ppRR$DiasInventario<=1] <- 'SinInventario'
    ppRR$Cat[ppRR$DiasInventario>=2 & ppRR$DiasInventario<=4] <- 'SubInventario'
    ppRR$Cat[ppRR$DiasInventario>=5 & ppRR$DiasInventario<=9] <- 'BuenInventario'
    ppRR$Cat[ppRR$DiasInventario>=10 & ppRR$DiasInventario<=15] <- 'LeveSobreInventario'
    ppRR$Color <- 'rgb(254,99,99)'
    ppRR$Color[ppRR$DiasInventario>=2 & ppRR$DiasInventario<=4] <- 'rgb(254,200,99)'
    ppRR$Color[ppRR$DiasInventario>=5 & ppRR$DiasInventario<=9] <- 'rgb(50, 255, 150)'
    ppRR$Color[ppRR$DiasInventario>=10 & ppRR$DiasInventario<=15] <- 'rgb(254,200,99)'
    ppRR$n <- 1
    
    
    box10 <- aggregate(OrdenDeCompra~Tienda, data=inventoryData, FUN=function(x){table(x)['NO']/length(x)*100}) # %>% filter(Tienda %in% as.character(input$TiendaGlobal), Producto %in% as.character(input$ProductoGlobal))
    box10$OrdenDeCompra[is.na(box10$OrdenDeCompra)] <- 0
    box10$Color <- '#32ff96'#'rgb(50, 255, 150)'
    box10$Color[box10$OrdenDeCompra<70] <- '#fe6363'#rgb(254,99,99)'
    
    plot <- highchart() %>%
      hc_chart(event= list(
        selection = JS(
          "function selectPointsByDrag(e) {
          
          // Select points
          Highcharts.each(this.series, function (series) {
          Highcharts.each(series.points, function (point) {
          if (point.x >= e.xAxis[0].min && point.x <= e.xAxis[0].max &&
          point.y >= e.yAxis[0].min && point.y <= e.yAxis[0].max) {
          point.select(true, true);
          }
          });
          });
          
          // Fire a custom event
          Highcharts.fireEvent(this, 'selectedpoints', { points: this.getSelectedPoints() });
          
          return false; // Don't zoom
  }"
        )
      ),
      zoomType= "xy") %>%
      hc_add_series(box10, type="column",
                    hcaes(x = Tienda, y = OrdenDeCompra, color = Color), name = "Dias de Inventario")  %>%
      # hc_add_series(name = "Dias de Inventario", data = box10$OrdenDeCompra, color = box10$Color)  %>%
      hc_add_series(name = 'Inventario Objetivo', 75, type = "line") %>%
      hc_title(
        text = '',
        margin = 20,
        align = "left",
        style = list(useHTML = TRUE) #color = "#22A884", 
      ) %>%
      hc_xAxis(title = list(text = "Tienda"), categories = box10$Tienda  #,
               # opposite = TRUE,
               # plotLines = list(
               #   list(label = list(text = "This is a plotLine"),
               #        color = "#'FF0000",
               #        width = 2,
               #        value = 5.5))
      ) %>% 
      hc_yAxis(title = list(text = "%")#,
               # opposite = TRUE,
               # minorTickInterval = "auto",
               # minorGridLineDashStyle = "LongDashDotDot",
               # showFirstLabel = FALSE,
               # showLastLabel = FALSE,
               # plotBands = list(
               #   list(from = 25, to = 80, color = "rgba(100, 0, 0, 0.1)",
               #        label = list(text = "This is a plotBand")))
      ) %>%
      hc_colors(c("#999999", "#E69F00")) %>%
      # hc_legend(
      #   align = "left",
      #   verticalAlign = "top",
      #   layout = "vertical",
      #   x = 0,
      #   y = 100
      # ) %>% 
      hc_tooltip(
        crosshairs = TRUE,
        borderWidth = 5,
        sort = TRUE,
        table = TRUE
      )
    
    output$box10DT <- renderDT({
      output$box10DT2 <- renderDT({
        # names(box10)[2:3] <- c(paste0('Ventas',year(today())),paste0('Ventas',year(today())-1))
        # for(j in 2:3){
        #   box10[,j] <- paste0('Q',prettyNum(round(box10[,j],2),big.mark = ',', big.interval = 3))
        # }
        datatable(box10,
                  rownames = F, escape = F,filter = 'top',
                  options = list(
                    scrollX = TRUE, scrollY = '400px'
                  )
        )
      })
      datatable(box10,
                rownames = F, escape = F,filter = 'top',
                options = list(
                  scrollX = TRUE, scrollY = '90px'
                )
      )
    })
    output$box10Plot2 <- renderHighchart({
      future({plot})
    })
    future({plot})
  })
  
  
  # COMPRAS ------------------------------------------------------------------
  # box ordenes data --------------
  output$boxordenesData <- renderUI({ 
    div(
      style = "position: relative",
      tabBox(side = 'right',
             title = '',
             id = "Compra Urgente",
             width = NULL,
             height = 450,
             tabPanel(
               title = "Tabla",
               dataTableOutput('ordenesData') %>% withSpinner(type = 4,color = "#d33724",size = 0.7)
             ),
             div(
               style = "position: absolute; right: 0.5em; bottom: 0.5em;",
               conditionalPanel(
                 "input.boxordenesData == 'Tabla'",
                 actionBttn(
                   inputId = "boxordenesDataDTplus",
                   icon = icon("search-plus", class = "opt"),
                   style = "fill",
                   color = "danger",
                   size = "xs"
                 )
               )
             )
             # div(
             #   style = "position: absolute; right: 0.5em; bottom: 0.5em;",
             #   conditionalPanel(
             #     "input.boxordenesData == 'Tabla'",
             #     actionBttn(
             #       inputId = "boxordenesDataDTplus",
             #       icon = icon("search-plus", class = "opt"),
             #       style = "fill",
             #       color = "danger",
             #       size = "xs"
             #     )
             #   )
             # )
      )
    )
  })
  observeEvent((input$boxordenesDataDTplus), {
    showModal(modalDialog(
      dataTableOutput('ordenesData2'),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  # box compras data --------------
  output$boxcomprasData <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        title = '',
        id = "boxcomprasData",
        width = NULL,
        height = 450,
        tabPanel(
          title = "Tabla",
          div(
            withSpinner(
              DT::dataTableOutput('comprasData')%>% withSpinner(type = 4,color = "#d33724",size = 0.7),
              type = 4,
              color = "#d33724",
              size = 0.7
            ),
            downloadBttn(outputId = "boxcomprasDatadtdescarga", label = "", color = 'primary', style = 'material-flat', size = 'xs')
          )
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em;",
          conditionalPanel(
            "input.boxcomprasData == 'Tabla'",
            actionBttn(
              inputId = "boxcomprasDatadtplus",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        )
        
      )
    )
  })
  observeEvent((input$boxcomprasDatadtplus), {
    showModal(modalDialog(
      DT::dataTableOutput('boxcomprasDataDT2', height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
 
  
  #Compras
  output$comprasData <- renderDataTable({
    # inventoryData <- inventoryDataUpdate()
    #REVISAR ESTO, TEMPORAL:
    print(table(inventoryData$Proveedor))
    print(as.character(input$ProveedorGlobalCompras))
    inventoryData <- inventoryData# %>% filter(Proveedor %in% as.character(input$ProveedorGlobalCompras), Tienda %in% as.character(input$TiendaGlobalCompras), Producto %in% as.character(input$ProductoGlobalCompras), Categoria %in% as.character(input$CategoriaGlobalCompras))
    print(table(filter(inventoryData, Proveedor %in% as.character(input$ProveedorGlobalCompras))$Proveedor))
    print(table(filter(inventoryData, Tienda %in% as.character(input$TiendaGlobalCompras))$Proveedor))
    print(table(filter(inventoryData, Producto %in% as.character(input$ProductoGlobalCompras))$Proveedor))
    print(table(filter(inventoryData, Categoria %in% as.character(input$CategoriaGlobalCompras))$Proveedor))
    #
    inventoryData$InventarioDeseado <- inventoryData$InventarioDeseado + input$DiasDeInventarioAdicionales
    
   # try(rm('con'),silent=TRUE);try({con <- dbConnect(odbc(),Driver      = "FreeTDS",Database    = 'DB_BBS',Uid         = 'blueballoon',Pwd         = 'bbs@db#pass!',Server      = '184.168.194.60',Port        = 1433,TDS_Version = 7.4)},silent = TRUE);if(!exists('con')){con <- dbConnect(odbc(),Driver      = "ODBC Driver 17 for SQL Server",Database    = 'DB_BBS',Uid         = 'blueballoon',Pwd         = 'bbs@db#pass!',Server      = '184.168.194.60',Port        = 1433,TDS_Version = 7.4)}
    # TiendasNuevas <- data.frame(Tienda=as.character('Prueba'), Espejo=as.character('F145'), FechaHora=now(), Usuario=as.character('rodrigo'))
    # for(j in 1:ncol(TiendasNuevas)){
    #   TiendasNuevas[,j] <- as.character(TiendasNuevas[,j])
    # }
    # DBI::dbCreateTable(con, 'TiendasNuevasSCM', TiendasNuevas, row.names = NULL)
    # DBI::dbAppendTable(con, 'TiendasNuevasSCM', TiendasNuevas, row.names = NULL)
    try(system.time({
      inventoryData$InventarioTotalDias <- round(inventoryData$Inventario/inventoryData$Prediccion.1.Semana*7)
      inventoryData$InventarioTotalDias[inventoryData$Inventario==0] <- 0
      inventoryData$InventarioTotalDias[inventoryData$Prediccion.1.Semana==0] <- inventoryData$Inventario[inventoryData$Prediccion.1.Semana==0]
      inventoryData$NivelDeInventario <- 'SIN VENTA RECURRENTE'
      inventoryData$NivelDeInventario[inventoryData$InventarioTotalDias >= inventoryData$InventarioDeseado] <- 'BUEN INVENTARIO'
      inventoryData$NivelDeInventario[inventoryData$InventarioTotalDias > 2*inventoryData$InventarioDeseado] <- 'EXCESO INVENTARIO'
      inventoryData$NivelDeInventario[inventoryData$InventarioTotalDias < inventoryData$InventarioDeseado] <- 'REORDEN'
      inventoryData$NivelDeInventario[inventoryData$InventarioTotalDias < inventoryData$Lead.Time] <- 'BAJO INVENTARIO'
      inventoryData$NivelDeInventario[inventoryData$InventarioTotalDias==0] <- 'QUIEBRE'
      
      inventoryData$OrdenDeCompra <- 'NO'
      inventoryData$OrdenDeCompra[inventoryData$NivelDeInventario=='REORDEN'] <- 'SI'
      inventoryData$OrdenDeCompra[inventoryData$NivelDeInventario=='SIN VENTA RECURRENTE'] <- 'REVISAR'
      inventoryData$OrdenDeCompra[inventoryData$NivelDeInventario=='QUIEBRE'] <- 'SI - URGENTE'
      inventoryData$OrdenDeCompra[inventoryData$NivelDeInventario=='BAJO INVENTARIO'] <- 'SI - URGENTE'
      
      
      inventoryData <- inventoryData %>% mutate(CompraUrgente=ifelse(InventarioDeseado>InventarioTotalDias,(InventarioDeseado-InventarioTotalDias)*(Unidades/7),0),
                                                CompraPronto=0,#ifelse((Inventario+Transito)<(Prediccion.1.Semana+Unidades), Prediccion.1.Semana-ifelse((Inventario-Unidades)>0,Inventario-Unidades,0)-Transito,0),
                                                CompraBien=0)#ifelse((Inventario+Transito)<(Prediccion.2.Semanas+Prediccion.1.Semana+Unidades), Prediccion.2.Semanas-ifelse((Inventario-Unidades)>0,Inventario-Unidades,0)-Transito,0))
      inventoryData <- inventoryData %>% mutate(SobreInventario=ifelse(InventarioTotalDias>InventarioDeseado*2, (InventarioTotalDias-InventarioDeseado*2)*Prediccion.1.Semana,0))
      inventoryData <- inventoryData[!duplicated(inventoryData),]
      inventoryData$CompraUrgente[is.na(inventoryData$CompraUrgente)] <- 0
      inventoryData$Minimo[is.na(inventoryData$Minimo) | inventoryData$Minimo==0] <- 1
      inventoryData[(inventoryData$Inventario==0 & inventoryData$CompraUrgente==0),'CompraUrgente'] <- inventoryData[(inventoryData$Inventario==0 & inventoryData$CompraUrgente==0),'Minimo']
      inventoryData$CompraUrgente[inventoryData$InventarioTotalDias==0 & inventoryData$CompraUrgente==0] <- inventoryData$Minimo[inventoryData$InventarioTotalDias==0 & inventoryData$CompraUrgente==0]
      
      inventoryData$Costo[is.na(inventoryData$Costo)] <- 0
      inventoryData$CompraUrgente[is.na(inventoryData$CompraUrgente)] <- inventoryData$Minimo[is.na(inventoryData$CompraUrgente)]
      inventoryData$CompraUrgente[(inventoryData$Inventario==0 & inventoryData$CompraUrgente<inventoryData$Minimo)] <- inventoryData$Minimo[(inventoryData$Inventario<=0 & inventoryData$CompraUrgente<inventoryData$Minimo)]
      inventoryData$Costo <- round(inventoryData$Costo,2)
      # if(file.exists('tiendasactivas.RData')){ #tiendasactivas.R
      #   source('tiendasactivas.R')
      #   inventoryData <- filter(inventoryData,Sucursal %in% unique(substring(tiendasactivas,1,4)))
      # }
      inventoryData <- mutate(inventoryData,CompraUrgente=round((CompraUrgente/Minimo)+0.2))
      inventoryData$CompraUrgente <- inventoryData$CompraUrgente*inventoryData$Minimo
      
      inventoryData <- inventoryData[!duplicated(inventoryData[,c('Producto','Tienda','weekyr')]),]
      inventoryData$CompraUrgente[inventoryData$Inventario==0 & inventoryData$CompraUrgente==0] <- inventoryData$Minimo[inventoryData$Inventario==0 & inventoryData$CompraUrgente==0]
    }))
    # Inactivos <- data.frame(Tienda=as.character('Prueba'), Producto="7401001500316", FechaHora=now(), Usuario=as.character('rodrigo'))
    # for(j in 1:ncol(Inactivos)){
    #   Inactivos[,j] <- as.character(Inactivos[,j])
    # }
    Inactivos <- DBI::dbGetQuery(con, paste("select * from [blueballoon].[InactivosSCM] where wd ='",wd,"'",sep=''))
    inventoryData <- inventoryData %>% mutate(Si=paste(Sucursal,Sku)) %>% filter(!Si %in% paste(Inactivos$Tienda, Inactivos$Producto)) %>% select(-Si)
    print(paste('2 compras',table(inventoryData$Proveedor)))
    ddd <- filter(inventoryData,Tienda %in% as.character(input$TiendaGlobalCompras), Producto %in% as.character(input$ProductoGlobalCompras), Proveedor %in% as.character(input$ProveedorGlobalCompras), Categoria %in% as.character(input$CategoriaGlobalCompras)) %>% arrange(Clasificacion)
    if(length(input$Tipo)>0){
      ddd <- arrange(select(mutate(ddd,Inventario.Total=Inventario+Transito, Prediccion.Esta.Semana=Unidades),Producto, Sku, Tienda, Inventario.Total, Lead.Time, Prediccion.Esta.Semana, Prediccion.1.Semana, Prediccion.2.Semanas, Prediccion.3.Semanas, CompraUrgente, CompraPronto, CompraBien, Costo, Clasificacion, InventarioTotalDias), desc(CompraUrgente))
      ddd <- ddd[(ddd[,(input$Tipo)]>0),]
      ddd <- ddd[complete.cases(ddd),]
    }
    else{
      ddd <- arrange(select(mutate(ddd,Inventario.Total=Inventario+Transito, Prediccion.Esta.Semana=Unidades),Producto, Sku, Tienda, Inventario.Total, Lead.Time, Prediccion.Esta.Semana, Prediccion.1.Semana, Prediccion.2.Semanas, Prediccion.3.Semanas, CompraUrgente, CompraPronto, CompraBien, Costo, Clasificacion, InventarioTotalDias), desc(CompraUrgente))
    }
    if(input$Inventario=="Con"){
      ddd <- filter(ddd, Inventario.Total>0)
    }
    if(input$Inventario=="Sin"){
      ddd <- filter(ddd, Inventario.Total==0)
    }
    
    ddd <- mutate(arrange(ddd, Clasificacion),CumTot = cumsum(CompraUrgente*Costo)) %>% select(-Costo)
    # #print(head(ddd,25))
    if(input$LimitePresupuesto>0){
      ddd <- filter(ddd, CumTot<input$LimitePresupuesto) %>% select(-Clasificacion)
    }
    
    output$compraurgente <- renderText({
      paste(nrow(filter(ddd, CompraUrgente>0)), "Productos")
    })
    output$comprapronto <- renderText({
      paste(nrow(filter(ddd, CompraPronto>0)), "Productos")
    })
    output$compraMurgente <- DT::renderDT({
      dd <- ddd %>% arrange(desc(CompraUrgente))
      dd <- dd[1:5, c("Producto","CompraUrgente")]
      dd
    })
    output$comprabien <- renderText({
      paste(nrow(filter(ddd, CompraUrgente==0, CompraPronto==0)), "Productos")
    })
    output$comprabien2 <- renderText({
      paste(nrow(filter(ddd, CompraUrgente==0, CompraPronto==0)), "Productos")
    })
    output$comprasData2 <- renderDataTable({
      DT::datatable(ddd %>% select(-CumTot),options = list(scrollX = TRUE)) 
    })
    print(paste('3 compras',table(inventoryData$Proveedor)))
    DT::datatable(ddd %>% select(-CumTot),options = list(scrollX = TRUE))
    output$boxcomprasDataDT2 <- renderDataTable({
      DT::datatable(ddd %>% select(-CumTot),options = list(scrollX = TRUE))
    })
  })
  
  #Ordenes de Compra
  output$ordenesData <- renderDataTable({
    #inventoryData <- inventoryDataUpdate()
    inventoryData$InventarioDeseado <- inventoryData$InventarioDeseado + input$DiasDeInventarioAdicionales
    
    try(rm('con'),silent=TRUE);try({con <- dbConnect(odbc(),Driver      = "FreeTDS",Database    = 'DB_BBS',Uid         = 'blueballoon',Pwd         = 'bbs@db#pass!',Server      = '184.168.194.60',Port        = 1433,TDS_Version = 7.4)},silent = TRUE);if(!exists('con')){con <- dbConnect(odbc(),Driver      = "ODBC Driver 17 for SQL Server",Database    = 'DB_BBS',Uid         = 'blueballoon',Pwd         = 'bbs@db#pass!',Server      = '184.168.194.60',Port        = 1433,TDS_Version = 7.4)}
    # TiendasNuevas <- data.frame(Tienda=as.character('Prueba'), Espejo=as.character('F145'), FechaHora=now(), Usuario=as.character('rodrigo'))
    # for(j in 1:ncol(TiendasNuevas)){
    #   TiendasNuevas[,j] <- as.character(TiendasNuevas[,j])
    # }
    # DBI::dbCreateTable(con, 'TiendasNuevasSCM', TiendasNuevas, row.names = NULL)
    # DBI::dbAppendTable(con, 'TiendasNuevasSCM', TiendasNuevas, row.names = NULL)
    inventoryData$Costo[inventoryData$Costo>1000] <- 0
    try(system.time({
      inventoryData$InventarioTotalDias <- round(inventoryData$Inventario/inventoryData$Prediccion.1.Semana*7)
      inventoryData$InventarioTotalDias[inventoryData$Inventario==0] <- 0
      inventoryData$InventarioTotalDias[inventoryData$Prediccion.1.Semana==0] <- inventoryData$Inventario[inventoryData$Prediccion.1.Semana==0]
      inventoryData$NivelDeInventario <- 'SIN VENTA RECURRENTE'
      inventoryData$NivelDeInventario[inventoryData$InventarioTotalDias >= inventoryData$InventarioDeseado] <- 'BUEN INVENTARIO'
      inventoryData$NivelDeInventario[inventoryData$InventarioTotalDias > 2*inventoryData$InventarioDeseado] <- 'EXCESO INVENTARIO'
      inventoryData$NivelDeInventario[inventoryData$InventarioTotalDias < inventoryData$InventarioDeseado] <- 'REORDEN'
      inventoryData$NivelDeInventario[inventoryData$InventarioTotalDias < inventoryData$Lead.Time] <- 'BAJO INVENTARIO'
      inventoryData$NivelDeInventario[inventoryData$InventarioTotalDias==0] <- 'QUIEBRE'
      
      inventoryData$OrdenDeCompra <- 'NO'
      inventoryData$OrdenDeCompra[inventoryData$NivelDeInventario=='REORDEN'] <- 'SI'
      inventoryData$OrdenDeCompra[inventoryData$NivelDeInventario=='SIN VENTA RECURRENTE'] <- 'REVISAR'
      inventoryData$OrdenDeCompra[inventoryData$NivelDeInventario=='QUIEBRE'] <- 'SI - URGENTE'
      inventoryData$OrdenDeCompra[inventoryData$NivelDeInventario=='BAJO INVENTARIO'] <- 'SI - URGENTE'
      
      
      inventoryData <- inventoryData %>% mutate(CompraUrgente=ifelse(InventarioDeseado>InventarioTotalDias,(InventarioDeseado-InventarioTotalDias)*(Unidades/7),0),
                                                CompraPronto=0,#ifelse((Inventario+Transito)<(Prediccion.1.Semana+Unidades), Prediccion.1.Semana-ifelse((Inventario-Unidades)>0,Inventario-Unidades,0)-Transito,0),
                                                CompraBien=0)#ifelse((Inventario+Transito)<(Prediccion.2.Semanas+Prediccion.1.Semana+Unidades), Prediccion.2.Semanas-ifelse((Inventario-Unidades)>0,Inventario-Unidades,0)-Transito,0))
      inventoryData <- inventoryData %>% mutate(SobreInventario=ifelse(InventarioTotalDias>InventarioDeseado*2, (InventarioTotalDias-InventarioDeseado*2)*Prediccion.1.Semana,0))
      inventoryData <- inventoryData[!duplicated(inventoryData),]
      inventoryData$CompraUrgente[is.na(inventoryData$CompraUrgente)] <- 0
      inventoryData$Minimo[is.na(inventoryData$Minimo) | inventoryData$Minimo==0] <- 1
      inventoryData[(inventoryData$Inventario==0 & inventoryData$CompraUrgente==0),'CompraUrgente'] <- inventoryData[(inventoryData$Inventario==0 & inventoryData$CompraUrgente==0),'Minimo']
      inventoryData$CompraUrgente[inventoryData$InventarioTotalDias==0 & inventoryData$CompraUrgente==0] <- inventoryData$Minimo[inventoryData$InventarioTotalDias==0 & inventoryData$CompraUrgente==0]
      
      inventoryData$Costo[is.na(inventoryData$Costo)] <- 0
      inventoryData$CompraUrgente[is.na(inventoryData$CompraUrgente)] <- inventoryData$Minimo[is.na(inventoryData$CompraUrgente)]
      inventoryData$CompraUrgente[(inventoryData$Inventario==0 & inventoryData$CompraUrgente<inventoryData$Minimo)] <- inventoryData$Minimo[(inventoryData$Inventario<=0 & inventoryData$CompraUrgente<inventoryData$Minimo)]
      inventoryData$Costo <- round(inventoryData$Costo,2)
      # if(file.exists('tiendasactivas.RData')){ #tiendasactivas.R
      #   source('tiendasactivas.R')
      #   inventoryData <- filter(inventoryData,Sucursal %in% unique(substring(tiendasactivas,1,4)))
      # }
      inventoryData <- mutate(inventoryData,CompraUrgente=round((CompraUrgente/Minimo)+0.2))
      inventoryData$CompraUrgente <- inventoryData$CompraUrgente*inventoryData$Minimo
      
      inventoryData <- inventoryData[!duplicated(inventoryData[,c('Producto','Tienda','weekyr')]),]
      inventoryData$CompraUrgente[inventoryData$Inventario==0 & inventoryData$CompraUrgente==0] <- inventoryData$Minimo[inventoryData$Inventario==0 & inventoryData$CompraUrgente==0]
    }))
    # Inactivos <- data.frame(Tienda=as.character('Prueba'), Producto="7401001500316", FechaHora=now(), Usuario=as.character('rodrigo'))
    # for(j in 1:ncol(Inactivos)){
    #   Inactivos[,j] <- as.character(Inactivos[,j])
    # }
    Inactivos <- DBI::dbGetQuery(con, paste("select * from [blueballoon].[InactivosSCM] where wd ='",wd,"'",sep=''))
    inventoryData <- inventoryData %>% mutate(Si=paste(Sucursal,Sku)) %>% filter(!Si %in% paste(Inactivos$Tienda, Inactivos$Producto)) %>% select(-Si)
    
    #print('inventoryData')
    #print(dim(inventoryData))
    
    xx <- filter(inventoryData, CompraUrgente>0, Tienda %in% as.character(input$TiendaGlobal), Producto %in% as.character(input$ProductoGlobal)) %>% mutate(Inventario.Total=Inventario+Transito)
    
    xx <- mutate(arrange(xx, Clasificacion),CumTot = cumsum(CompraUrgente*Costo))
    # # #print(head(xx,25))
    if(input$LimitePresupuesto>0){
      xx <- filter(xx, CumTot<input$LimitePresupuesto) %>% select(-Clasificacion)
    }
    
    #print('xx')
    #print(dim(xx))
    
    output$CostoCompra <- renderText({
      paste('Q.',format(round(sum(mutate(xx,Total=Costo*CompraUrgente)$Total),2), nsmall=1, big.mark=","), sep='')
    })
    
    observeEvent(input$EnviarERP, {
      shinyalert(title = 'Orden enviada a ERP', type = 'success')
    })
    
    output$DescargarOrden <- downloadHandler(
      filename = function(){
        # #print('download')
        paste0(paste('Orden', Sys.Date(), sep='-'),".zip")
      },
      content = function(file){
        #print('descargando')
        shinyalert(title = 'Iniciando descarga', type = 'success')
        #go to a temp dir to avoid permission issues
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        files <- NULL;
        
        if(input$DescargaPorProveedor){
          #loop through the sheets
          for(prov in unique(ProveedorGlobal())){
            try({
              
              # for(tnds in unique(as.character(input$TiendaGlobal))){
              #print(prov)
              # prods2 <- filter(proveedores, Proveedor %in% prov)[,1]
              compraData <- select(filter(xx, Proveedor %in% prov), Producto, Tienda, Sku, Inventario.Total, Cantidad=CompraUrgente, Costo, Lead.Time, Proveedor) %>% mutate(Total=Costo*Cantidad) #, Tienda %in% tnds
              # compraData <- select(inventoryData %>% mutate(Inventario.Total=Inventario+Transito), Producto, Tienda, Sku, Inventario.Total, Cantidad=CompraUrgente, Costo, Lead.Time, Proveedor) %>% mutate(Total=Costo*Cantidad); input <- data.frame(TipoOrden='Prueba', TipoPrecio=1, AutorizadoPor=1, Observaciones='')
              compraData <- compraData[!duplicated(compraData),] %>% arrange(Proveedor, Tienda, Cantidad)
              compraData$OrdenDeCompraTipo <- input$TipoOrden
              compraData$`Tipo de Precio` <- input$TipoPrecio
              compraData$`Autorizado por` <- input$AutorizadoPor
              compraData$Observaciones <- input$Observaciones
              if(exists('formatoDescarga')){
                compraData <- compraData %>% mutate(Sucursal=substring(Tienda,1,4))
                compraData <- merge(compraData, IDsucursal, all.x=TRUE)
                LlaveProveedor$Sku2 <- toupper(trimws(as.character(LlaveProveedor$Sku)))
                LlaveProveedor$LlaveProveedor <- as.character(LlaveProveedor$LlaveProveedor)
                compraData <- compraData %>% mutate(Sku2=toupper(Sku))
                compraData <- merge(compraData, LlaveProveedor %>% select(-Sku), all.x=TRUE)
                compraData$LlaveProveedor[is.na(compraData$LlaveProveedor) & compraData$Proveedor=='LOTERIA DEL NIÃÂÃÂO'] <- '98099015-1'
                compraData <- ddply(compraData%>% dplyr::arrange(Tienda, Proveedor), .(Tienda, Proveedor),
                                    mutate, `Numero Linea` = seq_along(Sku))
                OrdenCompraTipo$Sucursal <- trimws(as.character(OrdenCompraTipo$Sucursal))
                compraData <- merge(compraData, OrdenCompraTipo, all.x=TRUE)
                compraData$IdOrdenCompra[is.na(compraData$IdOrdenCompra)] <- 0
                compraData$`Id Orden Compra Tipo` <- compraData$IdOrdenCompra
                compraData$`Tipo de Precio` <- input$TipoPrecio
                compraData$`Autorizado por` <- input$AutorizadoPor
                compraData$Observaciones <- compraData$Proveedor
                compraData <- compraData %>%
                  dplyr::rename(`Id Sucursal`=idSucursal,
                                `Llave Proveedor`=LlaveProveedor,
                                `Llave Producto`=Sku
                  ) %>% mutate(`Fecha Ofrecido`=today()+Lead.Time, Fecha=today(),`Codigo Medida`='ST')
                compraData$`Llave Producto` <- paste0('"=""', compraData$`Llave Producto`, '"""')
                compraData <- select_at(compraData, names(formatoDescarga))
              }
              else{
                compraData$Sku <- paste0('"=""', compraData$Sku, '"""')
              }
              #write each sheet to a csv file, save the name
              if(nrow(compraData)>0){
                ID <- 1
                try({if(file.exists('ordenes.RData')){
                  orden <- ordenes
                  ID<-max(ordenes$ID)+1
                }
                  
                  ordenes <- compraData %>% data.frame(ID=ID, FechaHora=now(), Status='Descargada') #ordenes <- data.frame(ID=as.numeric(1), FechaHora=now(), Sku='1fc', Cantidad=0, Tienda='s', Status='Descargada') , Sku, Cantidad, Tienda)
                  # #print(ordenes)
                  if(file.exists('ordenes.RData')){
                    ordenes <- plyr::rbind.fill(ordenes, orden) %>% arrange(FechaHora)
                  }
                  save(list='ordenes', file='ordenes.RData')
                }, silent=T)
                
                for(j in 1:ncol(compraData)){
                  compraData[,j] <- as.character(compraData[,j])
                }
                
                fileName <- paste('OrdenId', ID, prov, '.csv', sep='-')
                prods2 <- filter(proveedores, Proveedor == prov)[,1]
                write.csv(compraData, fileName, row.names = F, quote = F)
                # write_csv(compraData, fileName)
                files <- c(fileName,files)
              }
              # }
            }, silent=TRUE)
          }
        }
        if(!input$DescargaPorProveedor){
          compraData <- select(xx, Producto, Tienda, Sku, Inventario.Total, Cantidad=CompraUrgente, Costo, Lead.Time, Proveedor) %>% mutate(Total=Costo*Cantidad) #, Tienda %in% tnds
          # compraData <- select(inventoryData %>% mutate(Inventario.Total=Inventario+Transito), Producto, Tienda, Sku, Inventario.Total, Cantidad=CompraUrgente, Costo, Lead.Time, Proveedor) %>% mutate(Total=Costo*Cantidad) #, Tienda %in% tnds
          compraData <- compraData[!duplicated(compraData),] %>% arrange(Proveedor, Tienda, Cantidad)
          compraData$OrdenDeCompraTipo <- input$TipoOrden
          compraData$`Tipo de Precio` <- input$TipoPrecio
          compraData$`Autorizado por` <- input$AutorizadoPor
          compraData$Observaciones <- input$Observaciones
          if(exists('formatoDescarga')){
            compraData <- compraData %>% mutate(Sucursal=substring(Tienda,1,4))
            compraData <- merge(compraData, IDsucursal, all.x=TRUE)
            LlaveProveedor$Sku2 <- toupper(trimws(as.character(LlaveProveedor$Sku)))
            LlaveProveedor$LlaveProveedor <- as.character(LlaveProveedor$LlaveProveedor)
            compraData <- compraData %>% mutate(Sku2=toupper(Sku))
            compraData <- merge(compraData, LlaveProveedor %>% select(-Sku), all.x=TRUE)
            compraData <- ddply(compraData %>% arrange(Tienda, Proveedor),
                                .(Tienda, Proveedor), mutate,
                                `Numero Linea` = seq_along(Sku))
            OrdenCompraTipo$Sucursal <- trimws(as.character(OrdenCompraTipo$Sucursal))
            compraData <- merge(compraData, OrdenCompraTipo, all.x=TRUE)
            compraData$`Id Orden Compra Tipo` <- compraData$IdOrdenCompra
            compraData$`Id Orden Compra Tipo`[is.na(compraData$`Id Orden Compra Tipo`)] <- 0
            compraData$`Tipo de Precio` <- input$TipoPrecio
            compraData$`Autorizado por` <- input$AutorizadoPor
            compraData$Observaciones <- compraData$Proveedor
            compraData <- compraData %>%
              dplyr::rename(`Id Sucursal`=idSucursal,
                            `Llave Proveedor`=LlaveProveedor,
                            `Llave Producto`=Sku
              ) %>% mutate(`Fecha Ofrecido`=today()+Lead.Time, Fecha=today(),`Codigo Medida`='ST')
            compraData$`Llave Producto` <- paste0('"=""', compraData$`Llave Producto`, '"""')
            compraData <- select_at(compraData, names(formatoDescarga))
          }
          else{
            compraData$Sku <- paste0('"=""', compraData$Sku, '"""')
          }
          #write each sheet to a csv file, save the name
          if(nrow(compraData)>0){
            ID <- 1
            try({if(file.exists('ordenes.RData')){
              orden <- ordenes
              ID<-max(ordenes$ID)+1
            }
              
              ordenes <- compraData %>% data.frame(ID=ID, FechaHora=now(), Status='Descargada') #ordenes <- data.frame(ID=as.numeric(1), FechaHora=now(), Sku='1fc', Cantidad=0, Tienda='s', Status='Descargada') , Sku, Cantidad, Tienda)
              # #print(ordenes)
              if(file.exists('ordenes.RData')){
                ordenes <- plyr::rbind.fill(ordenes, orden) %>% arrange(FechaHora)
              }
              save(list='ordenes', file='ordenes.RData')
            })
            
            
            for(j in 1:ncol(compraData)){
              compraData[,j] <- as.character(compraData[,j])
            }
            
            fileName <- paste('OrdenId', ID, '.csv', sep='-')
            write.csv(compraData, fileName, row.names = F, quote = F)
            # write_csv(compraData, fileName)
            files <- c(fileName,files)
          }
        }
        
        zip(zipfile = file,files = files)
      }
    )
    output$ordenesData2 <- renderDataTable({
      DT::datatable(select(xx, Producto, Tienda, Sku, Inventario.Total, Cantidad=CompraUrgente,Lead.Time)
                    ,options = list(scrollX = TRUE))
    })
    DT::datatable(select(xx, Producto, Tienda, Sku, Inventario.Total, Cantidad=CompraUrgente,Lead.Time)
                  ,options = list(scrollX = TRUE))
  })
  
  observeEvent(input$SubmitTiendaNueva,{
    try(rm('con'),silent=TRUE);try({con <- dbConnect(odbc(),Driver      = "FreeTDS",Database    = 'DB_BBS',Uid         = 'blueballoon',Pwd         = 'bbs@db#pass!',Server      = '184.168.194.60',Port        = 1433,TDS_Version = 7.4)},silent = TRUE);if(!exists('con')){con <- dbConnect(odbc(),Driver      = "ODBC Driver 17 for SQL Server",Database    = 'DB_BBS',Uid         = 'blueballoon',Pwd         = 'bbs@db#pass!',Server      = '184.168.194.60',Port        = 1433,TDS_Version = 7.4)}
    try(user <- session$user,silent=TRUE)
    if(!exists('user')){
      user <- 'SCM'
    }
    if(is.null(user)){
      user <- 'SCM'
    }
    #print(user); #print(wd); #print(input$TiendaNueva); #print(input$TiendaEspejo)
    TiendasNuevas <- data.frame(Tienda=as.character(input$TiendaNueva), Espejo=as.character(input$TiendaEspejo), FechaHora=now(), Usuario=as.character(user), wd=wd)
    for(j in 1:ncol(TiendasNuevas)){
      TiendasNuevas[,j] <- as.character(TiendasNuevas[,j])
    }
    
    # query <- sqlAppendTable(con = con, table = 'TiendasNuevasSCM', values = TiendasNuevas, row.names = FALSE )
    for(i in 1:nrow(TiendasNuevas)){
      query <- paste("INSERT INTO [DB_BBS].[blueballoon].[TiendasNuevasSCM]
                     ",
                     # paste(respuestasDF2[,1], collapse = ", "),
                     "
                     VALUES
                     ('",paste(paste(TiendasNuevas[i,], collapse = "', '"),"')",sep=''), sep='')
      
      #print(query)  
      queryResult <-try(DBI::dbExecute(con, query))
      #print(queryResult)
    }
    
    if(queryResult>0){
      shinyalert('Exito!', 'La tienda se ha agregado a la base de datos!', type = 'success')
    }
    if(queryResult==0){
      shinyalert('Error!', 'La tienda ya existe en la base de datos!', type = 'error')
    }
  })
  
  output$ActInactDT <- renderDT({
    try(rm('con'),silent=TRUE);try({con <- dbConnect(odbc(),Driver      = "FreeTDS",Database    = 'DB_BBS',Uid         = 'blueballoon',Pwd         = 'bbs@db#pass!',Server      = '184.168.194.60',Port        = 1433,TDS_Version = 7.4)},silent = TRUE);if(!exists('con')){con <- dbConnect(odbc(),Driver      = "ODBC Driver 17 for SQL Server",Database    = 'DB_BBS',Uid         = 'blueballoon',Pwd         = 'bbs@db#pass!',Server      = '184.168.194.60',Port        = 1433,TDS_Version = 7.4)}
    Inactivos <- DBI::dbGetQuery(con, paste("select * from [blueballoon].[InactivosSCM] where wd ='",wd,"'",sep=''))
    Inactivos
  })
  
  observeEvent(input$SubmitActInact,{
    try(rm('con'),silent=TRUE);try({con <- dbConnect(odbc(),Driver      = "FreeTDS",Database    = 'DB_BBS',Uid         = 'blueballoon',Pwd         = 'bbs@db#pass!',Server      = '184.168.194.60',Port        = 1433,TDS_Version = 7.4)},silent = TRUE);if(!exists('con')){con <- dbConnect(odbc(),Driver      = "ODBC Driver 17 for SQL Server",Database    = 'DB_BBS',Uid         = 'blueballoon',Pwd         = 'bbs@db#pass!',Server      = '184.168.194.60',Port        = 1433,TDS_Version = 7.4)}
    if(input$EstadoActInact=='Inactivar'){
      Inactivos <- expand.grid(Tienda=as.character(input$TiendaActInact), Producto=as.character(input$ProductoActInact), FechaHora=now(), Usuario=as.character('TM'), wd)
      for(j in 1:ncol(Inactivos)){
        Inactivos[,j] <- as.character(Inactivos[,j])
      }
      for(i in 1:nrow(Inactivos)){
        query <- paste("INSERT INTO [DB_BBS].[blueballoon].[InactivosSCM]
                       ",
                       # paste(respuestasDF2[,1], collapse = ", "),
                       "
                       VALUES
                       ('",paste(paste(Inactivos[i,], collapse = "', '"),"')",sep=''), sep='')
        #print(query)  
        queryResult <-try(DBI::dbExecute(con, query))
        #print(queryResult)
      }
    }
    if(input$EstadoActInact=='Activar'){
      for(prod in input$ProductoActInact){
        for(tnd in input$TiendaActInact){
          query <- paste("DELETE FROM [blueballoon].[InactivosSCM] WHERE Producto ='",paste(prod,paste("'",paste(" AND Tienda ='",paste(tnd,"'",sep=''),sep=''), sep=''),sep=''), sep='')
          # #print(query)
          queryResult <- DBI::dbSendQuery(con, query)
        }
      }
    }
    # if(queryResult==1){
    #   
    # }
    # if(queryResult==0){
    #   shinyalert('Error!', 'La tienda ya existe en la base de datos!', type = 'error')
    # }
    shinyalert('Exito!', 'Los cambios han sido guardados!', type = 'success')
    output$ActInactDT <- renderDT({
      try(rm('con'),silent=TRUE);try({con <- dbConnect(odbc(),Driver      = "FreeTDS",Database    = 'DB_BBS',Uid         = 'blueballoon',Pwd         = 'bbs@db#pass!',Server      = '184.168.194.60',Port        = 1433,TDS_Version = 7.4)},silent = TRUE);if(!exists('con')){con <- dbConnect(odbc(),Driver      = "ODBC Driver 17 for SQL Server",Database    = 'DB_BBS',Uid         = 'blueballoon',Pwd         = 'bbs@db#pass!',Server      = '184.168.194.60',Port        = 1433,TDS_Version = 7.4)}
      Inactivos <- DBI::dbGetQuery(con, paste("select * from [blueballoon].[InactivosSCM] where wd ='",wd,"'",sep=''))
      Inactivos
    })
  })
  
  output$AutorizadoPorUI <- renderUI({
    textInput('AutorizadoPor','Autorizado Por',value = session$user)
  })
  
  DiasVisitaProveedoresReact <- reactiveVal(DiasVisitaProveedores)
  
  
  
  
  
  # CONFIGURACIONES ---------------------------------
  observeEvent(input$add_btn, {
    t = rbind(data.frame(Sucursal = as.character(input$SucursalInput), Proveedor = as.character(input$ProveedorInput),
                         Nombre.Vendedor=as.character(input$Nombre.VendedorInput), Telefono=as.character(input$TelefonoInput),
                         Monday=as.character(input$MondayInput), Tuesday=as.character(input$TuesdayInput), Wednesday=as.character(input$WednesdayInput),
                         Thursday=as.character(input$ThursdayInput), Friday=as.character(input$FridayInput), Saturday=as.character(input$SaturdayInput),
                         Cliente=as.character(cliente))
              , DiasVisitaProveedoresReact())
    DiasVisitaProveedoresReact(t)
  })
  
  observeEvent(input$delete_btn, {
    t = DiasVisitaProveedoresReact()
    queryResult <- 0
    t <- filter(t, toupper(Nombre.Vendedor) == 'BORRAR')
    #print(t)
    
    try(rm('con'),silent=TRUE);try({con <- dbConnect(odbc(),Driver      = "FreeTDS",Database    = 'DB_BBS',Uid         = 'blueballoon',Pwd         = 'bbs@db#pass!',Server      = '184.168.194.60',Port        = 1433,TDS_Version = 7.4)},silent = TRUE);if(!exists('con')){con <- dbConnect(odbc(),Driver      = "ODBC Driver 17 for SQL Server",Database    = 'DB_BBS',Uid         = 'blueballoon',Pwd         = 'bbs@db#pass!',Server      = '184.168.194.60',Port        = 1433,TDS_Version = 7.4)}
    for(i in 1){
      # DBI::dbGetQuery(con, paste("select * from [blueballoon].[DiasVisitaProveedores] WHERE Cliente =",paste("'",cliente,"'",sep='')))
      query <- paste("DELETE FROM [DB_BBS].[blueballoon].[DiasVisitaProveedores] WHERE Cliente = '",t$Cliente[i],"'","AND Sucursal = '",t$Sucursal[i],"'","AND Proveedor = '",t$Proveedor[i],"'",sep='')
      queryResult1 <- try(DBI::dbExecute(con, query))
      queryResult <- queryResult1+queryResult
    }
    
    shinyalert(title = paste(queryResult,'rows deleted'), type = 'success')
    # #print(paste(queryResult,'rows added'))
    # #print(nrow(t))
    # if (!is.null(input$shiny_table_rows_selected)) {
    #   t <- t[-as.numeric(input$shiny_table_rows_selected),]
    # }
    # t <- t[-1,]
    # DiasVisitaProveedoresReact(t)
    t = DiasVisitaProveedoresReact()
    t <- filter(t, toupper(Nombre.Vendedor) != 'BORRAR')
    DiasVisitaProveedoresReact(t)
  })
  
  output$shiny_table <- renderDT({
    DiasVisitaProveedores <- DiasVisitaProveedoresReact()
    for(j in 1:ncol(DiasVisitaProveedores)){
      DiasVisitaProveedores[,j] <- as.character(DiasVisitaProveedores[,j])
    }
    datatable(DiasVisitaProveedores %>% select(-Cliente), editable = T, options = list(scrollX = TRUE))
  })
  
  observeEvent(input$shiny_table_cell_edit, {
    cell <- input$shiny_table_cell_edit
    newdf <- DiasVisitaProveedoresReact()
    for(j in 1:ncol(newdf)){
      newdf[,j] <- as.character(newdf[,j])
    }
    newdf[cell$row, cell$col] <- cell$value
    DiasVisitaProveedoresReact(newdf)
  })
  # comprasAjustes box
  
  output$boxcomprasAjustes <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        title = '',
        id = "boxcomprasAjustes",
        width = NULL,
        height = 450,
        
        tabPanel(
          title = "Tabla",
          div(
            withSpinner(
              DT::dataTableOutput('boxfiltrosDT') %>% withSpinner(type = 4,color = "#d33724",size = 0.7),
              type = 4,
              color = "#d33724",
              size = 0.7
            ),
            downloadBttn(outputId = "boxcomprasAjustesdtdescarga", label = "", color = 'primary', style = 'material-flat', size = 'xs')
          )
        )
        
      )
    )
  })
  
  output$boxfiltrosDT <- renderDataTable({
    datatable(filtrosDT,rownames = F, escape = F,filter = 'top',
              options = list(
                pageLength = 5,scrollX = TRUE, dom = 'tp'
              ))
  })
  # output$actdvp_btn <- downloadHandler(
  #   filename = function() {
  #     "mydata.csv"
  #   },
  #   content = function(file) {
  #     dfDownload <- DiasVisitaProveedoresReact()
  #     for(j in 1:ncol(dfDownload)){
  #       dfDownload[,j] <- as.character(dfDownload[,j])
  #     }
  #     try(cell <- input$shiny_table_cell_edit)
  #     try(cell)
  #     if(!is.null(cell)){
  #       dfDownload <- dfDownload[cell$row,]
  #     }
  #     write.csv(dfDownload, file, row.names = FALSE)
  #   }
  # )
  
  observeEvent(input$actdvp_btn,{
    dfDownload <- DiasVisitaProveedoresReact()
    for(j in 1:ncol(dfDownload)){
      dfDownload[,j] <- as.character(dfDownload[,j])
    }
    try(cell <- input$shiny_table_cell_edit)
    try(cell)
    queryResult <- 0
    if(!is.null(cell)){
      dfDownload <- dfDownload[cell$row,]
      try(rm('con'),silent=TRUE);try({con <- dbConnect(odbc(),Driver      = "FreeTDS",Database    = 'DB_BBS',Uid         = 'blueballoon',Pwd         = 'bbs@db#pass!',Server      = '184.168.194.60',Port        = 1433,TDS_Version = 7.4)},silent = TRUE);if(!exists('con')){con <- dbConnect(odbc(),Driver      = "ODBC Driver 17 for SQL Server",Database    = 'DB_BBS',Uid         = 'blueballoon',Pwd         = 'bbs@db#pass!',Server      = '184.168.194.60',Port        = 1433,TDS_Version = 7.4)}
      for(i in 1:nrow(dfDownload)){
        # DBI::dbGetQuery(con, paste("select * from [blueballoon].[DiasVisitaProveedores] WHERE Cliente =",paste("'",cliente,"'",sep='')))
        query <- paste("DELETE FROM [DB_BBS].[blueballoon].[DiasVisitaProveedores] WHERE Cliente = '",dfDownload$Cliente[i],"'","AND Sucursal = '",dfDownload$Sucursal[i],"'","AND Proveedor = '",dfDownload$Proveedor[i],"'",sep='')
        try(DBI::dbExecute(con, query))
        query <- paste("INSERT INTO [DB_BBS].[blueballoon].[DiasVisitaProveedores]",
                       " VALUES ('",paste(paste(dfDownload[i,], collapse = "', '"),"')"), sep='')
        queryResult1 <- try(DBI::dbExecute(con, query))
        queryResult <- queryResult1+queryResult
      }
    }
    shinyalert(title = paste(queryResult,'rows updated'), type = 'success')
    #print(paste(queryResult,'rows added'))
  })
  
  observeEvent(input$submit_btn,{
    DiasVisitaProveedores <- DiasVisitaProveedoresReact()
    names(DiasVisitaProveedores2)
    names(DiasVisitaProveedores)
    DiasVisitaProveedores$Si <- as.character(apply( DiasVisitaProveedores[ , 1:ncol(DiasVisitaProveedores)] , 1 , paste , collapse = "-" ))
    DiasVisitaProveedores2$Si <- as.character(apply( DiasVisitaProveedores2[ , 1:ncol(DiasVisitaProveedores2)] , 1 , paste , collapse = "-" ))
    # #print(DiasVisitaProveedores2$Si)
    DiasVisitaProveedores <- filter(DiasVisitaProveedores, !Si %in% DiasVisitaProveedores2$Si) %>% select(-Si)
    for(j in names(DiasVisitaProveedores)[!names(DiasVisitaProveedores) %in% c('Telefono',weekdays(seq.Date(today(),today()+6,'day')))]){
      DiasVisitaProveedores[,j] <- as.character(DiasVisitaProveedores[,j])
    }
    for(j in names(DiasVisitaProveedores)[names(DiasVisitaProveedores) %in% c('Telefono',weekdays(seq.Date(today(),today()+6,'day')))]){
      DiasVisitaProveedores[,j] <- as.numeric(as.character(DiasVisitaProveedores[,j]))
    }
    queryResult <- 0
    for(i in 1:nrow(DiasVisitaProveedores)){
      #print(i)
      try(rm('con'),silent=TRUE);try({con <- dbConnect(odbc(),Driver      = "FreeTDS",Database    = 'DB_BBS',Uid         = 'blueballoon',Pwd         = 'bbs@db#pass!',Server      = '184.168.194.60',Port        = 1433,TDS_Version = 7.4)},silent = TRUE);if(!exists('con')){con <- dbConnect(odbc(),Driver      = "ODBC Driver 17 for SQL Server",Database    = 'DB_BBS',Uid         = 'blueballoon',Pwd         = 'bbs@db#pass!',Server      = '184.168.194.60',Port        = 1433,TDS_Version = 7.4)}
      # DBI::dbGetQuery(con, paste("select * from [blueballoon].[DiasVisitaProveedores] WHERE Cliente =",paste("'",cliente,"'",sep='')))
      query <- paste("INSERT INTO [DB_BBS].[blueballoon].[DiasVisitaProveedores]",
                     " VALUES ('",paste(paste(DiasVisitaProveedores[i,], collapse = "', '"),"')"), sep='')
      #print(query)
      queryResult1 <- try(DBI::dbExecute(con, query))
      #print('result')
      #print(queryResult1)
      # if(class(queryResult1)!='numeric'){
      #   queryResult1 <- 0
      # }
      try(queryResult <- queryResult1+queryResult)
      #print(queryResult)
    }
    shinyalert(title = paste(queryResult,'rows added'), type = 'success')
  })
  
  output$keepAlive <- renderText({
    req(input$count)
    paste("keep alive ", input$count)
  })
  
  observeEvent(input$SubmitDinCom,{
    try(rm('con'),silent=TRUE);try({con <- dbConnect(odbc(),Driver      = "FreeTDS",Database    = 'DB_BBS',Uid         = 'blueballoon',Pwd         = 'bbs@db#pass!',Server      = '184.168.194.60',Port        = 1433,TDS_Version = 7.4)},silent = TRUE);if(!exists('con')){con <- dbConnect(odbc(),Driver      = "ODBC Driver 17 for SQL Server",Database    = 'DB_BBS',Uid         = 'blueballoon',Pwd         = 'bbs@db#pass!',Server      = '184.168.194.60',Port        = 1433,TDS_Version = 7.4)}
    DinCom <- expand.grid(Tienda=as.character(input$TiendaDinCom), Producto=as.character(input$ProductoDinCom), FechaHora=now(), Usuario=as.character('TM'), FechaFin=as.character(input$FechaDinCom), Aumento=as.character(input$AumentoDinCom), wd)
    for(j in 1:ncol(DinCom)){
      DinCom[,j] <- as.character(DinCom[,j])
    }
    # queryResult <- DBI::dbAppendTable(con, 'DinamicasComercialesSCM', DinCom, row.names = NULL)
    for(i in 1:nrow(DinCom)){
      query <- paste("INSERT INTO [DB_BBS].[blueballoon].[DinamicasComercialesSCM]
                     ",
                     # paste(respuestasDF2[,1], collapse = ", "),
                     "
                     VALUES
                     ('",paste(paste(DinCom[i,], collapse = "', '"),"')",sep=''), sep='')
      #print(query)  
      queryResult <-try(DBI::dbExecute(con, query))
      #print(queryResult)
    }
    
    shinyalert('Exito!', 'Los cambios han sido guardados!', type = 'success')
  })
  
  # output$clicked <- renderText({
  #   input$box1click
  # })
  # output$clickedplus <- renderText({
  #   input$box1clickplus
  # })
  
  output$keepAlive <- renderText({
    req(input$count)
    # paste("keep alive ", input$count)
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)

