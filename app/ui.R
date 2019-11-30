# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        # Application title
        titlePanel("-----------"),
        # Sidebar with a slider input for number of bins
        sidebarLayout(
            sidebarPanel(width = 3,
                         style = "",  # Para mantener fija la selección
                         
                         conditionalPanel("",
                                          selectInput("",
                                                      "", 
                                                      choices = '',
                                                      selected = ""
                                          )                 
                         ),
                         conditionalPanel("",
                                          selectInput("",
                                                      "", 
                                                      choices = '',
                                                      selected = ""
                                          )                 
                         ),
                         conditionalPanel("", 
                                          selectInput("",
                                                      "", 
                                                      choices = "",
                                                      selected = ""
                                          )
                         ),
                         conditionalPanel(condition = "",
                                          selectInput("",
                                                      "",
                                                      choices = '',
                                                      selected = ""
                                          )
                         ),
                         conditionalPanel(condition = "",
                                          selectInput(inputId = "",
                                                      label = "",
                                                      choices = "",
                                                      selected = ""
                                          )
                         )
            ),
            
            # Show a plot of the generated distribution
            mainPanel(width = 9,
                      navbarPage(
                          title = "Mercado Laboral",
                          id = "navbarPage",
                          position = "fixed-top", 
                          tabPanel(
                              title = "Inicio",
                              value = 'inicio',
                              tabsetPanel(
                                  type = "pills", 
                                  id   = "tabsetInicio",
                                  tabPanel(
                                      title = "Introducción", 
                                      value = 'intro'
                                  ),
                                  tabPanel(
                                      title = "Codebook",
                                      value = 'codebook'
                                  )
                              )
                          ),
                          tabPanel(
                              title = "Portales", 
                              value = 'arima',
                              tabsetPanel(
                                  type = "pills", 
                                  id   = "tabNacional", 
                                  tabPanel(
                                      title = "Portales Laborales", # Los portales van a ser un selectInput
                                      value = 'serie',
                                      fluidRow(
                                      )
                                  ),
                                  tabPanel(
                                      title = "Computrabajo",
                                      value = 'autocorrelograma',
                                      fluidRow()
                                  ),
                                  tabPanel(
                                      title = "Buscojobs", 
                                      value = "armaToMa"
                                  ),
                                  tabPanel(
                                      title = "Predicción",
                                      value = "prediccion"
                                  )
                              )
                          ),
                          
                          tabPanel(
                              title = "Demanda Laboral",
                              value = "estacionalidad",
                              tabsetPanel(
                                  type = "pills",
                                  id = "tabsetEstacionalidad",
                                  tabPanel(
                                      title = "Estacionalidad",
                                      value = "estacionalidadIntro",
                                      fluidRow()
                                  ),
                                  tabPanel(
                                      title = "Desestacionalización",
                                      value = "desestacionalizacion",
                                      fluidRow()
                                  )
                              )
                          ),
                          
                          
                          tabPanel(
                              title = "Curva de Beveridge", 
                              value = 'espectral',
                              tabsetPanel(
                                  type = "pills",
                                  id = "tabsetDpto",
                                  tabPanel(
                                      title = "departamento",
                                      value = "dpto1",
                                      fluidRow()
                                  ),
                                  tabPanel(
                                      title = "departamento2",
                                      value = 'dpto2',
                                      fluidRow()
                                  ),
                                  tabPanel(
                                      title = "departamento2",
                                      value = 'dpto3',
                                      fluidRow()
                                  )
                              )
                          ),
                          tabPanel(
                              title = "Metalearning",
                              value = "metalearning",
                              tabsetPanel(
                                  type = "pills",
                                  id = "tabsetMetalearning",
                                  tabPanel(
                                      title = "Metalearning",
                                      value = "metalearning",
                                      fluidRow()
                                  )
                              )
                          )
                      )
            )
        )
    ))    
