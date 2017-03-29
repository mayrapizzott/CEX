if (!require("install.load")) install.packages("install.load")
install.load::install_load("shiny", "RODBC", "openxlsx", "RPostgreSQL")

t0000<-Sys.time()

source("Z:\\github\\connect.inc")

ui <- fluidPage(
 
  tabsetPanel(

    tabPanel(title = "Exportacao",
             h3(em ("Exportacao 1997-2016 NCM 8 Digitos")),
             sidebarPanel(
               fluidRow(
                 column(width = 6, selectInput(inputId = 'date1_exp', 
                                               label = 'Data inicial', 
                                               choices = c(Selecione = "", 
                                                           sqlQuery(sql,"SELECT (CAST(NUM_ANO AS VARCHAR(4)) + RIGHT(('00'+ CAST(NUM_MES AS VARCHAR(5))),2)) AS 'Selecione o periodo inicial' 
                                                                        FROM (SELECT NUM_ANO, NUM_MES 
                                                                              FROM [MPP].[imp].[Exportacao]
                                                                              WHERE NUM_ANO>=1997
                                                                              GROUP BY NUM_ANO, NUM_MES) A
                                                                        ORDER BY 'Selecione o periodo inicial'")), 
                                               selected = "", 
                                               multiple = FALSE, 
                                               selectize = TRUE)),
                 
                 column(width = 6, selectInput(inputId = 'date2_exp', 
                                               label = 'Data final', 
                                               choices = c(Selecione = "", 
                                                           sqlQuery(sql,"SELECT (CAST(NUM_ANO AS VARCHAR(4)) + RIGHT(('00'+ CAST(NUM_MES AS VARCHAR(5))),2)) AS 'Selecione o periodo final' 
                                                                        FROM (SELECT NUM_ANO, NUM_MES 
                                                                              FROM [MPP].[imp].[Exportacao] 
                                                                              WHERE NUM_ANO>=1997
                                                                              GROUP BY NUM_ANO, NUM_MES) A
                                                                        ORDER BY 'Selecione o periodo final'")), 
                                               selected = "", 
                                               multiple = FALSE, 
                                               selectize = TRUE)),
                 
                 column(width = 12, selectInput(inputId = 'uf_exp', 
                                                label = 'Estado',  
                                                choices = c(Selecione = "", 
                                                            "Agregar por cada estado",
                                                             sqlQuery(sql,"SELECT [Dsc_UF] as 'Selecione um ou mais estados'
                                                                          FROM [MPP].[dbo].[UF] 
                                                                          ORDER BY Dsc_UF")), 
                                                selected = "", 
                                                multiple = TRUE, 
                                                selectize = TRUE)),
                 
                 column(width = 12, selectInput(inputId = 'porto_exp', 
                                                label = 'Porto', 
                                                choices = c(Selecione = "",
                                                            "Agregar por cada porto",
                                                            sqlQuery(sql, "SELECT [Nme_Porto] as 'Selecione um ou mais portos'
                                                                          FROM [MPP].[dbo].[Porto] 
                                                                          ORDER BY Nme_Porto")), 
                                                selected = "", 
                                                multiple = TRUE,
                                                selectize = TRUE)),
                 
                 column(width = 12, selectInput(inputId = 'pais_exp', 
                                                label = 'Pais',
                                                choices = c(Selecione="", 
                                                            "Agregar por cada pais", 
                                                            sqlQuery(sql, "SELECT [Nme_Pais] as 'Selecione um ou mais paises'
                                                                          FROM [MPP].[dbo].[Pais] 
                                                                          ORDER BY Nme_Pais")), 
                                                selected = "", 
                                                multiple = TRUE, 
                                                selectize = TRUE)),
                 
                 column(width = 12, selectInput(inputId = 'via_exp', 
                                                label = 'Via', 
                                                choices = c(Selecione = "",
                                                            "Agregar por cada via",
                                                             sqlQuery(sql, "SELECT [Dsc_Via] as 'Selecione uma ou mais vias'
                                                                          FROM [MPP].[ro].[dim_Via] 
                                                                          ORDER BY Dsc_Via")), 
                                                selected = "", 
                                                multiple = TRUE, 
                                                selectize = TRUE)),
                 
                 column(width = 12, selectInput(inputId = 'ncm_exp', 
                                                label = 'Codigo NCM', 
                                                choices = c(Selecione = "", 
                                                            "Agregar por cada NCM",
                                                            sqlQuery(sql, "SELECT [Cod_NCM] as 'Selecione um ou mais codigos'
                                                                          FROM [MPP].[dbo].[NCM] 
                                                                          ORDER BY Cod_NCM")), 
                                                selected = "", 
                                                multiple = TRUE, 
                                                selectize = TRUE)),
                 
                 column(width = 12, selectInput(inputId = 'nome_ncm_exp', 
                                                label = 'Nome NCM', 
                                                choices = c(Selecione = "", 
                                                            "Agregar por cada nome", 
                                                            sqlQuery(sql, "SELECT [Dsc_NCM] as 'Selecione um ou mais nomes'
                                                                          FROM [MPP].[dbo].[NCM] 
                                                                          ORDER BY Dsc_NCM")), 
                                                selected = "", 
                                                multiple = TRUE, 
                                                selectize = TRUE))
                 ),
               actionButton(inputId = "expo", label = "Gerar tabela"),
               downloadButton(outputId = "downloadExp", label = "Download")
                 ),
             mainPanel(
               dataTableOutput(outputId="Exp"))),
   
    tabPanel(title = "Importacao",
             h3(em ("Importacao 1997-2016 NCM 8 Digitos")),
             sidebarPanel(
               fluidRow(
                 column(width = 6, selectInput(inputId = 'date1_imp', 
                                               label = 'Data inicial', 
                                               choices = c(Selecione = "", 
                                                           sqlQuery(sql,"SELECT (CAST(NUM_ANO AS VARCHAR(4)) + RIGHT(('00'+ CAST(NUM_MES AS VARCHAR(5))),2)) AS 'Selecione o periodo inicial'
                                                                        FROM (SELECT NUM_ANO, NUM_MES 
                                                                              FROM [MPP].[imp].[Importacao]
                                                                              WHERE NUM_ANO>=1997
                                                                              GROUP BY NUM_ANO, NUM_MES) A
                                                                        ORDER BY 'Selecione o periodo inicial'")), 
                                               selected = "", 
                                               multiple = FALSE, 
                                               selectize = TRUE)),
                 
                 column(width = 6, selectInput(inputId = 'date2_imp', 
                                               label = 'Data final', 
                                               choices = c(Selecione = "", 
                                                           "Todos", 
                                                           sqlQuery(sql,"SELECT (CAST(NUM_ANO AS VARCHAR(4)) + RIGHT(('00'+ CAST(NUM_MES AS VARCHAR(5))),2)) AS 'Selecione o periodo final'
                                                                        FROM (SELECT NUM_ANO, NUM_MES 
                                                                              FROM [MPP].[imp].[Importacao] 
                                                                              WHERE NUM_ANO>=1997
                                                                              GROUP BY NUM_ANO, NUM_MES) A
                                                                        ORDER BY 'Selecione o periodo final'")), 
                                               selected = "", 
                                               multiple = FALSE, 
                                               selectize = TRUE)),
                 
                 column(width = 12, selectInput(inputId = 'uf_imp', 
                                                label = 'Estado',  
                                                choices = c(Selecione = "", 
                                                            "Agregar por cada estado",
                                                            sqlQuery(sql,"SELECT [Dsc_UF] as 'Selecione um ou mais estados'
                                                                         FROM [MPP].[dbo].[UF] 
                                                                         ORDER BY Dsc_UF")), 
                                                selected = "", 
                                                multiple = TRUE, 
                                                selectize = TRUE)),
                 
                 column(width = 12, selectInput(inputId = 'porto_imp', 
                                                label = 'Porto', 
                                                choices = c(Selecione = "",
                                                            "Agregar por cada porto",
                                                            sqlQuery(sql, "SELECT [Nme_Porto] as 'Selecione um ou mais portos'
                                                                          FROM [MPP].[dbo].[Porto] 
                                                                          ORDER BY Nme_Porto")), 
                                                selected = "", 
                                                multiple = TRUE,
                                                selectize = TRUE)),
                 
                 column(width = 12, selectInput(inputId = 'pais_imp', 
                                                label = 'Pais',
                                                choices = c(Selecione="", 
                                                            "Agregar por cada pais", 
                                                             sqlQuery(sql, "SELECT [Nme_Pais] as 'Selecione um ou mais paises'
                                                                          FROM [MPP].[dbo].[Pais] 
                                                                          ORDER BY Nme_Pais")), 
                                                selected = "", 
                                                multiple = TRUE, 
                                                selectize = TRUE)),
                 
                 column(width = 12, selectInput(inputId = 'via_imp', 
                                                label = 'Via', 
                                                choices = c(Selecione = "",
                                                            "Agregar por cada via",
                                                             sqlQuery(sql, "SELECT [Dsc_Via] as 'Selecione uma ou mais vias'
                                                                          FROM [MPP].[ro].[dim_Via] 
                                                                          ORDER BY Dsc_Via")), 
                                                selected = "", 
                                                multiple = TRUE, 
                                                selectize = TRUE)),
                 
                 column(width = 12, selectInput(inputId = 'ncm_imp', 
                                                label = 'Codigo NCM', 
                                                choices = c(Selecione = "", 
                                                            "Agregar por cada NCM",
                                                             sqlQuery(sql, "SELECT [Cod_NCM] as 'Selecione um ou mais codigos'
                                                                          FROM [MPP].[dbo].[NCM] 
                                                                          ORDER BY Cod_NCM")), 
                                                selected = "", 
                                                multiple = TRUE, 
                                                selectize = TRUE)),
                 
                 column(width = 12, selectInput(inputId = 'nome_ncm_imp', 
                                                label = 'Nome NCM', 
                                                choices = c(Selecione = "", 
                                                            "Agregar por cada nome", 
                                                             sqlQuery(sql, "SELECT [Dsc_NCM] as 'Selecione um ou mais nomes'
                                                                          FROM [MPP].[dbo].[NCM] 
                                                                          ORDER BY Dsc_NCM")), 
                                                selected = "", 
                                                multiple = TRUE, 
                                                selectize = TRUE))
                 ),
               actionButton(inputId = "impo", label = "Gerar tabela"),
               downloadButton(outputId = "downloadImp", label = "Download")
                 ),
             mainPanel(
               dataTableOutput(outputId="Imp")))
      )
        )



t1111<-Sys.time()

# t1111-t0000
# Time difference of 16.15703 secs

server <- function(input, output) {
  
  
  



  tab_ncm_exp<-reactive({ if (is.null(input$ncm_exp)) { "SELECT  Cod_NCM,
                                                                                   'Todos' as Cod_NCM_2
                                                                            FROM [MPP].[dbo].[NCM]"}

                          else if (input$ncm_exp=="Agregar por cada NCM") { "SELECT Cod_NCM, 
                                                                                    Cod_NCM as Cod_NCM_2
                                                                             FROM [MPP].[dbo].[NCM]"}
                          else {paste("SELECT Cod_NCM,
                                              Cod_NCM as Cod_NCM_2
                                       FROM [MPP].[dbo].[NCM]
                                       WHERE Cod_NCM IN (", paste(paste("'",input$ncm_exp, collapse="',", sep=""), "'", sep=""), ")") } })


  tab_nome_ncm_exp<-reactive({ if (is.null(input$nome_ncm_exp)) { "SELECT Cod_ncm,
                                                                                              'Todos' as Dsc_ncm
                                                                                        FROM [MPP].[dbo].[NCM]"}
    
                               else if (input$nome_ncm_exp=="Agregar por cada nome") { "SELECT Cod_ncm,
                                                                                               Dsc_ncm
                                                                                        FROM [MPP].[dbo].[NCM]"}
    
                               else {paste("SELECT Cod_ncm, 
                                                   Dsc_ncm
                                            FROM [MPP].[dbo].[NCM] 
                                            WHERE Dsc_ncm IN (", paste(paste("'",input$nome_ncm_exp, collapse="',", sep=""), "'", sep=""), ")") } })
  


  tab_uf_exp<-reactive({ if (is.null(input$uf_exp)) { "SELECT Cod_uf,
                                                                                    'Todos' as Dsc_uf
                                                                              FROM [MPP].[dbo].[uf]"}
    
                         else if (input$uf_exp=="Agregar por cada estado") { "SELECT Cod_uf,
                                                                                     Dsc_uf
                                                                              FROM [MPP].[dbo].[uf]"}
                         else {paste("SELECT Cod_uf, 
                                             Dsc_uf
                                      FROM [MPP].[dbo].[uf] 
                                      WHERE Dsc_uf IN (", paste(paste("'",input$uf_exp, collapse="',", sep=""), "'", sep=""), ")") } })
  

  tab_porto_exp<-reactive({ if (is.null(input$porto_exp)) { "SELECT Cod_porto,
                                                                                          'Todos' as Nme_porto
                                                                                   FROM [MPP].[dbo].[Porto]"}
    
                            else if (input$porto_exp=="Agregar por cada porto") { "SELECT Cod_porto,
                                                                                          Nme_porto
                                                                                    FROM [MPP].[dbo].[Porto]"}
                            else {paste("SELECT Cod_porto, 
                                                Nme_porto
                                         FROM [MPP].[dbo].[Porto] 
                                         WHERE Nme_porto IN (", paste(paste("'",input$porto_exp, collapse="',", sep=""), "'", sep=""), ")") } })
  
 
 
tab_pais_exp<-reactive({ if (is.null(input$pais_exp)) { "SELECT Cod_Pais,
                                                                                      'Todos' as Nme_Pais
                                                                               FROM [MPP].[dbo].[Pais]"}
                    
                         else if (input$pais_exp=="Agregar por cada pais") { "SELECT Cod_Pais,
                                                                                     Nme_Pais
                                                                              FROM [MPP].[dbo].[Pais]"}
                         else {paste("SELECT Cod_Pais, 
                                             Nme_Pais
                                      FROM [MPP].[dbo].[Pais] 
                                      WHERE Nme_Pais IN (", paste(paste("'",input$pais_exp, collapse="',", sep=""), "'", sep=""), ")") } })

tab_via_exp<-reactive({ if (is.null(input$via_exp)) { "SELECT Cod_Via,
                                                                                  'Todos' as Dsc_via
                                                                           FROM [MPP].[ro].[dim_Via]"}
  
                        else if (input$via_exp=="Agregar por cada via") { "SELECT Cod_via,
                                                                                  Dsc_via
                                                                           FROM [MPP].[ro].[dim_Via]"}
                        else {paste("SELECT Cod_via, 
                                            Dsc_via
                                     FROM [MPP].[ro].[dim_Via] 
                                     WHERE Dsc_via IN (", paste(paste("'",input$via_exp, collapse="',", sep=""), "'", sep=""), ")") } })

  tab_exp<-eventReactive(input$expo, {sqlQuery(sql,
                                               paste("SELECT  A.COD_NCM_2 AS NCM,
                                                              
                                                     B.Dsc_NCM AS NOME_NCM,
                                                     DATE as DATA,
                                                     DSC_UF as UF,
                                                     Nme_Porto as PORTO,
                                                     Nme_Pais as PAIS,
                                                     Dsc_Via AS VIA, 
                                                     SUM(NUM_PESO_LIQUIDO) AS PESO_TOTAL,
                                                     SUM(NUM_VALOR_FOB) AS VALOR_TOTAL
                                                     
                                                     FROM (", tab_ncm_exp(),") A
                                                     
                                                     INNER JOIN
                                                     
                                                     (", tab_nome_ncm_exp(), ") B
                                                     
                                                     
                                                     ON A.Cod_NCM=B.Cod_NCM
                                                     
                                                     INNER JOIN
                                                     
                                                     (SELECT * 
                                                     FROM (SELECT *, 
                                                     (CAST(NUM_ANO AS VARCHAR(4)) + RIGHT(('00'+ CAST(NUM_MES AS VARCHAR(5))),2)) AS DATE  
                                                     FROM [MPP].[imp].[Exportacao]) AA
                                                     WHERE DATE BETWEEN", input$date1_exp, "AND",  input$date2_exp,  " AND NUM_ANO>=1997) C
                                                     
                                                     ON B.Cod_NCM=C.Cod_NCM
                                                     
                                                     INNER JOIN 
                                                     
                                                     (", tab_uf_exp(), ") D
                                                     
                                                     ON C.Cod_UF=D.COD_UF
                                                     
                                                     INNER JOIN 
                                                     
                                                     (", tab_porto_exp(), ") E
                                                     
                                                     ON C.COD_PORTO=E.Cod_Porto
                                                     
                                                     INNER JOIN 
                                                     
                                                     (", tab_pais_exp(), ") F
                                                     
                                                     ON C.COD_PAIS=F.Cod_Pais
                                                     
                                                     INNER JOIN 
                                                     
                                                     (", tab_via_exp(), ") G
                                                     
                                                     ON C.COD_VIA=G.Cod_Via
                                                     
                                                     GROUP BY A.COD_NCM_2, 
                                                     B.Dsc_NCM ,
                                                     DATE,
                                                     DSC_UF ,
                                                     Nme_Porto ,
                                                     Nme_Pais,
                                                     Dsc_Via "))}) 
  

  
  tab_ncm_imp<-reactive({ if (is.null(input$ncm_imp)) { "SELECT  Cod_NCM,
                                                                                   'Todos' as Cod_NCM_2
                                                                            FROM [MPP].[dbo].[NCM]"}
    
                          else if (input$ncm_imp=="Agregar por cada NCM") { "SELECT Cod_NCM, 
                                                                                    Cod_NCM as Cod_NCM_2
                                                                             FROM [MPP].[dbo].[NCM]"}
                         else {paste("SELECT Cod_NCM,
                                             Cod_NCM as Cod_NCM_2
                                      FROM [MPP].[dbo].[NCM]
                                      WHERE Cod_NCM IN (", paste(paste("'",input$ncm_imp, collapse="',", sep=""), "'", sep=""), ")") } })
  

  tab_nome_ncm_imp<-reactive({ if (is.null(input$nome_ncm_imp)) { "SELECT Cod_ncm,
                                                                                               'Todos' as Dsc_ncm
                                                                                        FROM [MPP].[dbo].[NCM]"}
    
                               else if (input$nome_ncm_imp=="Agregar por cada nome") { "SELECT Cod_ncm,
                                                                                               Dsc_ncm
                                                                                        FROM [MPP].[dbo].[NCM]"}
    
                               else {paste("SELECT Cod_ncm, 
                                                   Dsc_ncm
                                            FROM [MPP].[dbo].[NCM] 
                                            WHERE Dsc_ncm IN (", paste(paste("'",input$nome_ncm_imp, collapse="',", sep=""), "'", sep=""), ")") } })
  
  
  
  tab_uf_imp<-reactive({ if (is.null(input$uf_imp)) { "SELECT Cod_uf,
                                                                                     'Todos' as Dsc_uf
                                                                              FROM [MPP].[dbo].[uf]"}
    
                         else if (input$uf_imp=="Agregar por cada estado") { "SELECT Cod_uf,
                                                                                     Dsc_uf
                                                                              FROM [MPP].[dbo].[uf]"}
                         else {paste("SELECT Cod_uf, 
                                             Dsc_uf
                                      FROM [MPP].[dbo].[uf] 
                                      WHERE Dsc_uf IN (", paste(paste("'",input$uf_imp, collapse="',", sep=""), "'", sep=""), ")") } })
  
  
  tab_porto_imp<-reactive({ if (is.null(input$porto_imp)) { "SELECT Cod_porto,
                                                                                          'Todos' as Nme_porto
                                                                                   FROM [MPP].[dbo].[Porto]"}
    
                            else if (input$porto_imp=="Agregar por cada porto") { "SELECT Cod_porto,
                                                                                          Nme_porto
                                                                                   FROM [MPP].[dbo].[Porto]"}
                            else {paste("SELECT Cod_porto, 
                                                Nme_porto
                                         FROM [MPP].[dbo].[Porto] 
                                         WHERE Nme_porto IN (", paste(paste("'",input$porto_imp, collapse="',", sep=""), "'", sep=""), ")") } })
  
  
  
  tab_pais_imp<-reactive({ if (is.null(input$pais_imp)) { "SELECT Cod_Pais,
                                                                                        'Todos' as Nme_Pais
                                                                                 FROM [MPP].[dbo].[Pais]"}
    
                           else if (input$pais_imp=="Agregar por cada pais") { "SELECT Cod_Pais,
                                                                                       Nme_Pais
                                                                                FROM [MPP].[dbo].[Pais]"}
                           else {paste("SELECT Cod_Pais, 
                                               Nme_Pais
                                        FROM [MPP].[dbo].[Pais] 
                                        WHERE Nme_Pais IN (", paste(paste("'",input$pais_imp, collapse="',", sep=""), "'", sep=""), ")") } })
  
  
  
  tab_via_imp<-reactive({ if (is.null(input$via_imp)) { "SELECT Cod_Via,
                                                                'Todos' as Dsc_via
                                                         FROM [MPP].[ro].[dim_Via]"}
    
                          else if (input$via_imp=="Agregar por cada via") { "SELECT Cod_via,
                                                                                    Dsc_via
                                                                             FROM [MPP].[ro].[dim_Via]"}
                          else {paste("SELECT Cod_via, 
                                              Dsc_via
                                       FROM [MPP].[ro].[dim_Via] 
                                       WHERE Dsc_via IN (", paste(paste("'",input$via_imp, collapse="',", sep=""), "'", sep=""), ")") } })
  
  tab_imp<-eventReactive(input$impo, {sqlQuery(sql,
                                               paste("SELECT  A.COD_NCM_2 AS NCM,
                                                     
                                                     B.Dsc_NCM AS NOME_NCM,
                                                     DATE as DATA,
                                                     DSC_UF as UF,
                                                     Nme_Porto as PORTO,
                                                     Nme_Pais as PAIS,
                                                     Dsc_Via AS VIA, 
                                                     SUM(NUM_PESO_LIQUIDO) AS PESO_TOTAL,
                                                     SUM(NUM_VALOR_FOB) AS VALOR_TOTAL
                                                     
                                                     FROM (", tab_ncm_imp(),") A
                                                     
                                                     INNER JOIN
                                                     
                                                     (", tab_nome_ncm_imp(), ") B
                                                     
                                                     
                                                     ON A.Cod_NCM=B.Cod_NCM
                                                     
                                                     INNER JOIN
                                                     
                                                     (SELECT * 
                                                     FROM (SELECT *, 
                                                     (CAST(NUM_ANO AS VARCHAR(4)) + RIGHT(('00'+ CAST(NUM_MES AS VARCHAR(5))),2)) AS DATE  
                                                     FROM [MPP].[imp].[Importacao]) AA
                                                     WHERE DATE BETWEEN", input$date1_imp, "AND",  input$date2_imp,  " AND NUM_ANO>=1997) C
                                                     
                                                     ON B.Cod_NCM=C.Cod_NCM
                                                     
                                                     INNER JOIN 
                                                     
                                                     (", tab_uf_imp(), ") D
                                                     
                                                     ON C.Cod_UF=D.COD_UF
                                                     
                                                     INNER JOIN 
                                                     
                                                     (", tab_porto_imp(), ") E
                                                     
                                                     ON C.COD_PORTO=E.Cod_Porto
                                                     
                                                     INNER JOIN 
                                                     
                                                     (", tab_pais_imp(), ") F
                                                     
                                                     ON C.COD_PAIS=F.Cod_Pais
                                                     
                                                     INNER JOIN 
                                                     
                                                     (", tab_via_imp(), ") G
                                                     
                                                     ON C.COD_VIA=G.Cod_Via
                                                     
                                                     GROUP BY A.COD_NCM_2, 
                                                     B.Dsc_NCM ,
                                                     DATE,
                                                     DSC_UF ,
                                                     Nme_Porto ,
                                                     Nme_Pais,
                                                     Dsc_Via "))}) 
  
 
  
  output$downloadExp <- downloadHandler(
    filename = function (){ 
      paste("Exporta", ".xlsx", sep="") 
    },
    content = function(file) {
      write.xlsx(tab_exp(), file)
    }
  )
  output$downloadImp <- downloadHandler(
    filename = function (){ 
      paste("Importa", ".xlsx", sep="") 
    },
    content = function(file) {
      write.xlsx(tab_imp(), file)
    }
  )
  
  output$Exp <- renderDataTable (tab_exp())
  output$Imp <- renderDataTable (tab_imp())
  }

t2222<-Sys.time()

# t2222-t0000
# Time difference of 16.62435 secs

shinyApp(ui = ui, server = server)
