# ================================================================

#
# Soporta:
# A) Ítems en filas / jueces en columnas
# B) Jueces en filas / ítems en columnas
#
# Incluye:
# 1. Configuración y carga
# 2. Data de jueces
# 3. V de Aiken por ítem + IC + V total
# 4. Homogeneidad entre jueces
# 5. Gráfico de puntos con IC
# 6. Comparación de dos grupos de jueces
#
# IC asimétricos mediante método Score
# Merino & Livia (2009)
# Penfield & Giacobbi (2004)
# ================================================================


# ================================================================
# PAQUETES
# ================================================================

library(shiny)
library(readxl)
library(dplyr)
library(DT)
library(ggplot2)
library(tidyr)


# ================================================================
# FUNCIÓN V DE AIKEN
# ================================================================

calcular_aiken <- function(matriz,
                           minimo = 1,
                           maximo = 4) {

  k <- maximo - minimo

  medias <- rowMeans(
    matriz,
    na.rm = TRUE
  )

  V <- (medias - minimo) / k

  return(V)
}


# ================================================================
# INTERVALO DE CONFIANZA SCORE
# ================================================================

aiken_score_ci <- function(V,
                           n,
                           k,
                           conf.level = 0.95) {

  alpha <- 1 - conf.level

  z <- qnorm(
    1 - alpha / 2
  )

  denominador <- 2 * (
    n * k + z^2
  )

  raiz <- sqrt(
    4 * n * k * V * (1 - V) +
      z^2
  )

  LI <- (
    2 * n * k * V +
      z^2 -
      z * raiz
  ) / denominador

  LS <- (
    2 * n * k * V +
      z^2 +
      z * raiz
  ) / denominador

  LI <- pmax(
    0,
    LI
  )

  LS <- pmin(
    1,
    LS
  )

  data.frame(
    LI = LI,
    LS = LS
  )
}


# ================================================================
# HOMOGENEIDAD ENTRE JUECES
# ================================================================

calcular_h <- function(matriz_jueces) {

  if (ncol(matriz_jueces) < 2) {
    return(NA_real_)
  }

  C <- cor(
    matriz_jueces,
    use = "pairwise.complete.obs"
  )

  valores <- C[
    upper.tri(C)
  ]

  mean(
    valores,
    na.rm = TRUE
  )
}


# ================================================================
# DETECTAR ORIENTACIÓN
# ================================================================

detectar_orientacion <- function(df) {

  nombres <- names(df)

  columnas_expert <- grep(
    "^Expert",
    nombres,
    value = TRUE,
    ignore.case = TRUE
  )

  columnas_item <- grep(
    "^Item",
    nombres,
    value = TRUE,
    ignore.case = TRUE
  )

  if (length(columnas_expert) >= 2) {
    return("items_filas")
  }

  if (length(columnas_item) >= 2) {
    return("jueces_filas")
  }

  return("desconocido")
}


# ================================================================
# DATOS DEMO - FORMATO A
# ================================================================

demo_A <- data.frame(

  Item = paste0(
    "Item ",
    1:12
  ),

  Expert1 =
    c(4,4,4,3,4,4,3,4,4,4,3,4),

  Expert2 =
    c(4,3,4,4,4,3,4,4,3,4,3,4),

  Expert3 =
    c(4,4,4,4,3,4,4,4,4,4,3,4),

  Expert4 =
    c(4,4,3,4,4,4,4,4,4,3,4,4),

  Expert5 =
    c(4,4,4,4,4,4,4,4,4,4,3,4),

  Expert6 =
    c(3,4,4,4,4,4,3,4,4,4,4,4)
)


# ================================================================
# DATOS DEMO - FORMATO B
# ================================================================

demo_B <- data.frame(

  Juez = paste0(
    "Juez",
    sprintf("%02d", 1:8)
  ),

  Grupo = c(
    "Clínicos",
    "Clínicos",
    "Clínicos",
    "Clínicos",
    "Académicos",
    "Académicos",
    "Académicos",
    "Académicos"
  ),

  Item1  = c(4,4,3,4,4,3,4,4),
  Item2  = c(4,3,4,4,3,4,4,3),
  Item3  = c(4,4,4,4,3,4,4,4),
  Item4  = c(3,4,4,4,4,4,3,4),
  Item5  = c(4,4,3,4,4,3,4,4),
  Item6  = c(4,3,4,4,3,4,4,4),
  Item7  = c(3,4,4,3,4,4,4,4),
  Item8  = c(4,4,4,4,4,3,4,4),
  Item9  = c(4,3,4,4,4,4,3,4),
  Item10 = c(4,4,4,3,4,4,4,4)
)


# ================================================================
# INTERFAZ
# ================================================================

ui <- fluidPage(

  tags$head(

    tags$style(

      HTML("

      body {
        background-color:#f5f7fa;
        font-family:Arial, Helvetica, sans-serif;
        color:#172554;
      }

      .contenedor {
        max-width:1250px;
        margin:auto;
        padding:10px 25px 35px 25px;
      }

      .cabecera {
        background:linear-gradient(135deg,#244395,#28499e);
        color:white;
        border-radius:12px;
        padding:27px;
        margin-bottom:25px;
        text-align:center;
        box-shadow:0 3px 7px rgba(0,0,0,.10);
      }

      .cabecera h1 {
        font-size:31px;
        font-weight:800;
        margin:0;
      }

      .cabecera p {
        font-size:15px;
        margin-top:8px;
        margin-bottom:0;
      }

      .panel-box {
        background:white;
        border:1px solid #cbd5e1;
        border-radius:9px;
        padding:20px;
        margin-bottom:20px;
        box-shadow:0 2px 5px rgba(0,0,0,.04);
      }

      .panel-title {
        font-size:20px;
        font-weight:bold;
        color:#163a8a;
        padding-bottom:10px;
        border-bottom:2px solid #e2e8f0;
        margin-bottom:18px;
      }

      .summary-card {
        padding:18px;
        background-color:#f8fafc;
        border-radius:9px;
        border-left:5px solid #23449a;
        text-align:center;
        min-height:125px;
      }

      .summary-card h2 {
        color:#173b8f;
        font-weight:bold;
      }

      .ayuda {
        background-color:#eff6ff;
        border-left:5px solid #2563eb;
        padding:15px;
        border-radius:7px;
        color:#334155;
        margin-bottom:20px;
      }

      .advertencia {
        background-color:#fff7ed;
        border-left:5px solid #f59e0b;
        padding:15px;
        border-radius:7px;
        margin-bottom:20px;
      }

      .formato {
        background:#f8fafc;
        padding:12px;
        border-radius:6px;
        margin-top:8px;
        font-family:monospace;
      }

      .btn-primary {
        background-color:#2864e8;
        border-color:#2864e8;
        font-weight:bold;
      }

      ")

    )

  ),


  div(

    class = "contenedor",


    # ============================================================
    # CABECERA
    # ============================================================

    div(

      class = "cabecera",

      h1(
        "CUANTIFICACIÓN DE LA VALIDEZ DE CONTENIDO (V DE AIKEN)"
      ),

      p(
        paste(
          "Evaluación de ítems, homogeneidad, comparación",
          "de grupos de jueces e intervalos de confianza",
          "(Merino & Livia, 2009)"
        )
      )

    ),


    # ============================================================
    # 1. CONFIGURACIÓN
    # ============================================================

    div(

      class = "panel-box",

      div(
        class = "panel-title",
        "1. Configuración de Escala y Carga de Datos"
      ),


      fluidRow(

        column(

          3,

          numericInput(
            "minimo",
            "Mínimo de la escala (l):",
            value = 1,
            min = 0,
            step = 1
          )

        ),

        column(

          3,

          numericInput(
            "maximo",
            "Máximo de la escala (h):",
            value = 4,
            min = 1,
            step = 1
          )

        ),

        column(

          3,

          selectInput(

            "confianza",

            "Nivel de confianza:",

            choices = c(
              "90%" = 0.90,
              "95%" = 0.95,
              "99%" = 0.99
            ),

            selected = 0.95

          )

        ),

        column(

          3,

          numericInput(

            "corte",

            "Punto de corte (LI ≥):",

            value = 0.70,

            min = 0,

            max = 1,

            step = 0.05

          )

        )

      ),


      fluidRow(

        column(

          5,

          selectInput(

            "orientacion",

            "Orientación de la base:",

            choices = c(

              "Detectar automáticamente" =
                "auto",

              "Ítems en filas / jueces en columnas" =
                "items_filas",

              "Jueces en filas / ítems en columnas" =
                "jueces_filas"

            ),

            selected = "auto"

          )

        ),

        column(

          7,

          uiOutput(
            "orientacion_detectada"
          )

        )

      ),


      fileInput(

        "archivo",

        "Seleccione Excel o CSV:",

        accept = c(
          ".xlsx",
          ".xls",
          ".csv"
        ),

        width = "100%"

      ),


      fluidRow(

        column(

          6,

          actionButton(
            "demoA",
            "Ejemplo A: Ítems en filas",
            class = "btn-primary"
          )

        ),

        column(

          6,

          actionButton(
            "demoB",
            "Ejemplo B: Jueces en filas",
            class = "btn-primary"
          )

        )

      )

    ),


    # ============================================================
    # PESTAÑAS
    # ============================================================

    tabsetPanel(

      id = "tabs",


      # ==========================================================
      # 2. DATA
      # ==========================================================

      tabPanel(

        "2. Data de Jueces",

        br(),

        div(

          class = "ayuda",

          strong(
            "La aplicación admite dos formatos:"
          ),

          div(
            class = "formato",
            "Formato A: Item | Expert1 | Expert2 | Expert3 | ..."
          ),

          div(
            class = "formato",
            "Formato B: Juez | Grupo | Item1 | Item2 | Item3 | ..."
          )

        ),

        DTOutput(
          "tabla_datos"
        )

      ),


      # ==========================================================
      # 3. RESULTADOS
      # ==========================================================

      tabPanel(

        "3. Resultados por Ítem & Aiken Total",

        br(),

        fluidRow(

          column(

            3,

            div(
              class = "summary-card",
              h4("V de Aiken Total"),
              h2(textOutput("v_total"))
            )

          ),

          column(

            3,

            div(
              class = "summary-card",
              h4("Ítems aceptados"),
              h2(textOutput("n_aceptados"))
            )

          ),

          column(

            3,

            div(
              class = "summary-card",
              h4("Ítems a revisar"),
              h2(textOutput("n_revisar"))
            )

          ),

          column(

            3,

            div(
              class = "summary-card",
              h4("N.º de jueces"),
              h2(textOutput("n_jueces"))
            )

          )

        ),

        br(),

        DTOutput(
          "tabla_resultados"
        ),

        br(),

        downloadButton(
          "descargar_resultados",
          "Descargar resultados CSV"
        )

      ),


      # ==========================================================
      # 4. HOMOGENEIDAD
      # ==========================================================

      tabPanel(

        "4. Coeficiente Homogeneidad (H)",

        br(),

        div(

          class = "panel-box",

          div(
            class = "panel-title",
            "Homogeneidad entre Jueces"
          ),

          fluidRow(

            column(

              3,

              div(
                class = "summary-card",
                h4("Coeficiente H"),
                h2(textOutput("coef_h"))
              )

            ),

            column(

              9,

              DTOutput(
                "matriz_correlaciones"
              )

            )

          )

        )

      ),


      # ==========================================================
      # 5. GRÁFICO
      # ==========================================================

      tabPanel(

        "5. Gráfico de Validez",

        br(),

        div(

          class = "panel-box",

          div(
            class = "panel-title",
            "V de Aiken e Intervalos de Confianza"
          ),

          div(

            class = "ayuda",

            paste(
              "Cada punto representa la V de Aiken.",
              "La línea horizontal representa el intervalo",
              "de confianza y la línea vertical discontinua",
              "corresponde al criterio mínimo."
            )

          ),

          plotOutput(
            "grafico_v",
            height = "700px"
          )

        )

      ),


      # ==========================================================
      # 6. COMPARACIÓN
      # ==========================================================

      tabPanel(

        "6. Comparación de 2 Grupos",

        br(),

        div(

          class = "panel-box",

          div(
            class = "panel-title",
            "Comparación entre Grupos de Jueces"
          ),

          uiOutput(
            "interfaz_comparacion"
          ),

          br(),

          actionButton(
            "comparar",
            "Calcular comparación",
            class = "btn-primary"
          ),

          br(),
          br(),

          DTOutput(
            "tabla_comparacion"
          ),

          br(),

          plotOutput(
            "grafico_comparacion",
            height = "700px"
          )

        )

      )

    )

  )

)


# ================================================================
# SERVER
# ================================================================

server <- function(input,
                   output,
                   session) {


  # ==============================================================
  # DATOS REACTIVOS
  # ==============================================================

  datos_reactivos <- reactiveVal(
    NULL
  )


  # ==============================================================
  # CARGAR ARCHIVO
  # ==============================================================

  observeEvent(
    input$archivo,
    {

      req(
        input$archivo
      )

      ext <- tolower(
        tools::file_ext(
          input$archivo$name
        )
      )

      if (ext %in% c("xlsx", "xls")) {

        df <- read_excel(
          input$archivo$datapath
        )

      } else if (ext == "csv") {

        df <- read.csv(

          input$archivo$datapath,

          check.names = FALSE,

          stringsAsFactors = FALSE

        )

      } else {

        showNotification(
          "Formato no permitido.",
          type = "error"
        )

        return()

      }

      datos_reactivos(
        as.data.frame(df)
      )

    }
  )


  # ==============================================================
  # DEMOS
  # ==============================================================

  observeEvent(
    input$demoA,
    {

      datos_reactivos(
        demo_A
      )

    }
  )


  observeEvent(
    input$demoB,
    {

      datos_reactivos(
        demo_B
      )

    }
  )


  # ==============================================================
  # ORIENTACIÓN REAL
  # ==============================================================

  orientacion_actual <- reactive({

    req(
      datos_reactivos()
    )

    if (input$orientacion != "auto") {

      return(
        input$orientacion
      )

    }

    detectar_orientacion(
      datos_reactivos()
    )

  })


  # ==============================================================
  # MOSTRAR ORIENTACIÓN
  # ==============================================================

  output$orientacion_detectada <- renderUI({

    req(
      datos_reactivos()
    )

    ori <- orientacion_actual()

    texto <- switch(

      ori,

      "items_filas" =
        "Detectado: Ítems en filas / jueces en columnas",

      "jueces_filas" =
        "Detectado: Jueces en filas / ítems en columnas",

      "desconocido" =
        "No se pudo detectar automáticamente."

    )

    div(

      class = "ayuda",

      strong(
        texto
      )

    )

  })


  # ==============================================================
  # MOSTRAR DATOS
  # ==============================================================

  output$tabla_datos <- renderDT({

    req(
      datos_reactivos()
    )

    datatable(

      datos_reactivos(),

      rownames = FALSE,

      options = list(
        scrollX = TRUE,
        pageLength = 15
      )

    )

  })


  # ==============================================================
  # MATRIZ ESTÁNDAR
  # Siempre devuelve:
  # filas = ítems
  # columnas = jueces
  # ==============================================================

  matriz_estandar <- reactive({

    req(
      datos_reactivos()
    )

    df <- datos_reactivos()

    ori <- orientacion_actual()


    # ------------------------------------------------------------
    # FORMATO A
    # ------------------------------------------------------------

    if (ori == "items_filas") {

      columnas <- grep(

        "^Expert",

        names(df),

        value = TRUE,

        ignore.case = TRUE

      )

      validate(

        need(
          length(columnas) >= 2,
          "No se encontraron al menos dos columnas Expert."
        )

      )

      matriz <- df[
        columnas
      ]

      matriz[] <- lapply(

        matriz,

        function(x) {

          suppressWarnings(
            as.numeric(x)
          )

        }

      )

      if ("Item" %in% names(df)) {

        nombres <- as.character(
          df$Item
        )

      } else {

        nombres <- paste0(
          "Item ",
          seq_len(
            nrow(df)
          )
        )

      }

      return(

        list(

          matriz = matriz,

          items = nombres,

          jueces = columnas

        )

      )

    }


    # ------------------------------------------------------------
    # FORMATO B
    # ------------------------------------------------------------

    if (ori == "jueces_filas") {

      columnas_item <- grep(

        "^Item",

        names(df),

        value = TRUE,

        ignore.case = TRUE

      )

      validate(

        need(
          length(columnas_item) >= 2,
          "No se encontraron columnas Item1, Item2, ..."
        )

      )

      datos_items <- df[
        columnas_item
      ]

      datos_items[] <- lapply(

        datos_items,

        function(x) {

          suppressWarnings(
            as.numeric(x)
          )

        }

      )

      matriz <- as.data.frame(
        t(
          as.matrix(
            datos_items
          )
        )
      )

      if ("Juez" %in% names(df)) {

        names(matriz) <- as.character(
          df$Juez
        )

      } else {

        names(matriz) <- paste0(
          "Juez",
          seq_len(
            nrow(df)
          )
        )

      }

      return(

        list(

          matriz = matriz,

          items = columnas_item,

          jueces = names(matriz)

        )

      )

    }


    validate(

      need(
        FALSE,
        "Seleccione manualmente la orientación de la base."
      )

    )

  })


  # ==============================================================
  # RESULTADOS AIKEN
  # ==============================================================

  resultados_aiken <- reactive({

    M <- matriz_estandar()

    matriz <- M$matriz

    minimo <- input$minimo

    maximo <- input$maximo


    validate(

      need(
        maximo > minimo,
        "El máximo debe ser mayor que el mínimo."
      )

    )


    valores <- unlist(
      matriz
    )

    valores <- valores[
      !is.na(valores)
    ]


    validate(

      need(

        length(valores) > 0,

        "No se encontraron valoraciones numéricas válidas."

      ),

      need(

        all(
          valores >= minimo &
          valores <= maximo
        ),

        paste0(
          "Existen puntuaciones fuera del rango ",
          minimo,
          "–",
          maximo,
          "."
        )

      )

    )


    k <- maximo - minimo


    V <- calcular_aiken(

      matriz,

      minimo,

      maximo

    )


    n <- apply(

      matriz,

      1,

      function(x) {

        sum(
          !is.na(x)
        )

      }

    )


    validate(

      need(

        all(n >= 2),

        "Cada ítem debe tener al menos dos jueces válidos."

      )

    )


    IC <- aiken_score_ci(

      V = V,

      n = n,

      k = k,

      conf.level =
        as.numeric(
          input$confianza
        )

    )


    media <- rowMeans(
      matriz,
      na.rm = TRUE
    )


    decision <- ifelse(

      IC$LI >= input$corte,

      "Aceptado",

      "Revisar"

    )


    data.frame(

      Item =
        M$items,

      N_Jueces =
        n,

      Media =
        round(
          media,
          3
        ),

      V_Aiken =
        round(
          V,
          3
        ),

      LI =
        round(
          IC$LI,
          3
        ),

      LS =
        round(
          IC$LS,
          3
        ),

      Decision =
        decision,

      stringsAsFactors = FALSE

    )

  })


  # ==============================================================
  # RESULTADOS TABLA
  # ==============================================================

  output$tabla_resultados <- renderDT({

    req(
      resultados_aiken()
    )

    datatable(

      resultados_aiken(),

      rownames = FALSE,

      options = list(
        scrollX = TRUE,
        pageLength = 20
      )

    )

  })


  # ==============================================================
  # RESUMEN
  # ==============================================================

  output$v_total <- renderText({

    req(
      resultados_aiken()
    )

    sprintf(
      "%.3f",
      mean(
        resultados_aiken()$V_Aiken,
        na.rm = TRUE
      )
    )

  })


  output$n_aceptados <- renderText({

    req(
      resultados_aiken()
    )

    sum(
      resultados_aiken()$Decision == "Aceptado",
      na.rm = TRUE
    )

  })


  output$n_revisar <- renderText({

    req(
      resultados_aiken()
    )

    sum(
      resultados_aiken()$Decision == "Revisar",
      na.rm = TRUE
    )

  })


  output$n_jueces <- renderText({

    req(
      matriz_estandar()
    )

    length(
      matriz_estandar()$jueces
    )

  })


  # ==============================================================
  # HOMOGENEIDAD
  # ==============================================================

  output$coef_h <- renderText({

    req(
      matriz_estandar()
    )

    matriz <- matriz_estandar()$matriz

    h <- calcular_h(
      matriz
    )

    if (is.na(h)) {

      return("NA")

    }

    sprintf(
      "%.3f",
      h
    )

  })


  output$matriz_correlaciones <- renderDT({

    req(
      matriz_estandar()
    )

    matriz <- matriz_estandar()$matriz

    C <- round(

      cor(
        matriz,
        use = "pairwise.complete.obs"
      ),

      3

    )

    datatable(

      C,

      options = list(
        dom = "t",
        scrollX = TRUE
      )

    )

  })


  # ==============================================================
  # 5. GRÁFICO V + IC
  # ==============================================================

  output$grafico_v <- renderPlot({

    req(
      resultados_aiken()
    )

    r <- resultados_aiken()

    r$Item <- factor(

      r$Item,

      levels =
        rev(
          r$Item
        )

    )

    ggplot(

      r,

      aes(
        x = V_Aiken,
        y = Item
      )

    ) +

      geom_errorbarh(

        aes(
          xmin = LI,
          xmax = LS
        ),

        height = 0.18,

        linewidth = 0.9

      ) +

      geom_point(
        size = 4
      ) +

      geom_vline(

        xintercept =
          input$corte,

        linetype =
          "dashed",

        linewidth =
          0.8

      ) +

      geom_text(

        aes(
          label =
            sprintf(
              "%.2f",
              V_Aiken
            )
        ),

        nudge_y = 0.28,

        size = 3.5

      ) +

      scale_x_continuous(

        limits = c(0,1),

        breaks =
          seq(
            0,
            1,
            by = 0.10
          )

      ) +

      labs(

        title =
          "V de Aiken por Ítem",

        subtitle =
          paste0(
            "IC ",
            as.numeric(
              input$confianza
            ) * 100,
            "% | Criterio LI ≥ ",
            input$corte
          ),

        x =
          "V de Aiken",

        y =
          NULL,

        caption =
          paste(
            "Punto = V de Aiken |",
            "línea horizontal = IC |",
            "línea discontinua = criterio"
          )

      ) +

      theme_minimal(
        base_size = 13
      ) +

      theme(

        plot.title =
          element_text(
            face = "bold",
            size = 18
          ),

        axis.text.y =
          element_text(
            size = 11
          ),

        panel.grid.minor =
          element_blank()

      )

  })


  # ==============================================================
  # 6. INTERFAZ COMPARACIÓN
  # ==============================================================

  output$interfaz_comparacion <- renderUI({

    req(
      datos_reactivos()
    )

    ori <- orientacion_actual()

    df <- datos_reactivos()


    # ------------------------------------------------------------
    # FORMATO A
    # ------------------------------------------------------------

    if (ori == "items_filas") {

      expertos <- grep(

        "^Expert",

        names(df),

        value = TRUE,

        ignore.case = TRUE

      )


      return(

        tagList(

          div(

            class = "advertencia",

            strong(
              "Base con ítems en filas:"
            ),

            p(
              paste(
                "Seleccione qué columnas de jueces",
                "pertenecen al Grupo 1 y cuáles",
                "pertenecen al Grupo 2."
              )
            )

          ),


          fluidRow(

            column(

              6,

              textInput(
                "nombre_grupo1_A",
                "Nombre del Grupo 1:",
                value = "Grupo 1"
              ),

              checkboxGroupInput(

                "expertos_grupo1",

                "Jueces del Grupo 1:",

                choices =
                  expertos

              )

            ),


            column(

              6,

              textInput(
                "nombre_grupo2_A",
                "Nombre del Grupo 2:",
                value = "Grupo 2"
              ),

              checkboxGroupInput(

                "expertos_grupo2",

                "Jueces del Grupo 2:",

                choices =
                  expertos

              )

            )

          )

        )

      )

    }


    # ------------------------------------------------------------
    # FORMATO B
    # ------------------------------------------------------------

    if (ori == "jueces_filas") {

      columnas_item <- grep(

        "^Item",

        names(df),

        value = TRUE,

        ignore.case = TRUE

      )

      candidatas <- setdiff(

        names(df),

        c(
          columnas_item,
          "Juez"
        )

      )

      validate(

        need(
          length(candidatas) >= 1,
          "No se encontró una columna disponible para definir grupos."
        )

      )


      return(

        tagList(

          div(

            class = "ayuda",

            strong(
              "Base con jueces en filas:"
            ),

            p(
              paste(
                "Seleccione la columna que identifica",
                "el grupo al que pertenece cada juez."
              )
            )

          ),


          selectInput(

            "variable_grupo_B",

            "Columna que define el grupo:",

            choices =
              candidatas

          ),


          uiOutput(
            "niveles_grupo_B"
          )

        )

      )

    }


    div(
      class = "advertencia",
      "No se pudo determinar la orientación de la base."
    )

  })


  # ==============================================================
  # NIVELES DE GRUPOS - FORMATO B
  # ==============================================================

  output$niveles_grupo_B <- renderUI({

    req(
      datos_reactivos()
    )

    req(
      input$variable_grupo_B
    )

    df <- datos_reactivos()


    validate(

      need(

        input$variable_grupo_B %in% names(df),

        "Seleccione una columna válida para definir los grupos."

      )

    )


    niveles <- unique(

      as.character(
        df[[input$variable_grupo_B]]
      )

    )


    niveles <- niveles[
      !is.na(niveles) &
      trimws(niveles) != ""
    ]


    validate(

      need(

        length(niveles) >= 2,

        "La variable seleccionada debe contener al menos dos grupos."

      )

    )


    fluidRow(

      column(

        6,

        selectInput(

          "grupo1_B",

          "Grupo 1:",

          choices =
            niveles,

          selected =
            niveles[1]

        )

      ),

      column(

        6,

        selectInput(

          "grupo2_B",

          "Grupo 2:",

          choices =
            niveles,

          selected =
            niveles[2]

        )

      )

    )

  })


  # ==============================================================
  # FUNCIÓN AUXILIAR PARA GRUPOS
  # ==============================================================

  calcular_resultado_grupo <- function(matriz,
                                       items) {

    k <- input$maximo - input$minimo


    V <- calcular_aiken(

      matriz,

      input$minimo,

      input$maximo

    )


    n <- apply(

      matriz,

      1,

      function(x) {

        sum(
          !is.na(x)
        )

      }

    )


    IC <- aiken_score_ci(

      V = V,

      n = n,

      k = k,

      conf.level =
        as.numeric(
          input$confianza
        )

    )


    data.frame(

      Item = items,

      V = V,

      LI = IC$LI,

      LS = IC$LS,

      stringsAsFactors = FALSE

    )

  }


  # ==============================================================
  # COMPARACIÓN DE GRUPOS
  # ==============================================================

  comparacion_resultados <- eventReactive(

    input$comparar,

    {

      req(
        datos_reactivos()
      )

      df <- datos_reactivos()

      ori <- orientacion_actual()


      # ==========================================================
      # FORMATO A
      # ==========================================================

      if (ori == "items_filas") {

        req(
          input$expertos_grupo1,
          input$expertos_grupo2
        )


        validate(

          need(

            length(
              input$expertos_grupo1
            ) >= 2,

            "Seleccione al menos 2 jueces para el Grupo 1."

          ),

          need(

            length(
              input$expertos_grupo2
            ) >= 2,

            "Seleccione al menos 2 jueces para el Grupo 2."

          ),

          need(

            length(
              intersect(
                input$expertos_grupo1,
                input$expertos_grupo2
              )
            ) == 0,

            paste(
              "Un mismo juez no puede pertenecer",
              "simultáneamente a ambos grupos."
            )

          )

        )


        M1 <- df[
          input$expertos_grupo1
        ]

        M2 <- df[
          input$expertos_grupo2
        ]


        M1[] <- lapply(
          M1,
          as.numeric
        )

        M2[] <- lapply(
          M2,
          as.numeric
        )


        if ("Item" %in% names(df)) {

          items <- as.character(
            df$Item
          )

        } else {

          items <- paste0(
            "Item ",
            seq_len(
              nrow(df)
            )
          )

        }


        r1 <- calcular_resultado_grupo(
          M1,
          items
        )

        r2 <- calcular_resultado_grupo(
          M2,
          items
        )


        nombre1 <- input$nombre_grupo1_A

        nombre2 <- input$nombre_grupo2_A

      }


      # ==========================================================
      # FORMATO B
      # ==========================================================

      else if (ori == "jueces_filas") {


        req(
          input$variable_grupo_B,
          input$grupo1_B,
          input$grupo2_B
        )


        validate(

          need(

            input$variable_grupo_B %in% names(df),

            "La columna seleccionada para grupos no existe."

          ),

          need(

            input$grupo1_B != input$grupo2_B,

            "Seleccione dos grupos diferentes."

          )

        )


        columnas_item <- grep(

          "^Item",

          names(df),

          value = TRUE,

          ignore.case = TRUE

        )


        grupo_vector <- as.character(
          df[[input$variable_grupo_B]]
        )


        df1 <- df[
          grupo_vector == input$grupo1_B,
          ,
          drop = FALSE
        ]


        df2 <- df[
          grupo_vector == input$grupo2_B,
          ,
          drop = FALSE
        ]


        validate(

          need(
            nrow(df1) >= 2,
            "El Grupo 1 debe contener al menos dos jueces."
          ),

          need(
            nrow(df2) >= 2,
            "El Grupo 2 debe contener al menos dos jueces."
          )

        )


        M1 <- as.data.frame(

          t(
            as.matrix(
              df1[
                columnas_item
              ]
            )
          )

        )


        M2 <- as.data.frame(

          t(
            as.matrix(
              df2[
                columnas_item
              ]
            )
          )

        )


        M1[] <- lapply(
          M1,
          function(x) suppressWarnings(as.numeric(x))
        )

        M2[] <- lapply(
          M2,
          function(x) suppressWarnings(as.numeric(x))
        )


        r1 <- calcular_resultado_grupo(
          M1,
          columnas_item
        )

        r2 <- calcular_resultado_grupo(
          M2,
          columnas_item
        )


        nombre1 <- input$grupo1_B

        nombre2 <- input$grupo2_B

      }


      else {

        validate(

          need(
            FALSE,
            "No se pudo determinar la orientación de la base."
          )

        )

      }


      # ==========================================================
      # TABLA FINAL
      # ==========================================================

      data.frame(

        Item =
          r1$Item,

        Grupo1 =
          nombre1,

        V_Grupo1 =
          round(
            r1$V,
            3
          ),

        LI_Grupo1 =
          round(
            r1$LI,
            3
          ),

        LS_Grupo1 =
          round(
            r1$LS,
            3
          ),

        Grupo2 =
          nombre2,

        V_Grupo2 =
          round(
            r2$V,
            3
          ),

        LI_Grupo2 =
          round(
            r2$LI,
            3
          ),

        LS_Grupo2 =
          round(
            r2$LS,
            3
          ),

        Diferencia_V =
          round(
            r1$V -
            r2$V,
            3
          ),

        stringsAsFactors = FALSE

      )

    }

  )


  # ==============================================================
  # TABLA COMPARACIÓN
  # ==============================================================

  output$tabla_comparacion <- renderDT({

    req(
      comparacion_resultados()
    )

    datatable(

      comparacion_resultados(),

      rownames = FALSE,

      options = list(
        scrollX = TRUE,
        pageLength = 20
      )

    )

  })


  # ==============================================================
  # GRÁFICO COMPARACIÓN
  # ==============================================================

  output$grafico_comparacion <- renderPlot({

    req(
      comparacion_resultados()
    )

    r <- comparacion_resultados()


    datos_grafico <- bind_rows(

      data.frame(

        Item =
          r$Item,

        Grupo =
          r$Grupo1,

        V =
          r$V_Grupo1,

        LI =
          r$LI_Grupo1,

        LS =
          r$LS_Grupo1

      ),

      data.frame(

        Item =
          r$Item,

        Grupo =
          r$Grupo2,

        V =
          r$V_Grupo2,

        LI =
          r$LI_Grupo2,

        LS =
          r$LS_Grupo2

      )

    )


    datos_grafico$Item <- factor(

      datos_grafico$Item,

      levels =
        rev(
          unique(
            datos_grafico$Item
          )
        )

    )


    ggplot(

      datos_grafico,

      aes(
        x = V,
        y = Item,
        shape = Grupo
      )

    ) +

      geom_errorbarh(

        aes(
          xmin = LI,
          xmax = LS,
          group = Grupo
        ),

        position =
          position_dodge(
            width = 0.45
          ),

        height = 0.15,

        linewidth = 0.8

      ) +

      geom_point(

        position =
          position_dodge(
            width = 0.45
          ),

        size = 3.8

      ) +

      geom_vline(

        xintercept =
          input$corte,

        linetype =
          "dashed",

        linewidth =
          0.8

      ) +

      scale_x_continuous(

        limits =
          c(0,1),

        breaks =
          seq(
            0,
            1,
            0.1
          )

      ) +

      labs(

        title =
          "Comparación de V de Aiken entre Grupos",

        subtitle =
          paste0(
            unique(r$Grupo1)[1],
            " vs. ",
            unique(r$Grupo2)[1]
          ),

        x =
          "V de Aiken",

        y =
          NULL,

        shape =
          "Grupo"

      ) +

      theme_minimal(
        base_size = 13
      ) +

      theme(

        plot.title =
          element_text(
            face = "bold",
            size = 18
          ),

        panel.grid.minor =
          element_blank()

      )

  })


  # ==============================================================
  # DESCARGA
  # ==============================================================

  output$descargar_resultados <- downloadHandler(

    filename = function() {

      paste0(
        "ValCont_Aiken_",
        Sys.Date(),
        ".csv"
      )

    },

    content = function(file) {

      write.csv(

        resultados_aiken(),

        file,

        row.names = FALSE,

        fileEncoding = "UTF-8"

      )

    }

  )

}


# ================================================================
# EJECUTAR
# ================================================================

shinyApp(
  ui = ui,
  server = server
)
