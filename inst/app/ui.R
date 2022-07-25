library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinyMatrix)
library(dplyr)
library(shinycssloaders)
library(ggplot2)
library(shinyjs)
library(mpiBpred)
library(coda)

tagList(
  shinyjs::useShinyjs(),
  shiny::navbarPage(
    title = paste("mpiBpred App", packageVersion("mpiBpred")),
    theme = shinythemes::shinytheme("flatly"),
    id = "tab",
    # DATA ---------------------------------------------------------------------------------------
    tabPanel(
      title = "Data",
      id = "Data",
      value = "Data",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          h3("Upload Data"),
          selectInput(
            "filetypeData",
            "File Type",
            choices = c("xlsx", "csv"),
            selected = "xlsx"
          ),
          conditionalPanel(
            condition = "input.filetypeData == 'csv'",
            div(
              style = "display: inline-block;horizontal-align:top; width: 80px;",
              textInput("colseparatorData", "column separator:", value = ",")
            ),
            div(
              style = "display: inline-block;horizontal-align:top; width: 80px;",
              textInput("decseparatorData", "decimal separator:", value = ".")
            )
          ),
          helpText("The first row in your file needs to contain variable names."),
          h4("Example Data"),
          fileInput("DataFile", ""),
          HTML("<hr>"),
          actionButton("simulateData", "Simulate Example Data"),
          numericInput(
            "n",
            label = "Observations:",
            value = 100,
            min = 1
          )
          
        ),
        mainPanel(dataTableOutput('data')),
      )
    ),
    # FORMULAS -----------------------------------------------------------------------------------
    tabPanel(
      title = "Formulas",
      id = "Formulas",
      value = "Formulas",
      
      sidebarLayout(
        sidebarPanel(
          width = 2,
          h3("Define Formulas"),
          textInput("formName", "Name:", "formula_1"),
          pickerInput(
            inputId = "f_y",
            label = "y variable:",
            choices = character(0),
            options = list(
              "actions-box" = FALSE,
              "none-selected-text" = 'No variables selected',
              "max-options" = 1
            ),
            multiple = FALSE
          ),
          selectInput(
            inputId = "f_yunc",
            label = "y-uncertainty (sd) - optional:",
            choices = character(0),
            multiple = FALSE
          ),
          radioButtons(
            inputId = "selectFType",
            label = "Select formula type:",
            choices = c(
              "Univariate linear regression" = "linear",
              "Custom formula" = "custom"
            )
          ),
          conditionalPanel(
            condition = "input.selectFType == 'linear'",
            pickerInput(
              inputId = "f_x",
              label = "x variable:",
              choices = character(0),
              options = list(
                "actions-box" = FALSE,
                "none-selected-text" = 'No variables selected',
                "max-options" = 1
              ),
              multiple = FALSE
            ),
            selectInput(
              inputId = "f_xunc",
              label = "x-uncertainty (sd) - optional:",
              choices = character(0),
              multiple = FALSE
            ),
            selectInput(
              "f_link",
              "relationship (link)",
              choices = list(
                'linear' =  "linIntcp",
                'linear without Intercept' =  "linNoIntcp",
                'log-linear' = "log",
                'sqrt-linear' = "sqrt"
              )
            )
          ),
          conditionalPanel(
            condition = "input.selectFType == 'custom'",
            textInput(
              "formCustom",
              "Custom formula (variables in square brackets and parameters in curly braces):",
              "{a} * [x] + {b}"
            ),
            selectizeInput(
              "custom_x",
              label = "x variables (same names as in formula)",
              choices = character(0),
              multiple = TRUE
            ),
            selectizeInput(
              "custom_x_unc",
              label = "x unc variables (optional, same order as x variables)",
              choices = character(0),
              multiple = TRUE
            ),
            checkboxInput("dirichlet", "Restrict (some) parameters to sum up to 1 (and larger than 0)"),
            conditionalPanel(
              condition = "input.dirichlet == true",
            textInput(
              "parRestricted",
              "To be restricted parameters names seperated by comma:",
              "a,b"
            )
            )
          ),
          # pickerInput(
          #   inputId = "f_yunc",
          #   label = "y-uncertainty (sd) - optional:",
          #   choices = character(0),
          #   options = list(
          #     "actions-box" = FALSE,
          #     "none-selected-text" = 'No variables selected',
          #     "max-options" = 1
          #   ),
          #   multiple = FALSE
          # ),
          # pickerInput(
          #   inputId = "f_xunc",
          #   label = "x-uncertainty (sd) - optional:",
          #   choices = character(0),
          #   options = list(
          #     "actions-box" = FALSE,
          #     "none-selected-text" = 'No variables selected',
          #     "max-options" = 1
          #   ),
          #   multiple = FALSE
          # ),
          sliderInput(
            inputId = "iter",
            label = "Number of total iterations",
            min = 500,
            max = 10000,
            step = 100,
            value = 2000
          ),
          sliderInput(
            inputId = "burnin",
            label = "Number of burnin iterations",
            min = 200,
            max = 3000,
            step = 100,
            value = 500
          ),
          sliderInput(
            inputId = "chains",
            label = "Number of MCMC chains",
            min = 1,
            max = 16,
            step = 1,
            value = 4
          ),
          conditionalPanel(
            condition = "input.selectFType == 'custom'",
            sliderInput(
              inputId = "thinning",
              label = "Thinning of MCMC chains",
              min = 1,
              max = 20,
              step = 1,
              value = 5
            )
          ),
          actionButton("saveFormula", "Define Formula")
        ),
        mainPanel(tabsetPanel(
          id = "formTabs",
          tabPanel(
            "Defined Formulas",
            tableOutput('formTable') %>%
              withSpinner(color = "#20c997")
          ),
          tabPanel(
            "Convergence diagnostics",
            pickerInput("convF", "Choose formula", choices = character(0)),
            radioButtons(("diagType"),
                         label = "Diagnostics Type",
                         choices = c(
                           "Gelman Scale Reduction Factor" = "gelman",
                           "Raftery and Lewis" = "raftery",
                           "Geweke z-Score" = "geweke",
                           "Heidelberger-Welch" = "heidel"
                         )
            ),
            verbatimTextOutput(("diagnostics")),
            textExportButton(("exportText"))
          ),
          tabPanel(
            "Display formulas",
            pickerInput("dispF", "Choose formula", choices = character(0)),
            plotOutput("plotDisp"),
            selectInput("xVarDisp", "Choose x variable", choices = character(0)),
            textAreaInput(
              inputId = ("headerLabelF"),
              label = "Header",
              value = ""
            ),
            textAreaInput(
              inputId = ("xlabelF"),
              label = "Title x-axis",
              value = ""
            ),
            textAreaInput(
              inputId = ("ylabelF"),
              label = "Title y-axis",
              value = ""
            ),
            numericInput(
              inputId = ("xTextSizeF"),
              label = "Font size x-axis title",
              value = 24
            ),
            numericInput(
              inputId = ("yTextSizeF"),
              label = "Font size y-axis title",
              value = 24
            ),
            numericInput(
              inputId = ("xAxisSizeF"),
              label = "Font size x-axis",
              value = 18
            ),
            numericInput(
              inputId = ("yAxisSizeF"),
              label = "Font size y-axis",
              value = 18
            ),
            sliderInput(
              inputId = "PointSizeF",
              label = "Point size",
              min = 0.1, max = 5,value =  1, step = 0.1
            ),
            sliderInput(
              inputId = "LineWidthF",
              label = "Line Width",
              min = 0.1, max = 5,value =  1, step = 0.1
            ),
            
            actionButton("exportPlotF", "Export Plot"),
            actionButton("exportDataF", "Export Data")
          )
        ))
      )
    ),
    # MEASURES -----------------------------------------------------------------------------------
    tabPanel(
      title = "Measures",
      id = "Measures",
      value = "Measures",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          radioButtons(
            "measuresSource",
            label = NULL,
            choices = c("Enter Data", "Upload Data")
          ),
          HTML("<hr>"),
          conditionalPanel(
            condition = ("input.measuresSource == 'Enter Data'"),
            actionButton("simulateMeasures", "Load Example Data")
          ),
          conditionalPanel(
            condition = ("input.measuresSource == 'Upload Data'"),
            h3("Upload Data"),
            selectInput(
              "filetypeMeasures",
              "File Type",
              choices = c("xlsx", "csv"),
              selected = "xlsx"
            ),
            conditionalPanel(
              condition = "input.filetypeMeasures == 'csv'",
              div(
                style = "display: inline-block;horizontal-align:top; width: 80px;",
                textInput("colseparatorMeasures", "column separator:", value = ",")
              ),
              div(
                style = "display: inline-block;horizontal-align:top; width: 80px;",
                textInput("decseparatorMeasures", "decimal separator:", value = ".")
              )
            ),
            helpText("The first row in your file needs to contain variable names."),
            
            fileInput("MeasuresFile", "")
          )
        ),
        mainPanel(
          matrixInput(
            inputId = "measuresMatrix",
            class = "character",
            value = matrix(),
            copy = TRUE,
            paste = TRUE,
            cols = list(
              names = TRUE,
              extend = TRUE,
              delta = 1,
              editableNames = TRUE
            ),
            rows = list(
              names = FALSE,
              editableNames = TRUE,
              extend = TRUE,
              delta = 1
            )
          )
        ),
      )
    ),
    # ESTIMATES -----------------------------------------------------------------------------------
    tabPanel(
      title = "Estimates",
      id = "Estimates",
      value = "Estimates",
      fluidRow(
        sidebarPanel(
          width = 2,
          textInput("relationship", "relationship:", value = "Y ~ 3 + 4.5 * formula_1([X1])"),
          pickerInput(
            inputId = "regfunctions",
            label = "regfunctions:",
            choices = character(0),
            options = list("actions-box" = FALSE,
                           "none-selected-text" = 'No variables selected'),
            multiple = TRUE
          ),
          selectizeInput(
            inputId = "indVars",
            label = "indVars:",
            choices = character(0),
            multiple = TRUE
          ),
          selectizeInput(
            inputId = "indVarsUnc",
            label = "indVarsUnc:",
            choices = character(0),
            multiple = TRUE
          ),
          pickerInput(
            inputId = "category",
            label = "category:",
            choices = c("Category"),
            options = list("actions-box" = FALSE,
                           "none-selected-text" = 'No variables selected'),
            multiple = TRUE
          ),
          selectInput(
            "yDist",
            "Distribution of dependent variables:",
            choices = c("normal", "lognormal", "gamma"),
            selected = "normal"
          ),
          checkboxInput("rangeRestrict",
                        "Restrict range of dependent variable:",
                        value = FALSE),
          conditionalPanel(
            condition = "input.rangeRestrict == true",
            numericInput("minRange", "Min restriction value (optional) ", value = NULL),
            numericInput("maxRange", "Max restriction value (optional) ", value = NULL)
          ),
          checkboxInput(
            "includeRegUnc",
            "Include uncertainty of regression parameters:",
            value = TRUE
          ),
          HTML("<br>"),
          actionButton("estimateY", "Estimate Y")#,
          # tags$hr(),
          # downloadModelUI("modelDownload", "Download Model"),
          # uploadModelUI("modelUpload", "Upload Model")
        ),
        mainPanel(
          width = 8,
          selectInput(
            "summaryType",
            "Type",
            choices = c("Sample", "Combined", "Category"),
            selected = "Sample"
          ),
          tabsetPanel(
            id = "summaryTabs",
            tabPanel(
              "Plot",
              value = "plotTab",
              conditionalPanel(
                condition = "input.summaryType == 'Category'",
                radioButtons(
                  "meanType",
                  label = "Type of category distribution",
                  choices = c(
                    "Mean distribution" = "1",
                    "Total variance distribution" = "2"
                  )
                )
              ),
              selectInput(
                "summaryPlotType",
                "Plot Type",
                choices = c("KernelDensity", "Histogram", "Boxplot"),
                selected = "KernelDensity"
              ),
              conditionalPanel(
                condition = "input.summaryPlotType == 'Histogram'",
                sliderInput(
                  "nBins",
                  label = "Number of histogram bins",
                  min = 3,
                  max = 500,
                  value = 50,
                  step = 1
                )
              ),
              
              plotOutput("plot") %>% withSpinner(color =
                                                   "#20c997"),
              textAreaInput(
                inputId = ("headerLabel"),
                label = "Header",
                value = ""
              ),
              textAreaInput(
                inputId = ("xlabel"),
                label = "Title x-axis",
                value = ""
              ),
              textAreaInput(
                inputId = ("ylabel"),
                label = "Title y-axis",
                value = ""
              ),
              numericInput(
                inputId = ("xTextSize"),
                label = "Font size x-axis title",
                value = 24
              ),
              numericInput(
                inputId = ("yTextSize"),
                label = "Font size y-axis title",
                value = 24
              ),
              numericInput(
                inputId = ("xAxisSize"),
                label = "Font size x-axis",
                value = 18
              ),
              numericInput(
                inputId = ("yAxisSize"),
                label = "Font size y-axis",
                value = 18
              ),
              checkboxInput(
                inputId = ("showLegend"),
                label = "Show legend",
                value = TRUE
              ),
              # selectInput(
              #   inputId = ("colorPalette"),
              #   label = "Color Palette",
              #   choices = c(
              #     "default",
              #     "white",
              #     RColorBrewer::brewer.pal.info %>% row.names()
              #   )
              # ),
              # selectInput(
              #   inputId = ("fontFamily"),
              #   label = "Font",
              #   selected = NULL,
              #   choices = availableFonts()
              # ),
              conditionalPanel(
                condition = "input.summaryPlotType == 'Boxplot'",
              sliderInput(
                inputId = ("boxQuantile"),
                label = "Box upper quantile",
                value = 0.68,
                min = 0.5,
                max = 0.99,
                step = 0.01
              ),
              sliderInput(
                inputId = ("whiskerMultiplier"),
                label = "Whiskers coverage interval",
                value = 0.95,
                min = 0.5,
                max = 1,
                step = 0.001
              )),
              actionButton("exportPlot", "Export Plot"),
              actionButton("exportData", "Export Data")
            ),
            tabPanel(
              "Summary Statistics",
              value = "summaryTab",
              numericInput(
                "summaryProb",
                label = "probability:",
                value = 0.95,
                min = 0,
                max = 1,
                step = 0.05
              ),
              selectInput(
                "summaryRefType",
                "Reference Type:",
                choices = c("none", "dist", "sample", "freqTable")
              ),
              conditionalPanel(
                condition = "input.summaryRefType == 'dist'",
                selectInput(
                  "summaryRefDist",
                  "Reference Distribution:",
                  choices = c(
                    "Normal" = "norm",
                    "Gamma" = "gamma",
                    "Log-Normal" = "lnorm"
                  )
                ),
                textInput("summaryRefParams", "Reference Parameters:", value = "50, 3")
              ),
              
              conditionalPanel(
                condition = "input.summaryRefType == 'sample'",
                radioButtons(
                  "refSampleSource",
                  label = NULL,
                  choices = c("Enter Data", "Upload Data")
                ),
                conditionalPanel(
                  condition = "input.refSampleSource == 'Enter Data'",
                  textInput("summaryRefSample", "Reference Sample:", value = "60, 52, 75, 48, 50, 56")
                ),
                conditionalPanel(
                  condition = "input.refSampleSource == 'Upload Data'",
                  selectInput(
                    "filetypeRefSample",
                    "File Type",
                    choices = c("xlsx", "csv"),
                    selected = "xlsx"
                  ),
                  conditionalPanel(
                    condition = "input.filetypeRefSample == 'csv'",
                    div(
                      style = "display: inline-block;horizontal-align:top; width: 80px;",
                      textInput("colseparatorRefSample", "column separator:", value = ",")
                    ),
                    div(
                      style = "display: inline-block;horizontal-align:top; width: 80px;",
                      textInput("decseparatorRefSample", "decimal separator:", value = ".")
                    )
                  ),
                  fileInput("DataRefSample", "")
                )
              ),
              conditionalPanel(
                condition = "input.summaryRefType == 'freqTable'",
                radioButtons(
                  "refFreqTable",
                  label = NULL,
                  choices = c("Enter Data", "Upload Data")
                ),
                conditionalPanel(
                  condition = "input.refFreqTable == 'Enter Data'",
                  textInput("summaryFreqTable", "Reference Values:", value = "60, 52, 75, 48, 50, 56"),
                  textInput("summaryFreqTable2", "Reference Frequencies:", value = "1, 3, 6, 5, 4, 2")
                ),
                conditionalPanel(
                  condition = "input.refFreqTable == 'Upload Data'",
                  selectInput(
                    "filetypeRefSample",
                    "File Type",
                    choices = c("xlsx", "csv"),
                    selected = "xlsx"
                  ),
                  conditionalPanel(
                    condition = "input.filetypeRefSample == 'csv'",
                    div(
                      style = "display: inline-block;horizontal-align:top; width: 80px;",
                      textInput("colseparatorRefSample", "column separator:", value = ",")
                    ),
                    div(
                      style = "display: inline-block;horizontal-align:top; width: 80px;",
                      textInput("decseparatorRefSample", "decimal separator:", value = ".")
                    )
                  ),
                  fileInput("DataRefFreqTable", "")
                )
              ),
              actionButton("estimateSummary", "Compute Summary Statistics"),
              verbatimTextOutput("summaryEstimates") %>% withSpinner(color =
                                                                       "#20c997"),
              actionButton("exportSummary", "Export Mean Tables")
              
            )
          )),
        sidebarPanel(
          width = 2,
          downloadModelUI("modelDownload", "Download Model"),
          uploadModelUI("modelUpload", "Upload Model")
        )
      ),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      )
    )
  ),
  div(
    id = "header-right",
    div(
      id = "logo-mpi",
      tags$a(
        href = "https://www.mpg.de/en",
        img(src = "MPIlogo.png", alt = "Supported by the Max Planck society"),
        target = "_blank"
      )
    ),
    div(
      id = "logo-isomemo",
      tags$a(
        href = "https://isomemo.com/",
        img(src = "IsoMemoLogo.png", alt = "IsoMemo"),
        target = "_blank"
      )
    ),
    div(
      id = "further-help",
      tags$button(onclick = "window.open('https://isomemo.com','_blank');",
                  class = "btn btn-default",
                  "Further Help")
    ),
    div(id = "help",
        actionButton("getHelp", "?"))
  )
)
