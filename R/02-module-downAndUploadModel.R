#' Download model module
#'
#' UI function to download a zip file with notes and a list of models
#'
#' @param id id of module
#' @param label label of module
#'
#' @rdname downloadModel
#'
#' @export
downloadModelUI <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    tags$h5(label),
    textAreaInput(ns("notes"), "Notes"),
    HTML("<br>"),
    downloadButton(ns("downloadModel"), "Download"),
    tags$br()
  )
}


#' Server function download model
#'
#' Backend for download model module
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param yEstimates (reactive) An object created by \code{\link{estimateY}}.
#'  Distributions of the dependent variable.
#' @param formulas (reactive) formulas
#' @param data (reactive) data
#' @param uploadedNotes (reactive) variable that stores content of README.txt
#'
#' @export
downloadModel <-
  function(input,
           output,
           session,
           yEstimates,
           formulas,
           data,
           uploadedNotes) {
    observe({
      updateTextAreaInput(session, "notes", value = uploadedNotes())
    })
    
    output$downloadModel <- downloadHandler(
      filename = function() {
        paste(gsub("\ ", "_", Sys.time()), "bpred.zip", sep = "_")
      },
      content = function(file) {
        zipdir <- tempdir()
        modelfile <- file.path(zipdir, "model.Rdata")
        notesfile <- file.path(zipdir, "README.txt")
        helpfile <- file.path(zipdir, "help.html")
        
        model <- yEstimates()
        formulasObj <- reactiveValuesToList(formulas)
        dataObj <- reactiveValuesToList(data)
        inputObj <- reactiveValuesToList(input)
        save(model, formulasObj, dataObj, inputObj, file = modelfile)
        writeLines(input$notes, notesfile)
        save_html(getHelp(input$tab), helpfile)
        zip::zipr(file, c(modelfile, notesfile, helpfile))
      }
    )
  }


#' Upload model module
#'
#' UI function to upload a zip file with notes and a list of models
#'
#' @param id id of module
#' @param label label of module
#'
#' @rdname uploadModel
#'
#' @export
uploadModelUI <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    HTML("<br>"),
    tags$h5(label),
    fileInput(ns("uploadModel"), label = "Upload local model"),
    selectInput(
      ns("remoteModel"),
      label = "Select remote model",
      choices = dir(file.path(settings$pathToSavedModels)) %>%
        sub(pattern = '\\.zip$', replacement = ''),
      selected = NULL
    ),
    actionButton(ns("loadRemoteModel"), "Load Remote Model")#,
    #helpText("Remote models are only available on on https://isomemoapp.com")
  )
}


#' Server function upload model
#'
#' Backend for upload model module
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param yEstimates (reactive) An object created by \code{\link{estimateY}}.
#'  Distributions of the dependent variable.
#' @param formulas (reactive) formulas
#' @param data (reactive) data
#' @param uploadedNotes (reactive) variable that stores content of README.txt
#' @param inputFields (reactive) inputFields
#'
#' @export
uploadModel <-
  function(input,
           output,
           session,
           yEstimates,
           formulas,
           data,
           uploadedNotes,
           inputFields) {
    pathToModel <- reactiveVal(NULL)
    
    observeEvent(input$uploadModel, {
      pathToModel(input$uploadModel$datapath)
    })
    
    observeEvent(input$loadRemoteModel, {
      pathToModel(file.path(
        settings$pathToSavedModels,
        paste0(input$remoteModel, ".zip")
      ))
    })
    
    observeEvent(pathToModel(), {
      res <- try({
        zip::unzip(pathToModel())
        load("model.Rdata") # should contain: model, formulasObj, dataObj, inputObj
        uploadedNotes(readLines("README.txt"))
      })
      
      if (inherits(res, "try-error")) {
        shinyjs::alert("Could not load file.")
        return()
      }
      
      if (!exists("model") ||
          !exists("formulasObj") ||
          !exists("dataObj") || !exists("inputObj")) {
        shinyjs::alert("File format not valid. Model object not found.")
        return()
      }
      
      yEstimates(model)
      
      if (!is.null(model$data))
        updateMatrixInput(session, "measuresMatrix", value = as.matrix(model$data))
      
      for (n in names(dataObj))
        data[[n]] <- dataObj[[n]]
      for (n in names(formulasObj))
        formulas[[n]] <- formulasObj[[n]]
      
      inputFields(inputObj)
      
      alert("Model loaded")
    })
  }
