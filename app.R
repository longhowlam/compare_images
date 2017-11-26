#### Description ###############################################################
##
## Skeleton Shiny app  using miniUI 
## This will look just better on mobile devices
##


#### Libraries needed #########################################################
library(shiny)
library(miniUI)
library(jpeg)
library(keras)
library(DT)
library(text2vec)


##### Initals / startup code #################################################

vgg16_notop = application_vgg16(weights = 'imagenet', include_top = FALSE)
ImageFeatures = readRDS("data/ikeafeautures.RDs")
ImageMetaData = readRDS("data/ikeaimagesmeta.RDs")

##### Addittional Helper Functions ############################################

calcIkeaSimilarity = function(x)
{
  M1 <- as(matrix(x, ncol = length(x)), "dgCMatrix")
  out = text2vec::sim2(M1,ImageFeatures)
  out
}

#### MINIPAGE #################################################################

ui <- miniPage(
  gadgetTitleBar(
    left = NULL, 
    right = NULL,
    "compare an image"
  ),
  
  miniTabstripPanel(
    
    #### introduction tab ############
    miniTabPanel(
      "introduction", icon = icon("area-chart"),
      miniContentPanel(
        htmlOutput("intro")
      )
    ),
    
    #### parameters tab ##############
    miniTabPanel(
      "Parameters", icon = icon("sliders"),
      miniContentPanel(
        fileInput('file1', 'Choose an image (max 5MB)'),
        numericInput("input1", "dummy input 1", value=100),
        numericInput("input2", "dummy input 2", value=2),
        checkboxInput("input3", "dummy input 3", value = FALSE)
      )
    ),
 
    #### image tab ##################
    miniTabPanel(
      "image", icon = icon("file-image-o"),
      miniContentPanel(
        padding = 0,
        imageOutput("plaatje")
      )
    ),
    
    #### Resultaat tab ############
    miniTabPanel(
      "Resultaat", icon = icon("table"),
      miniContentPanel(
        verbatimTextOutput("ResultaatOut")
      )
    ),
    
    #### Tabel resultaat ###########
    miniTabPanel(
      "Tabel", icon = icon("table"),
      miniContentPanel(
        dataTableOutput("ResultaatTabel")
      )
    )
    
  )
)

################################################################################

#### SERVER FUNCTION ###########################################################

server <- function(input, output, session) {
  
  #### reactive functions ###################
  ProcessImage <- reactive({
    
    progress <- Progress$new(session, min=1, max=15)
    on.exit(progress$close())
    
    progress$set(
      message = 'in progress', 
      detail = 'This may take some time...'
    )
    
    inFile = input$file1
    if(is.null(inFile)){
      imgfile  = "www/kast.png"
    }else{
      imgfile = inFile$datapath
    }
    
    img = image_load(imgfile, target_size = c(224,224))
    x = image_to_array(img)
    
    dim(x) <- c(1, dim(x))
    x = imagenet_preprocess_input(x)
    
    # extract features
    features = vgg16_notop %>% predict(x)
    IkeaDistance = calcIkeaSimilarity(features)
    IkeaDistance
  })
  
  
  ###### OUTPUT ELEMENTS ######################################################
  
  #### intro ####
  output$intro <- renderUI({
    list(
      h4("This shiny app is a dummy for image input and processing"), 
      h4("Cheers, Longhow")
    )
  })
  
  
  #### plaatje ####
  output$plaatje <- renderImage({
    
    inFile = input$file1
    print(inFile)
    if (!is.null(inFile))
    {
      
      width  <- session$clientData$output_plaatje_width
      height <- session$clientData$output_plaatje_height
      list(
        src = inFile$datapath,
        width=width,
        height=height
      )
    }
    else
    {
      list(src="www/kast.png")
    }
  },
  deleteFile = FALSE
  )
  
  
  #### ResultaatOut ####
  output$ResultaatOut = renderPrint({
    pp = ProcessImage()
    print(pp)
  })
  
  
  #### ResultaatTabel ####
  output$ResultaatTabel =  renderDataTable({
   
    simies = ProcessImage()
    ImageMetaData2 = ImageMetaData
    ImageMetaData2$similarities = as.numeric(simies)
    ImageMetaData2$image = paste0(
      "<img src='",
      ImageMetaData2$imagefile,
      "'",
      "height='80' width='90' </img>"
    )
    datatable(
      escape = FALSE,
      rownames = FALSE, 
      data = ImageMetaData2
    )
  })
  
  observeEvent(input$done, {
    stopApp(TRUE)
  })
}


##### SHINY APP CALL ###########################################################

shinyApp(ui, server)
