library(shiny)
library(miniUI)

library(jpeg)
library(keras)

vgg16_notop = application_vgg16(weights = 'imagenet', include_top = FALSE)


ui <- miniPage(
  gadgetTitleBar(left = NULL, right = NULL,"compare an image"),
  miniTabstripPanel(
    
    miniTabPanel("introduction", icon = icon("area-chart"),
                 miniContentPanel(
                   htmlOutput("intro")
                 )
    ),
    
    miniTabPanel( "Parameters", icon = icon("sliders"),
                 miniContentPanel(
                   fileInput('file1', 'Choose an image (max 5MB)'),
                   numericInput("input1", "dummy input 1", value=100),
                   numericInput("input2", "dummy input 2", value=2),
                   checkboxInput("input3", "dummy input 3", value = FALSE)
                 )
    ),
 
    miniTabPanel("image", icon = icon("file-image-o"),
                 miniContentPanel(
                   padding = 0,
                   imageOutput("plaatje")
                 )
    ),
    miniTabPanel("Resultaat", icon = icon("table"),
                 miniContentPanel(
                   verbatimTextOutput("ResultaatOut")
                 )
    )
  )
)

server <- function(input, output, session) {
  
  ProcessImage <- reactive({
    
    progress <- Progress$new(session, min=1, max=15)
    on.exit(progress$close())
    
    progress$set(
      message = 'in progress', 
      detail = 'This may take some time...'
    )
    
    inFile = input$file1
    
    img = image_load(inFile$datapath, target_size = c(224,224))
    x = image_to_array(img)
    
    dim(x) <- c(1, dim(x))
    x = imagenet_preprocess_input(x)
    
    # extract features
    features = vgg16_notop %>% predict(x)
    return(features)
  })
  
  output$intro <- renderUI({
    list(
      h4("This shiny app is a dummy for image input and processing"), 
      h4("Cheers, Longhow")
    )
  })
  
  
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
  
  output$ResultaatOut = renderPrint({
    
    cat(ProcessImage())
  })
  
  
  
  observeEvent(input$done, {
    stopApp(TRUE)
  })
}



shinyApp(ui, server)
