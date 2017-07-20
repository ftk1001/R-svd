# Simple image compression with Singular Value Decomposition
# Author: hafiz

library(shiny)
library(EBImage)
library(GetoptLong)

width = 384
height = 256
file = 'www/sample.png'

makeList <- function(file, text) {
  list(src = file,
       contentType = 'image/png',
       width = width,
       height = height,
       alt = text)
}

outfile <- function() {
  outfile <- tempfile(fileext = '.png')
}

deconstructImage <- function(f) {
  tp = c()
  
  tp$img = readImage(f)
  tp$res = svd(tp$img)
  
  tp$D = diag(tp$res[[1]])
  tp$U = tp$res[[2]]
  tp$V = tp$res[[3]]
  tp$t.V = t(tp$V)
  
  tp$proc.img = matrix(rep(0, NROW(tp$img) * NCOL(tp$img)), ncol = NCOL(tp$img))
  
  return(tp)
}

processImage <- function(outfile, img) {
  png(outfile, width = width, height = height)
  par(mar = c(0,0,0,0))
  t.image <- img
  colorMode(t.image) = Grayscale
  image(flip(t.image))
  dev.off()
}

percentDifference <- function(old, new) {
  (old - new) / old * 100
}

server <- shinyServer(function(input, output, session) {
  output$generated <- renderImage({
    in.img <- input$img
    if (!is.null(in.img)) {
      file.rename(in.img$datapath, qq("@{in.img$datapath}.png"))
      file = qq("@{in.img$datapath}.png")
    }
    
    bf = deconstructImage(file)
    
    for (i in 1:input$rows) {
      bf$proc.img = bf$proc.img + bf$res[[1]][i] * (
        (matrix(bf$U[ ,i], ncol = 1) %*% bf$t.V[i, ])
      )
    }

    outfile <- outfile()
    processImage(outfile, Image(bf$proc.img))
    
    updateSliderInput(session, "rows", value = input$rows, max = NCOL(bf$D))
    output$maxRows <- renderText({qq("(@{NCOL(bf$D)} rows)")})
    
    org.size = file.info(file)$size
    new.size = file.info(outfile)$size
    output$imageSize <- renderText({
      qq(paste(
          "@{round(percentDifference(org.size, new.size), 3)}% compression;",
          "saved @{utils:::format.object_size(org.size - new.size, 'auto')}", 
          sep = " "))
    }, quoted = TRUE)

    makeList(outfile, 'Generated')
  }, deleteFile = TRUE)
})

ui <- shinyUI(pageWithSidebar(
  headerPanel("Singular value decomposition",
              tags$head(
                conditionalPanel(
                  condition = "($('html').hasClass('shiny-busy'))",
                  tags$div(class = "overlay"),
                  tags$div(class = "load-text",
                           "This may take a minute..."))
  )),
  sidebarPanel(
    fileInput('img', 'Choose PNG Image',
              accept = c(
                'image/png',
                '.png')
              ),
    sliderInput("rows", "Number of matrix rows:",
                min = 0, max = 512, value = 20),
    sliderInput("amplitude", "Amplitude:",
                min = -255, max = 255, value = 20)
  ),
  mainPanel(
    tags$head(
      tags$link(rel = "stylesheet", href = "style.css", type = "text/css")
    ),
    tags$div(
      class = "img-block",
      tags$h4(
        tags$span('Default Image'),
        textOutput("maxRows", inline = TRUE)
      ),
      img(src = substring(file, 5), width = width, height = height)
    ),
    tags$div(
      class = "img-block",
      tags$h4(
        "Generated Image",
        textOutput("imageSize")
      ),
      imageOutput("generated")
    )
  )
))

shinyApp(ui = ui, server = server)
