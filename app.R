# app.R
# Demonstrates image compression with Singular Value Decomposition
# Author: hafiz
# Date: 4-22-17

library(shiny)
library(EBImage)

w = 384
h = 256
f = 'www/sample.png'

img = readImage(f)
res = svd(img)

d = res[[1]]
D = diag(d)

U = res[[2]]

V = res[[3]]
tV = t(V)

makeList <- function(fl, txt) {
  list(src = fl,
       contentType = 'image/png',
       width = w,
       height = h,
       alt = txt)
}

outfile <- function() {
  outfile <- tempfile(fileext = '.png')
}

processImage <- function(outfile, img) {
  png(outfile, width = 2 * w, height = 2 * h)
  par(mar = c(0,0,0,0))
  tImage <- img
  colorMode(tImage) = Grayscale
  image(flip(tImage))
  dev.off()
}

server <- shinyServer(function(input, output, session) {
  
  # GENERATED
  output$generated <- renderImage({
    
    # Compress Image
    apx_img = matrix( rep(0, 768*512), ncol=512)
    for (i in 1:input$rows) {
      apx_img = apx_img + d[i] * matrix(U[,i], ncol = 1)%*%tV[i,]
    }
    
    # Display image
    outfile <- outfile()
    processImage(outfile, Image(apx_img))
    
    makeList(outfile, 'Generated')
  }, deleteFile = TRUE)
  
  # DIAGONAL
  output$wDiag <- renderImage({
    outfile <- outfile()
    processImage(outfile, Image(input$amplitude * (U %*% tV)))
    
    makeList(outfile, 'Alternate')
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
    sliderInput("rows", "Number of matrix rows:",
                min = 0, max = NCOL(D), value = 20),
    sliderInput("amplitude", "Amplitude:",
                min = -255, max = 255, value = 20)
  ),
  mainPanel(
    tags$head(tags$link(rel = "stylesheet", href = "style.css", type = "text/css")),
    h4(paste('Default Image (', NCOL(D), ' rows)', sep = '')),
    img(src = substring(f, 5), width = w, height = h),
    h4("Generated Image"),
    imageOutput("generated"),
    h4("Without Diagonal"),
    imageOutput("wDiag")
  )
))

shinyApp(ui = ui, server = server)