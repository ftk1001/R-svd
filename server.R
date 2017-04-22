# server.R

library(shiny)
library("EBImage")

shinyServer(function(input, output, session) {
  
  w = 384
  h = 256
  
  f = 'www/sample.png'
  img = readImage(f)
  res = svd(img)
  
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
  
  d = res[[1]]
  D = diag(d)
  
  U = res[[2]]
  
  V = res[[3]]
  tV = t(V)
  
  #GENERATED
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
  
  output$wDiag <- renderImage({
    outfile <- outfile()
    processImage(outfile, Image(input$amplitude * (U %*% tV)))
    
    makeList(outfile, 'Alternate')
  }, deleteFile = TRUE)
})