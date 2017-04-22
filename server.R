# server.R

library(shiny)
library("EBImage")

shinyServer(function(input, output, session) {
  
  w = 400
  h = 300
  
  f = 'sample.png'
  img = readImage(f)
  
  res <- svd(img)
  
  d = res[[1]]
  D = diag(d)
  
  U = res[[2]]
  
  V = res[[3]]
  tV = t(V)
  
  # DEFAULT
  output$default <- renderImage({
    outfile <- tempfile(fileext = '.png')
    
    png(outfile, width = 2 * w, height = 2 * h)
    image(flip(img))
    dev.off()
    
    list(src = outfile,
         contentType = 'image/png',
         width = w,
         height = h,
         alt = input$obs)
  }, deleteFile = TRUE)
  
  #GENERATED
  output$generated <- renderImage({
    
    # Compress Image
    apx_img = matrix( rep(0, 768*512), ncol=512)
    for (i in 1:input$rows) {
      apx_img = apx_img + d[i] * matrix(U[,i], ncol = 1)%*%tV[i,]
    }
    
    # Display image
    outfile <- tempfile(fileext = '.png')
    
    png(outfile, width = 2 * w, height = 2 * h)
    tImage <- Image(apx_img)
    colorMode(tImage) = Grayscale
    image(flip(tImage))
    dev.off()
    
    list(src = outfile,
         contentType = 'image/png',
         width = w,
         height = h,
         alt = "Generated")
  }, deleteFile = TRUE)
  
  output$wDiag <- renderImage({
    outfile <- tempfile(fileext = '.png')
    
    png(outfile, width = 2 * w, height = 2 * h)
    tImage <- Image(input$amplitude*(U%*%tV))
    colorMode(tImage) = Grayscale
    image(flip(tImage))
    dev.off()
    
    list(src = outfile,
         contentType = 'image/png',
         width = w,
         height = h,
         alt = "Generated")
  }, deleteFile = TRUE)
})