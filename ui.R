# ui.R

library(shiny)
library(EBImage)

shinyUI(pageWithSidebar(
  headerPanel("Singular value decomposition",
              tags$head(
                conditionalPanel(
                  condition="($('html').hasClass('shiny-busy'))",
                  tags$div(style="position:fixed;left:0;top:0;height:100%;width:100%;
                           z-index:99;background:rgba(0,0,0,0.5)"),
                  tags$div(style="position:fixed;display:block;left:50%;top:50%;-moz-transform:translate(-50%,-50%);
                           translate(-50%,-50%);transform:translate(-50%,-50%);z-index:99;color:#FFF;font-size:6vmin", 
                           "This may take a minute...")
                )
              )),
  sidebarPanel(
    sliderInput("rows", "Number of matrix rows:",
                min = 0, max = 512, value = 20),
    sliderInput("amplitude", "Amplitude:",
                min = -255, max = 255, value = 20)
  ),
  mainPanel(
    tags$head(tags$style("html.shiny-busy .container-fluid{filter: blur(5px)}
                    .shiny-image-output{min-height: 200px; height: auto !important}")),
    h4("Default Image (512 rows)"),
    img(src="sample.png", width = 384, height = 256),
    tags$h4(style="margin-top:20px", "Generated Image"),
    imageOutput("generated"),
    tags$h4(style="margin-top:20px", "Without Diagonal"),
    imageOutput("wDiag")
  )
))