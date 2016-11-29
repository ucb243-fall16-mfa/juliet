shinyUI(
  fluidPage(
    # Application title
    titlePanel('Plots from MFA'),
    
    #Sidebar with input widgets
    sidebarLayout(
      sidebarPanel(
        selectInput("plot_ob","Select what to plot",c("a barchart of eigenvalues"="a",
                                                      "a scatterplot of common factor scores"="b",
                                                      "a scatterplot of partial factor scores"="c",
                                                      "a scatterplot of partial factor & loadings")),
        #Choose the two dimensions to plot
        sliderInput("dim_1","The first Dimension to Plot",min=1,max=12,value=4),
        sliderInput("dim_2","The second Dimension to Plot",min=1,max=12,value=5)
      ),
      #Show a plot from the MFA calculation
      mainPanel(
        plotOutput("plot")
      )
    )
  )
)