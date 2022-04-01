library(shiny)

shinyUI(fluidPage(
  # Application title
  HTML("<h1>Demo: 'qmethod' package to analyse Q methodology data in R</h1>"),
  HTML("<hr/>"),
  sidebarLayout(
    sidebarPanel(
      HTML("<p>See more information <a href='https://github.com/aiorazabala/qmethod/wiki' target='_blank'>about the package</a>, <a href='https://github.com/aiorazabala/qmethod/wiki/Cookbook' target='_blank'>a cookbook</a>, and a paper on its <a href='http://journal.r-project.org/archive/accepted/zabala.pdf' target='_blank'>implementation and validation</a>.</p>"),
      HTML("<p style='font-size:x-small'><span style='text-transform:uppercase'>Technical notes:</span> 1) This interface performs the analysis only on Q studies with <b>'forced' distribution</b>. To run the analysis with non-forced distributions, see <a href='http://cran.r-project.org/web/packages/qmethod/qmethod.pdf' target='_blank'>documentation</a>. 2) Q-sorts are <b>flagged automatically</b>. You can explore this flagging <a href='#flagging'>below</a>. Manual flagging is possible using R beyond this simplified interface. Questions & comments (including how to improve this GUI) to <i>aiora [dot] zabala (at) gmail [dot] com</i></p>")
    ),

    mainPanel(
      HTML("<p>This is a graphical user interface (GUI) of the R package <a href='http://cran.r-project.org/web/packages/qmethod/index.html' target='_blank'>'qmethod'</a>, with basic functionality.</p>"),
      HTML("<p>Follow these steps to analyse Q methodology data:</p>

           <ol>

           <li>Upload your data from a *.csv file.</li>

           <li>Select the extraction method (<i>'PCA'</i> or <i>'centroid'</i>), the rotation method (<i>'varimax'</i> is most common), and the number of factors to extract.</li> 

           </ol>

           <p><a href='#full'>Full results</a> are displayed at the end. To run the same analysis directly in R and use the full package functionality, see the code created below in <a href='#code'><i>Run the analysis directly in R</i></a>.</p>

           "),

      HTML("<p style='font-size:small'><span style='text-transform:uppercase'>Advanced: </span>1) <b>explore the flagging of Q-sorts</b> resulting from different number of factors <a href='#flagging'>below</a>, or 2) <b>information to aid the selection of number of factors</b> <a href='#xfactors'>below</a>.</p>")
      )
),

HTML("<hr/>"),
h3('Step 1. Upload your data'),

sidebarLayout(
  sidebarPanel(
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv')),
    HTML("<p style='font-size:x-small'>See an example of the format of the dataset <a href='http://aiorazabala.net/qmethod-gui/lipset.csv'>here</a>.</p>"), 
    tags$hr(),
    checkboxInput('header', 'Header', TRUE),
    checkboxInput('statnames', 'Statement names in 1st column', FALSE),
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t'),
                 ','),
    radioButtons('quote', 'Quote',
                 c(None='',
                   'Double Quote'='"',
                   'Single Quote'="'"),
                 '"')
  ),

  mainPanel(
    textOutput('rawinfo'),
    tableOutput('contents')
  )
),

HTML("<hr/>"),

h3('Step 2. Select the extraction and rotation methods and the number of factors'),

sidebarLayout(
  sidebarPanel(
    sliderInput("nfactors", label = "Number of factors:",
                min = 2, max = 7, value = 3, step = 1),
    selectInput("extraction", label = "Extraction:",
                choices = c("PCA", "centroid"), selected = "PCA"),
    selectInput("rotation", label = "Rotation:",
                choices = c("none", "varimax", "quartimax", "promax", "oblimin", "simplimax", "cluster"), selected = "varimax"),
    sliderInput("maileft", label = "Item margin:",
                min = 0.8, max = 2.8, value = 1, step = 0.2),
  ),

  mainPanel(
    plotOutput("qmPlot"),
    verbatimTextOutput("summary")
  )
),

HTML("<hr/>"),

HTML("<h3 id='code'>Run the analysis directly in R</h3>"),

sidebarLayout(
  sidebarPanel(
    p("To run the above analysis in R and explore the results further, copy and paste the code on the right. (You need to <a href='http://aiorazabala.github.io/qmethod/Cookbook#2-install-r' target='_blank'>install R</a> AND <a href='http://aiorazabala.github.io/qmethod/Cookbook#3-install-the-package-only-once' target='_blank'>install the 'qmethod' package</a> in your computer first if you have not done so already).")
  ),
  mainPanel(
    p("1. Load the package and upload your data:"),
    verbatimTextOutput("codeUpload"),
    p("2. Run Q analysis:"),
    verbatimTextOutput("codeQmethod"),
    p("3. Plot the results:"),
    verbatimTextOutput("codePlot"),
    p("4. Save your data in R format:"),
    verbatimTextOutput("codeSave"),
    p("5. Export the full report of results in plain text:"),
    verbatimTextOutput("codeReport")
  )
),

HTML("<hr/>"),
HTML("<h3 id='flagging'>Advanced: Explore the flagged Q-sorts</h3>"),
p("The table indicates with an '*' those Q-sorts flagged for each factor, using the automatic method. Change the number of factors in the slider below, to see how flaggings vary."),

sidebarLayout(
  sidebarPanel(
    sliderInput("nfactors2", label = "Number of factors:", min = 2, max = 7, value = 3, step = 1),
    selectInput("extraction2", label = "Extraction:",
                choices = c("PCA", "centroid"), selected = "PCA"),
    selectInput("rotation2", label = "Rotation:",
                choices = c("none", "varimax", "quartimax", "promax", "oblimin", "simplimax", "cluster"), selected = "varimax")
  ),

  mainPanel(
    verbatimTextOutput("flaggedqsorts")
  )
),

HTML("<hr/>"),
HTML("<h3 id='xfactors'>Advanced: Explore how many factors to extract</h3>"),

sidebarLayout(
  sidebarPanel(
    sliderInput("nfactors3", label = "Number of factors:",
                min = 2, max = 7, value = 3, step = 1),
    selectInput("extraction3", label = "Extraction:",
               choices = c("PCA", "centroid"), selected = "PCA"),
    selectInput("rotation3", label = "Rotation:",
                choices = c("none", "varimax", "quartimax", "promax", "oblimin", "simplimax", "cluster"), selected = "varimax")
  ),

  mainPanel(
    verbatimTextOutput("factorsel"),
    plotOutput("screePlot")
  )
),

HTML("<hr/>"),
HTML("<h3 id='full'>Full results</h3>"),

fluidRow(
  verbatimTextOutput("fullResults")
)
))
