library(shiny)
library(shinyMatrix)
library(shinyjs)
library(stringr)
path_dir <- '/Users/areguly6/Documents/Personal/Webpage/regulyagoston.github.io/teaching/autograde'
source(paste0(path_dir,'/grader_r/grade.r'))
source(paste0(path_dir,'/grader_r/uni_grade.r'))

mints = 1

neptun_codes = c('asd123','gvk123')

ui <- fluidPage(
  useShinyjs(),
  
  tags$br(), 
  tags$br(),
  
  fluidPage(
    h3("Upload R-script"),
    fileInput('inFile', 'Choose file'),
    actionButton('reset', 'Reset'),
    h3("Script status:"),
    verbatimTextOutput("summary"),
    verbatimTextOutput("chck_name")
  ),
  
  tags$br(), 
  tags$br(),
  
  fluidPage(
    h3("Select Assignment"),
    selectInput("assign", "Assignment:",
                c("Assignment 1" = "as1",
                  "Assignment 2" = "as2",
                  "Assignment 3" = "as3")),
    tags$br(),
    fluidRow(
      actionButton('grade', 'Autograde', class = "btn-primary"),
      #actionButton("stop", "Stop", class = "btn-danger"),
      actionButton('grade_reset', 'Reset')
    ),
    h6(paste('Autograding can take up to ', mints, ' minutes maximum.')),
    h6('After that it will exit and not evaluate your code.'),
    tags$br(),
    h3("Grade status:"),
    tableOutput("grade_res"),
    verbatimTextOutput("grade_sum")
  )
  
) # close fluid page

server <- function(input, output, session) {
  
  
  ########################
  # Handle uploaded file
  # Reset submitted script
  values <- reactiveValues(
    upload_state = NULL
  )
  
  # New upload
  observeEvent(input$inFile, {
    values$upload_state <- 'Uploaded'
  })
  
  observeEvent(input$reset, {
    shinyjs::reset()
    values$upload_state <- NULL
    hide("grade_res")
    hide("grade_sum")
  })
  
  
  file_input <- reactive({
    if (is.null(values$upload_state)) {
      return("No file uploaded.")
    } else if (values$upload_state == 'Uploaded') {
      return(input$inFile)
    }
  })
  
  ##
  # Output for file upload
  output$summary <- renderText({
    txt = ifelse( values$upload_state == 'Uploaded' , 
                  paste("Uploaded file:", file_input()$name ),
                file_input() )
    return(txt)
  })
  
  ##
  # Check validity of uploaded name
  output$chck_name <- renderText({
    
    # chck function
    chck_name <- function( inputfile ){
      tryCatch({
        any( substr( file_input()$name, 0, 6 ) == neptun_codes )
      }, 
      error = function( e )
        NULL
      )
    }

    txt = ifelse( !is.null( chck_name ),
                  ifelse( chck_name(),
                          'Valid script name!',
                          'Invalid script name! Use your neptun code as the first 6 character of your submission file!' ),
                  'No file uploaded!')
                  
    return(txt)
    
    #if ( values$upload_state == 'Uploaded' ){
    #  chck = any( substr( file_input()$name, 0, 6 ) == neptun_codes )
    #  if ( chck ){
    #    txt = 'Valid script name!'
    #  } else{
    #    txt = 'Invalid script name! Use your neptun code as the first 6 character of your submission file!'
    #  }
    #} else{
    #  txt = ''
    #}
    
  })
  
  ########################
  # Process uploaded file

  grading <- eventReactive( input$grade, {
    if ( is.character( file_input ) ){
      
      showNotification("No script file uploaded!")
      overflow = F
      grade_res = NULL
      
    } else{
      
      showNotification("Grading in progress!")
        
      # Process input
      #script = readLines( input$inFile$datapath )
      path_file = file_input()$datapath
          
      # Start grading
      if ( input$assign == 'as1' ){
        Mytestfile <- paste0(path_dir, "/solutions/grader_week01.R")   
      } else if ( input$assign == 'as2' ){
        Mytestfile <- paste0(path_dir, "/solutions/grader_week02.R")
      }
          
      # Eval function
      grade <- function( path_file, Mytestfile ){
          tryCatch({
            calc_one_Grade( path_file, Mytestfile,
                            suppress_warnings = F,
                            verbose = TRUE )
            }, 
            error = function( e )
                NULL
          )
      }
          
      setTimeLimit( elapsed = mints*60 ) # After x minutes it will shut downe
      st=Sys.time()
      grade_res <- grade( path_file, Mytestfile )
      et = Sys.time() - st
      setTimeLimit( elapsed = Inf )
          
      overflow = F
      if ( et >= mints ){
          grade_res = NULL
          overflow =T 
      }
          
      if ( !is.null( grade_res ) ){
         # Remove ID
         grade_res[,1] = NULL
             
         # Inform
         showNotification("Grading is done!")
      } else{
         
          # Inform
          showNotification(paste("Grading takes more than ", mints,
                                  " minutes! Revise your code!") )
      }
    }
    return( list( grade_res = grade_res, overflow = overflow ) )
  })
  
  ######
  # Reset autogrades
  observeEvent(input$grade_reset,{
    #shinyjs::reset("grading")
    hide("grade_res")
    hide("grade_sum")
  })
  
  
  ######
  # Grading show output
  
  # Detailed table
  output$grade_res <- renderTable({
    grading()$grade_res
  }, rownames = T)
  
  # Final point
  output$grade_sum <- renderText({
    if ( is.null(values$upload_state) ){
      txt = 'No file uploaded!'
    } else {
      withProgress(
        message='Grading in progress',
        detail='Grading in progress',
        value = 10,
        {
          gr <- grading()
          #for ( i in seq(0,mints,by=100)){
            incProgress(1/mints, detail = paste("Grading in progress!"))  
          #}
          
        }
      )
      if ( !gr$overflow ){
        txt = paste0( 'Overall points: ', sum( gr$grade_res[1,] ) , '/', ncol( gr$grade_res ) )    
      } else{
        txt = paste("Grading takes more than ", mints,
                    " minutes! Revise your code!")
        hide("grade_res")
      }
      
    }
    return(txt)
  })
  
  hide("grade_res")
  hide("grade_sum")
  observeEvent(input$grade,{
    show("grade_res")
    show("grade_sum")
    
  })
  
  
} # close server

shinyApp(ui, server)