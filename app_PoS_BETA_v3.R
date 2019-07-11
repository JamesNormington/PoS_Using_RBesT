library(RBesT); library(shinyjs)
library(data.table); library(DT); library(shiny)
## some gMAP calls below require this option
options(RBesT.MC.control=list(adapt_delta=0.99999))

ui = navbarPage("", 
                tabPanel("Historical trial meta-data",
                         sidebarLayout(
                           sidebarPanel(selectInput("endpt", "Endpoint type", choices = c("Binary", "Normal", "Poisson")),
                         conditionalPanel("input.endpt == 'Normal'", numericInput("sigma", "Please enter the Reference Scale (i.e., the standard deviation of the individual-level data).",
                                                     value = 10, min = .Machine$double.eps)),
                         checkboxInput("use_hist" ,"Use historical trial meta-data", value = TRUE),
                         conditionalPanel("input.use_hist",
                                          DT::dataTableOutput("table", width = 3),
                                          tags$hr(),
                                          checkboxInput("add_study", "Add study to table?"),
                                          conditionalPanel("input.add_study",
                                                           textInput("study_add", "Name of added study", value = "New Study"),
                                                           numericInput("n_add", "Sample size of added study", value = 0, step = 1),
                                                           conditionalPanel("input.endpt == 'Binary'",
                                                                            numericInput("r_add", "Number of events in added study", value = 0, step = 1)),
                                                           conditionalPanel("input.endpt == 'Normal'", 
                                                                            numericInput("m_add", "Sample mean in added study", value = 0)),
                                                           conditionalPanel("input.endpt == 'Poisson'",
                                                                            numericInput("m_add_pois", "Total count in added study", value = 1000),
                                                                            numericInput("t_add", "Trial duration of added study", value = 10)),
                                                           actionButton("add_btn", "Add study")),
                                          checkboxInput("delete_study", "Delete study from table?"),
                                          conditionalPanel("input.delete_study",
                                                           numericInput("delete_row", "Number of row to delete", value = 1, step = 1),
                                                           actionButton("delete_btn", "Delete study"))
                                                             
                                          )
                         ),
                         mainPanel(actionButton("make.forest", "Visualize meta-data and MAP prior"),
                                   textOutput("samp_size_warn"),
                                   plotOutput("forest"),
                                   htmlOutput("plot_err"),
                                   textOutput("unique_warn"),
                                   htmlOutput("forest_plot_addendum") 
                         )
                         )),
                        
                tabPanel("New trial information", 
                         conditionalPanel(condition = "input.endpt == 'Binary'",
                                          numericInput("n.new", "Sample size of treatment group at interim", value = 100, min = 1, step = 1),
                                          numericInput("r.new", "Number of events within treatment group at interim", value = 50, min = 1, step = 1),
                                          numericInput("n.trt.target", "Total target sample size within treatment group", value = 150, min = 1, step = 1), 
                                          selectInput("samp_bin", "One or two sample trial?", choices = c("One", "Two")),
                                          conditionalPanel(condition = "input.samp_bin == 'Two'", 
                                                           numericInput("n.ctrl", "Sample size of control group at interim", value = 100, min = 1, step = 1),
                                                           numericInput("r.ctrl", "Number of events within control group at interim", value = 50, min = 1, step = 1), 
                                                           numericInput("n.ctrl.target", "Total target sample size within control group", value = 150, min = 1, step = 1)
                                          )),
                conditionalPanel(condition = "input.endpt == 'Normal'",
                                 numericInput("n.new", "Sample size of treatment group at interim", value = 100, min = 1, step = 1),
                                 numericInput("m.new", "Sample mean within treatment group at interim", value = -20),
                                 numericInput("n.trt.target", "Total target sample size within treatment group", value = 150, min = 1, step = 1), 
                                 selectInput("samp_norm", "One or two sample trial?", choices = c("One", "Two")),
                                 conditionalPanel(condition = "input.samp_norm == 'Two'", 
                                                  numericInput("n.ctrl", "Sample size of control group at interim", value = 100, min = 1, step = 1),
                                                  numericInput("m.ctrl", "Sample mean within control group at interim", value = -20),
                                                  numericInput("n.ctrl.target", "Total target sample size within control group", value = 150, min = 1, step = 1)
                                 )),
                conditionalPanel(condition = "input.endpt == 'Poisson'",
                                 numericInput("n.new.pois", "Sample size of treatment group at interim", value = 100, min = 1, step = 1),
                                 numericInput("y.new.pois", "Total count within treatment group at interim", value = 20, min = 0, step = 1),
                                 numericInput("n.trt.target", "Total target sample size within treatment group", value = 150, min = 1, step = 1), 
                                 selectInput("samp_pois", "One or two sample trial?", choices = c("One", "Two")),
                      
                                 conditionalPanel(condition = "input.samp_pois == 'Two'", 
                                                  numericInput("n.ctrl.pois", "Sample size of control group at interim", value = 0, min = 1, step = 1),
                                                  numericInput("y.ctrl.pois", "Total count within control group at interim", value = 20, min = 0, step = 1),
                                                  numericInput("n.ctrl.target", "Total target sample size within control group", value = 150, min = 1, step = 1)
                                 ))),
        
                tabPanel("PoS calculation",
                         sidebarLayout(
                           sidebarPanel(conditionalPanel("input.use_hist", tags$h3("Model specification")),
                                        numericInput("seed", "Random seed (positive integer)", value=sample(1:10000, 1), min = 1),
                                        conditionalPanel("input.use_hist",
                                        selectInput("tau_dist", HTML("<p> Specify prior for &tau; </p>"), 
                                                    choices = c(`HalfNormal` = "HalfNormal", `TruncNormal` = "TruncNormal", 
                                                                `Uniform` = "Uniform", `Gamma` = "Gamma", `InvGamma` = "InvGamma", 
                                                                `LogNormal` = "LogNormal", `TruncCauchy` = "TruncCauchy", 
                                                                `Exponential` = "Exp", `Fixed`  ="Fixed")),
                                        conditionalPanel("input.tau_dist == 'HalfNormal'", HTML("<p> &tau; ~ HalfNormal(&mu;, &sigma;) </p>"), 
                                                         numericInput("tau_arg1_HN", HTML("<p> &mu; </p>"), value = 0, min = .Machine$double.xmin), 
                                                         numericInput("tau_arg2_HN", HTML("<p> &sigma; </p>"), value = 1, min = .Machine$double.xmin)),
                                        conditionalPanel("input.tau_dist == 'TruncNormal'", HTML("<p> &tau; ~ TruncNormal(&mu;, &sigma;) </p>"), 
                                                         numericInput("tau_arg1_TN", HTML("<p> &mu; </p>"), value = 0, min = .Machine$double.xmin), 
                                                         numericInput("tau_arg2_TN", HTML("<p> &sigma; </p>"), value = 1, min = .Machine$double.xmin)),
                                        conditionalPanel("input.tau_dist == 'Uniform'", HTML("<p> &tau; ~ Uniform(a, b) </p>"), 
                                                         numericInput("tau_arg1_Uni", HTML("a"), value = 0), 
                                                         numericInput("tau_arg2_Uni", HTML("b"), value = 1)),
                                        conditionalPanel("input.tau_dist == 'Gamma'", HTML("<p> &tau; ~ Gamma(shape = &alpha;, rate = &beta;) </p>"), 
                                                         numericInput("tau_arg1_G", HTML("<p> &alpha; </p>"), value = 0.001), 
                                                         numericInput("tau_arg2_G", HTML("<p> &beta; </p>"), value = 0.001)),
                                        conditionalPanel("input.tau_dist == 'InvGamma'", HTML("<p> &tau; ~ InverseGamma(shape = &alpha;, scale = &beta;) </p>"), 
                                                         numericInput("tau_arg1_IG", HTML("<p> &alpha; </p>"), value = 0.001), 
                                                         numericInput("tau_arg2_IG", HTML("<p> &beta; </p>"), value = 0.001)),
                                        conditionalPanel("input.tau_dist == 'LogNormal'", HTML("<p> &tau; ~ LogNormal(&mu;, &sigma;) </p>"), 
                                                         numericInput("tau_arg1_LN", HTML("<p> &mu; </p>"), value = 0), 
                                                         numericInput("tau_arg2_LN", HTML("<p> &sigma; </p>"), value = 50, min = .Machine$double.xmin)),
                                        conditionalPanel("input.tau_dist == 'TruncCauchy'", HTML("<p> &tau; ~ TruncCauchy(&mu;, &sigma;) </p>"), 
                                                         numericInput("tau_arg1_TC", HTML("<p> &mu; </p>"), value = 0), 
                                                         numericInput("tau_arg2_TC", HTML("<p> &sigma; </p>"), value = 50, min = .Machine$double.xmin)),
                                        conditionalPanel("input.tau_dist == 'Exp'", HTML("<p> &tau; ~ Exp(rate = &lambda;) </p>"), 
                                                         numericInput("tau_arg_exp", HTML("<p> &lambda; </p>"), value = 0.001)), 
                                        conditionalPanel("input.tau_dist == 'Fixed'", HTML("<p> &tau; = &tau;<sub>0</sub> </p>"), 
                                                         numericInput("tau_fixed", HTML("<p> &tau;<sub>0</sub> </p>"), value = 1)),
                                        selectInput("beta_choice", HTML("<p> Specify prior for &beta; </p>"), choices = c("Default", "Specify")),
                                        conditionalPanel("input.beta_choice == 'Specify'", HTML("<p> &beta; ~ Normal(&mu;, &sigma;) </p>"),
                                                         numericInput("beta_mu", HTML("<p> &mu; </p>"), value = 0), 
                                                         numericInput("beta_sigma", HTML("<p> &sigma; </p>"), value = 100, min = .Machine$double.xmin)),
                                        conditionalPanel("input.beta_choice == 'Default'", 
                                                         conditionalPanel("input.endpt == 'Binary'", HTML("<p> Default prior is &beta; ~ N(0, 2) </p>")),
                                                         conditionalPanel("input.endpt == 'Normal'", HTML("<p> Default prior is &beta; ~ N(0, 100 * sd(y))) </p>")),
                                                         conditionalPanel("input.endpt == 'Poisson'", HTML("<p> Default prior is &beta; ~ N(0, sd(log(y + 0.5 + log(t*n))))" ))),
                                        numericInput("wt", "Weight for robust prior", value = 0.2, min = 0, max = 1)
                                        )),

                           
                           mainPanel(tags$b("Success criterion"), 
                                     selectInput("sided", "One- or two-sided decision rule?", choices = c("One", "Two")),
                                     conditionalPanel("(input.samp_bin == 'One' & input.endpt == 'Binary') | (input.samp_norm == 'One' & input.endpt == 'Normal') | (input.samp_pois == 'One' & input.endpt == 'Poisson')",
                                                      selectInput("sidedness", "Lower or upper tail?", c("Lower", "Upper"))),
                                     conditionalPanel("input.samp_pois == 'Two' | input.samp_bin == 'Two' & (input.endpt == 'Binary' | input.endpt == 'Poisson')",
                                       selectInput("link", "Select a link function g() for the decision rule", 
                                                      choices = c("identity", "logit", "log"))),
                    
                                     conditionalPanel("input.endpt == 'Binary'",
                                     conditionalPanel("input.sidedness == 'Lower'",
                                     conditionalPanel("input.samp_bin == 'One' & input.sided == 'One'", 
                                                      HTML("<p> Enter the definition of trial success, which is of the form P(p < p<sub>0</sub>) > 1 - &alpha;/2 </p>") , br(),
                                                      numericInput("p0", HTML("<p> Null value for P(event), p<sub>0</sub> </p>"), value = 0.5, min = 0.01, max = 0.99)),
                                     conditionalPanel("input.samp_bin == 'One' & input.sided == 'Two'",
                                                      HTML("<p> Enter the definition of trial success, which is of the form P(p < p<sub>0</sub>) > 1 - &alpha; </p>")), br(),
                                                      numericInput("p0", HTML("<p> Null value for P(event), p<sub>0</sub> </p>"), value = 0.5, min = 0.01, max = 0.99)),
                                     
                                     conditionalPanel("input.sidedness == 'Upper'",
                                                      conditionalPanel("input.samp_bin == 'One' & input.sided == 'One'", 
                                                                       HTML("<p> Enter the definition of trial success, which is of the form P(p > p<sub>0</sub>) > 1 - &alpha;/2 </p>") , br(),
                                                                       numericInput("p0", HTML("<p> Null value for P(event), p<sub>0</sub> </p>"), value = 0.5, min = 0.01, max = 0.99)),
                                                      conditionalPanel("input.samp_bin == 'One' & input.sided == 'Two'",
                                                                       HTML("<p> Enter the definition of trial success, which is of the form P(p > p<sub>0</sub>) > 1 - &alpha; </p>")), br(),
                                                      numericInput("p0", HTML("<p> Null value for P(event), p<sub>0</sub> </p>"), value = 0.5, min = 0.01, max = 0.99)),
                                     
                                     
                                     conditionalPanel("input.samp_bin == 'Two' & input.sided == 'One'",
                                                      HTML("<p> Enter the definition of trial success, which is of the form P(g(p<sub>ctrl</sub>) - g(p<sub>trt</sub>) < 0) > 1 - &alpha;/2 </p>"), br(),
                                                      numericInput("diff0", HTML("<p> Null value for g(p<sub>ctrl</sub>) - g(p<sub>trt</sub>) </p>"), value = 0, min = -0.99, max = 0.99)),
                                     conditionalPanel("input.samp_bin == 'Two' & input.sided == 'Two'",
                                                      HTML("<p> Enter the definition of trial success, which is of the form P(g(p<sub>ctrl</sub>) - g(p<sub>trt</sub>) < 0) > 1 - &alpha; </p>"), br(),
                                                      numericInput("diff0", HTML("<p> Null value for g(p<sub>ctrl</sub>) - g(p<sub>trt</sub>) </p>"), value = 0, min = -0.99, max = 0.99))),
                                     conditionalPanel("input.endpt == 'Normal'",
                                                      
                                                      conditionalPanel("input.sidedness == 'Lower'",
                                                      conditionalPanel("input.samp_norm == 'One' & input.sided == 'One'", 
                                                                       HTML("<p> Enter the definition of trial success, which is of the form P(&mu; < &mu;<sub>0</sub>) > 1 - &alpha;/2 </p>") , br(),
                                                                       numericInput("mu0", HTML("<p> Null hypothesized value of &mu; (&mu;<sub>0</sub>) </p>"), value = 0)),
                                                      conditionalPanel("input.samp_norm == 'One' & input.sided == 'Two'",
                                                                       HTML("<p> Enter the definition of trial success, which is of the form P(&mu; < &mu;<sub>0</sub>) > 1 - &alpha; </p>")) , br(),
                                                                       numericInput("mu0", HTML("<p> Null hypothesized value of &mu; (&mu;<sub>0</sub>) </p>"), value = 0)),
                                                      
                                                      conditionalPanel("input.sidedness == 'Upper'",
                                                                       conditionalPanel("input.samp_norm == 'One' & input.sided == 'One'", 
                                                                                        HTML("<p> Enter the definition of trial success, which is of the form P(&mu; > &mu;<sub>0</sub>) > 1 - &alpha;/2 </p>") , br(),
                                                                                        numericInput("mu0", HTML("<p> Null hypothesized value of &mu; (&mu;<sub>0</sub>) </p>"), value = 0)),
                                                                       conditionalPanel("input.samp_norm == 'One' & input.sided == 'Two'",
                                                                                        HTML("<p> Enter the definition of trial success, which is of the form P(&mu; > &mu;<sub>0</sub>) > 1 - &alpha; </p>")) , br(),
                                                                       numericInput("mu0", HTML("<p> Null hypothesized value of &mu; (&mu;<sub>0</sub>) </p>"), value = 0)),
                                                      
                                                      conditionalPanel("input.samp_norm == 'Two' & input.sided == 'One'",
                                                                       HTML("<p> Enter the definition of trial success, which is of the form P(&mu;<sub>ctrl</sub> - &mu;<sub>trt</sub>< &Delta;<sub>0</sub>) > 1 - &alpha;/2 </p>"), br(),
                                                                       numericInput("diff0", HTML("<p> Null hypothesized value of &Delta;<sub>0</sub> (=  &mu;<sub>ctrl</sub> - &mu;<sub>trt</sub>) </p>"), value = 0)),
                                                      conditionalPanel("input.samp_norm == 'Two' & input.sided == 'Two'",
                                                                       HTML("<p> Enter the definition of trial success, which is of the form P(&mu;<sub>ctrl</sub> - &mu;<sub>trt</sub> < &Delta;<sub>0</sub>) > 1 - &alpha; </p>"), br(),
                                                                       numericInput("diff0", HTML("<p> Null hypothesized value of &Delta;<sub>0</sub> (=  &mu;<sub>ctrl</sub> - &mu;<sub>trt</sub>) </p>"), value = 0))),
                                     conditionalPanel("input.endpt == 'Poisson'",
                                                      conditionalPanel("input.sidedness == 'Lower'", 
                                                      conditionalPanel("input.samp_pois == 'One' & input.sided == 'One'", 
                                                                       HTML("<p> Enter the definition of trial success, which is of the form P(&lambda; < &lambda;<sub>0</sub>) > 1 - &alpha;/2 </p>"), br(),
                                                                       numericInput("lambda0", HTML("<p> Null hypothesized value of &lambda; (&lambda;<sub>0</sub>) </p>"), value = 20)),
                                                      conditionalPanel("input.samp_pois == 'One' & input.sided == 'Two'",
                                                                       HTML("<p> Enter the definition of trial success, which is of the form P(&lambda; < &lambda;<sub>0</sub>) > 1 - &alpha;"), br(),
                                                                       numericInput("lambda0", HTML("<p> Null hypothesized value of &lambda; (&lambda;<sub>0</sub>) </p>"), value = 20))),
                                                      
                                                      conditionalPanel("input.sidedness == 'Upper'", 
                                                                       conditionalPanel("input.samp_pois == 'One' & input.sided == 'One'", 
                                                                                        HTML("<p> Enter the definition of trial success, which is of the form P(&lambda; > &lambda;<sub>0</sub>) > 1 - &alpha;/2 </p>"), br(),
                                                                                        numericInput("lambda0", HTML("<p> Null hypothesized value of &lambda; (&lambda;<sub>0</sub>) </p>"), value = 20)),
                                                                       conditionalPanel("input.samp_pois == 'One' & input.sided == 'Two'",
                                                                                        HTML("<p> Enter the definition of trial success, which is of the form P(&lambda; > &lambda;<sub>0</sub>) > 1 - &alpha;"), br(),
                                                                                        numericInput("lambda0", HTML("<p> Null hypothesized value of &lambda; (&lambda;<sub>0</sub>) </p>"), value = 20))),
                                                      
                                                      conditionalPanel("input.samp_pois == 'Two' & input.sided == 'One'",
                                                                       HTML("<p> Enter the definition of trial success, which is of the form P(g(&lambda;<sub>ctrl</sub>) - g(&lambda;<sub>trt</sub>) < &Delta;<sub>0</sub>) > 1 - &alpha;/2 </p>"), br(),
                                                                       numericInput("diff0.pois", HTML("<p> Null hypothesized value of &Delta;<sub>0</sub> (=  g(&lambda;<sub>ctrl</sub>) - g(&lambda;<sub>trt</sub>)) </p>"), value = 0)),
                                                      conditionalPanel("input.samp_pois == 'Two' & input.sided == 'Two'",
                                                                       HTML("<p> Enter the definition of trial success, which is of the form P(g(&lambda;<sub>ctrl</sub>) - g(&lambda;<sub>trt</sub>) < &Delta;<sub>0</sub>) > 1 - &alpha; </p>"), br(),
                                                                       numericInput("diff0.pois", HTML("<p> Null hypothesized value of &Delta;<sub>0</sub> (=  g(&lambda;<sub>ctrl</sub>) - g(&lambda;<sub>trt</sub>)) </p>"), value = 0))),
                                     
                                     numericInput("alpha", HTML("<p> Type I error rate, &alpha; </p>"), value = 0.05, min = .Machine$double.xmin, max = 1 - .Machine$double.xmin), 
                                     actionButton("compute.pos", "Compute PoS!"), br(),
                                     textOutput("warning.wt"),
                                     textOutput("warning.alpha"),
                                     br(),
                                     htmlOutput("results")
                                     
                           ) # close mainPanel
                         ) # close sidebarlayout 
                         ), # close tabPanel 
                tabPanel("Help", uiOutput("link0"), br(), uiOutput("link"), br(), uiOutput("link2"), br(), uiOutput("link3"))
                ) # close NavBarPanel

server = function(input, output, session) {
  
  this_table_bin = data.frame(study = c("Study 1", "Study 2", "Study 3"), n = rep(100, 3), r = c(40, 50, 60), stringsAsFactors = FALSE)
  names(this_table_bin) = c("Study", "n", "# of events")
  this_table_norm = data.frame(study = c("Study 1", "Study 2", "Study 3"), n = rep(100, 3), m = c(-10, -20, -30), stringsAsFactors = FALSE)
  names(this_table_norm) = c("Study", "n", "Sample mean")
  this_table_pois = data.frame(study = c("Study 1", "Study 2", "Study 3"), n = rep(100, 3), y = c(1113, 980, 1020), t = rep(12, 3), stringsAsFactors = FALSE)
  names(this_table_pois) = c("Study", "n", "Total count", "Trial duration")
  
  val = reactiveValues(mat = this_table_bin)
  val2 = reactiveValues(mat = this_table_norm)
  val3 = reactiveValues(mat = this_table_pois)
  
  which_val = function() {
    if(input$endpt == "Binary") {
      return(val)   
    } else if(input$endpt == "Normal") {
      return(val2)
    } else{
      return(val3)
    }
  }
  
  observeEvent(input$add_btn, {
    if(input$endpt == "Binary") {
      t = as.data.frame(rbind(which_val()$mat, c(input$study_add, input$n_add, input$r_add)))
      val$mat <<- t    
    } else if(input$endpt ==   "Normal") {
      t = as.data.frame(rbind(which_val()$mat, c(input$study_add, input$n_add, input$m_add)))
      val2$mat <<- t
    } else{
      t = as.data.frame(rbind(which_val()$mat, c(input$study_add, input$n_add, input$m_add_pois, input$t_add)))
      val3$mat <<- t
    }
  })
  
  observeEvent(input$delete_btn, {
    t = which_val()$mat
    if (!is.null(input$delete_row)) {
      t <- t[-as.numeric(input$delete_row),]
    }
    if(input$endpt == "Binary") {
      val$mat <<- t    
    } else if(input$endpt == "Normal") {
      val2$mat <<- t
    } else{
      val3$mat <<- t
    }
  })
  
  # Create editable DataTabe of meta-data.
  output$table <- DT::renderDataTable({
    if(input$endpt == "Binary") {
      DT::datatable(val$mat, options = list(dom = 't', columnDefs = list(list(width = "50px", targets = "_all"))), 
                    editable = TRUE, selection="single")
    } else if(input$endpt == "Normal") {
      DT::datatable(val2$mat, options = list(dom = 't', columnDefs = list(list(width = "50px", targets = "_all"))), 
                    editable = TRUE, selection="single")
    } else{
      DT::datatable(val3$mat, options = list(dom = 't', columnDefs = list(list(width = "50px", targets = "_all"))), 
                    editable = TRUE, selection="single")
    }
    
  }, server = TRUE)
  
  proxy = dataTableProxy('table')
  
  observeEvent(input$table_cell_edit, {
    info = input$table_cell_edit
    
    i = info$row
    j = info$col
    v = info$val
    
    if(input$endpt == "Binary") {
      val$mat[i,j] = DT::coerceValue(v, val$mat[i,j])
      replaceData(proxy, val$mat, rownames = FALSE)
    } else if(input$endpt == "Normal") {
      val2$mat[i,j] = DT::coerceValue(v, val2$mat[i,j])
      replaceData(proxy, val2$mat, rownames = FALSE)
    } else{
      val3$mat[i,j] = DT::coerceValue(v, val3$mat[i,j])
      replaceData(proxy, val3$mat, rownames = FALSE)
    }
    
    
  })
  
  # Error checking for MAP prior
  errCheck_MAP = function() {
    output$warning.wt = output$warning.alpha = renderText({""})
    
    co.data = which_val()$mat
    
    co.data[,2] = as.numeric(co.data[,2])
    co.data[,3] = as.numeric(co.data[,3])
    if(input$endpt == "Poisson") co.data[,4] = as.numeric(co.data[,4])
    
    # Error check historical data
    if(sum(co.data[,1] == "") > 0 | sum(is.na(co.data[,c(2,3)])) > 0) return("Please complete the table of historical trial meta-data.")
    if(sum(co.data[,2] == "") > 0) return("Please complete the table of historical trial meta-data.")
    if(sum(co.data[,3] == "") > 0) return("Please complete the table of historical trial meta-data.")
    if(input$endpt == "Poisson") {
      if(sum(is.na(co.data[,4])) > 0) return("Please complete the table of historical trial meta-data.")
    }
    
    if(length(unique(co.data[,1])) != nrow(co.data)) {
      output$unique_warn = renderText({"Each row should uniquely define one study!"})
    } else{
      output$unique_warn = renderText({""})
    }
    
    if(sum(co.data[,2] <= 0) > 0) return("Your 2nd column should contain control arm sample sizes, which are positive integers.")  
    if(sum(co.data[,2]%%1 != 0) > 0) return("Your 2nd column should contain control arm sample sizes, which are positive integers.")  
    
    if(sum(!is.numeric(co.data[,2])) > 0) return("Your 2nd column should contain numbers!")
    if(sum(co.data[,3] < 0) > 0 & input$endpt == "Binary") return("Your 3rd column should contain # of events in control group, which are non-negative integers.")  
    if(input$endpt == "Binary" & sum(co.data[,3]%%1 != 0) > 0) return("Your 3rd column should contain control arm sample sizes, which are positive integers.")  
    
    if(sum(!is.numeric(co.data[,3])) > 0) return("Your 3rd column should contain numbers!")
    if(input$endpt == "Binary" & (sum(co.data[,2] < co.data[,3]) > 0)) return("Your number of events (r) cannot exceed your sample size (n)!")
    if(input$endpt == "Poisson") {
      if(sum(!is.numeric(co.data[,4])) > 0) return("Your 4th column should contain numbers!")
      if(((sum(co.data[,3] < 0) > 0) | (sum(co.data[,4] <= 0) > 0))) return("Your total counts (y) must be non-negative and your trial durations (t) must be positive!")
    }
    
    if(sum(co.data[,2] >= 100000) > 0) {
      output$samp_size_warn = renderText({"MAP prior is slower to compute for large sample sizes..."})
    } else{
      output$samp_size_warn = renderText({""})
    }
    
    if(input$endpt == "Normal" & (!is.numeric(input$sigma) | input$sigma < 0)) {return("Reference Scale must be a number greater than 0!")}
    if(input$endpt == "Normal" & input$sigma > 1000000) {return("Are you sure the Reference Scale is that high? Think about rescaling the data.")}
    if(!is.numeric(input$seed) | !input$seed%%1 == 0| input$seed < 1){return("Seed must be a positive integer!")}
    
    
  
    if(!is.numeric(input$wt) | input$wt < 0 | input$wt > 1){return("Weight for non-informative prior must be between 0 and 1!")}
    if(input$wt > 0.7 & input$wt <= 1){
      output$warning.wt = renderText({
        ("Note: It is not recommended for the non-informative prior's weight to be greater than 0.7.")
      })
    }
    if(input$tau_dist == "HalfNormal" & (!is.numeric(input$tau_arg1_HN) | !is.numeric(input$tau_arg2_HN) | input$tau_arg1_HN < 0 | input$tau_arg2_HN <= 0)) {return(HTML(("<p> Parameters for &tau; prior must be non-negative numbers! </p>")))}
    if(input$tau_dist == "TruncNormal" & (!is.numeric(input$tau_arg1_TN) | !is.numeric(input$tau_arg2_TN) | input$tau_arg1_TN < 0 | input$tau_arg2_TN <= 0)) {return(HTML(("<p> Parameters for &tau; prior must be non-negative numbers! </p>")))}
    if(input$tau_dist == "Uniform" & (!is.numeric(input$tau_arg1_Uni) | !is.numeric(input$tau_arg2_Uni) | (input$tau_arg2_Uni <= input$tau_arg1_Uni))) {return(HTML(("<p> Parameters for &tau; prior must be numeric, with a < b! </p>")))}
    if(input$tau_dist == "Gamma" & (!is.numeric(input$tau_arg1_G) | !is.numeric(input$tau_arg2_G) | input$tau_arg1_G <= 0 | input$tau_arg2_G <= 0)) {return(HTML(("<p> Parameters for &tau; prior must be positive numbers! </p>")))}
    if(input$tau_dist == "InvGamma" & (!is.numeric(input$tau_arg1_IG) | !is.numeric(input$tau_arg2_IG) | input$tau_arg1_IG <= 0 | input$tau_arg2_IG <= 0)) {return(HTML(("<p> Parameters for &tau; prior must be positive numbers! </p>")))}
    if(input$tau_dist == "LogNormal" & (!is.numeric(input$tau_arg1_LN) | !is.numeric(input$tau_arg2_LN) | input$tau_arg1_LN < 0 | input$tau_arg2_LN <= 0)) {return(HTML(("<p> Parameters for &tau; prior must be non-negative numbers! </p>")))}
    if(input$tau_dist == "TruncCauchy" & (!is.numeric(input$tau_arg1_TC) | !is.numeric(input$tau_arg2_TC) | input$tau_arg1_TC < 0 | input$tau_arg2_TC <= 0)) {return(HTML(("<p> Parameters for &tau; prior must be non-negative numbers! </p>")))}
    if(input$tau_dist == "Exp" & (input$tau_arg_exp <= 0 | !is.numeric(input$tau_arg_exp))) {return(HTML(("<p> &lambda; must be a positive number! </p>")))}
    if(input$tau_dist == "Fixed" & (input$tau_fixed < 0 | !is.numeric(input$tau_fixed))) {return(HTML(("<p> &tau;<sub>0</sub> must be a positive number! </p>")))}
    
    
    return(NULL)
  }
  
  # Error checking for PoS function
  errCheck_PoS = function(use.hist) {
    
    if(!is.numeric(input$n.new) | !input$n.new%%1 == 0 | input$n.new < 1){return("Sample size of treatment group must be a positive integer!")}
    if(input$endpt == "Binary" & (!is.numeric(input$r.new) |  !input$r.new%%1 == 0 | input$r.new < 0 | input$r.new > input$n.new)){return("Number of events within treatment group must be a non-negative integer smaller than the sample size!")}
    if(input$endpt == "Normal" & !is.numeric(input$m.new)) {return("Sample mean within treatment group must be a number!")}
    if(input$endpt == "Poisson" & (!is.numeric(input$y.new.pois) | input$y.new.pois < 0)){return("Total count within treatment group must be a non-negative number!")}
    if(!is.numeric(input$n.trt.target) | input$n.trt.target%%1 != 0 | input$n.trt.target <= 0 | input$n.trt.target <= input$n.new){return("Total sample size must be a positive integer greater than the interim sample size!")}
    
    if(input$samp_bin != "One") {
      if(!is.numeric(input$n.ctrl.target) | input$n.ctrl.target%%1 != 0 | input$n.ctrl.target <= 0 | input$n.ctrl.target <= input$n.ctrl){return("Total sample size must be a positive integer greater than the interim sample size!")}
      if(!input$n.ctrl%%1 == 0 | input$n.ctrl < 1){return("Sample size of control group must be a positive integer!")}
      
      if(input$endpt == "Binary") {
        if(!is.numeric(input$r.ctrl) | !input$r.ctrl%%1 == 0 | input$r.ctrl < 0 | input$r.ctrl > input$r.new){return("Number of events within control group must be a non-negative integer smaller than the sample size!")}
      } else if(input$endpt == "Normal") {
        if(!is.numeric(input$m.ctrl)){return("Sample mean within control group must be a number!")}
      } else{
        if(!is.numeric(input$y.ctrl.pois) | input$y.ctrl.pois < 0) {return("Total count within treatment group must be a non-negative number!")}
      }
    }
    if(input$endpt == "Normal" & (!is.numeric(input$sigma) | input$sigma < 0)) {return("Reference Scale must be a number greater than 0!")}
    
    if(!is.numeric(input$seed) | !input$seed%%1 == 0| input$seed < 1){return("Seed must be a positive integer!")}
    
    if(use.hist) {
      if(!is.numeric(input$wt) | input$wt < 0 | input$wt > 1){return("Weight for non-informative prior must be between 0 and 1!")}
      if(input$wt > 0.7 & input$wt <= 1){
        output$warning.wt = renderText({
          ("Note: It is not recommended for the non-informative prior's weight to be greater than 0.7.")
        })
      }
      if(input$tau_dist == "HalfNormal" & (!is.numeric(input$tau_arg1_HN) | !is.numeric(input$tau_arg2_HN) | input$tau_arg1_HN < 0 | input$tau_arg2_HN <= 0)) {return(HTML(("<p> Parameters for &tau; prior must be non-negative numbers! </p>")))}
      if(input$tau_dist == "TruncNormal" & (!is.numeric(input$tau_arg1_TN) | !is.numeric(input$tau_arg2_TN) | input$tau_arg1_TN < 0 | input$tau_arg2_TN <= 0)) {return(HTML(("<p> Parameters for &tau; prior must be non-negative numbers! </p>")))}
      if(input$tau_dist == "Uniform" & (!is.numeric(input$tau_arg1_Uni) | !is.numeric(input$tau_arg2_Uni) | (input$tau_arg2_Uni <= input$tau_arg1_Uni))) {return(HTML(("<p> Parameters for &tau; prior must be numeric, with a < b! </p>")))}
      if(input$tau_dist == "Gamma" & (!is.numeric(input$tau_arg1_G) | !is.numeric(input$tau_arg2_G) | input$tau_arg1_G <= 0 | input$tau_arg2_G <= 0)) {return(HTML(("<p> Parameters for &tau; prior must be positive numbers! </p>")))}
      if(input$tau_dist == "InvGamma" & (!is.numeric(input$tau_arg1_IG) | !is.numeric(input$tau_arg2_IG) | input$tau_arg1_IG <= 0 | input$tau_arg2_IG <= 0)) {return(HTML(("<p> Parameters for &tau; prior must be positive numbers! </p>")))}
      if(input$tau_dist == "LogNormal" & (!is.numeric(input$tau_arg1_LN) | !is.numeric(input$tau_arg2_LN) | input$tau_arg1_LN < 0 | input$tau_arg2_LN <= 0)) {return(HTML(("<p> Parameters for &tau; prior must be non-negative numbers! </p>")))}
      if(input$tau_dist == "TruncCauchy" & (!is.numeric(input$tau_arg1_TC) | !is.numeric(input$tau_arg2_TC) | input$tau_arg1_TC < 0 | input$tau_arg2_TC <= 0)) {return(HTML(("<p> Parameters for &tau; prior must be non-negative numbers! </p>")))}
      if(input$tau_dist == "Exp" & (input$tau_arg_exp <= 0 | !is.numeric(input$tau_arg_exp))) {return(HTML(("<p> &lambda; must be a positive number! </p>")))}
      if(input$tau_dist == "Fixed" & (input$tau_fixed < 0 | !is.numeric(input$tau_fixed))) {return(HTML(("<p> &tau;<sub>0</sub> must be a positive number! </p>")))}
    }
    if(input$endpt == "Binary" & !is.numeric(input$p0) | input$p0 < 0 | input$p0 > 1){return("<p> Null value for P(event), one-sample trials must be between 0 and 1!")}
    if(!is.numeric(input$diff0)){return("Value for null hypothesis difference in two-sample trials must be a number")}
    if(!is.numeric(input$alpha) | input$alpha < 0 | input$alpha > 1){return("Type I error rate must be between 0 and 1!")}
    if(input$alpha > 0.5 & input$alpha <= 1){
      output$warning.alpha = renderText({
        ("Are you sure you want a Type I error rate that high?")
      })
    }
    if(input$endpt == "Poisson" & input$link == "logit" & input$samp_pois == "Two") {return("Logit link for Poisson data won't give a sensible decision rule!")}
    
    return(NULL)
  }
  
  # Compute gMAP prior
  get.gMAP = function() {
    
    co.data = which_val()$mat
    
    co.data[,2] = as.numeric(co.data[,2])
    co.data[,3] = as.numeric(co.data[,3])
    if(input$endpt == "Poisson") co.data[,4] = as.numeric(co.data[,4])
    
    # Error check historical data
    if(sum(co.data[,1] == "") > 0 | sum(is.na(co.data[,c(2,3)])) > 0) return("Please complete the table of historical trial meta-data.")
    if(sum(co.data[,2] == "") > 0) return("Please complete the table of historical trial meta-data.")
    if(sum(co.data[,3] == "") > 0) return("Please complete the table of historical trial meta-data.")
    if(input$endpt == "Poisson") {
      if(sum(is.na(co.data[,4])) > 0) return("Please complete the table of historical trial meta-data.")
    }
    
    if(length(unique(co.data[,1])) != nrow(co.data)) {
      output$unique_warn = renderText({"Each row should uniquely define one study!"})
    } else{
      output$unique_warn = renderText({""})
    }
    
    if(sum(co.data[,2] <= 0) > 0) return("Your 2nd column should contain control arm sample sizes, which are positive integers.")  
    if(sum(co.data[,2]%%1 != 0) > 0) return("Your 2nd column should contain control arm sample sizes, which are positive integers.")  
    
    if(sum(!is.numeric(co.data[,2])) > 0) return("Your 2nd column should contain numbers!")
    if(sum(co.data[,3] < 0) > 0 & input$endpt == "Binary") return("Your 3rd column should contain # of events in control group, which are non-negative integers.")  
    if(input$endpt == "Binary" & sum(co.data[,3]%%1 != 0) > 0) return("Your 3rd column should contain control arm sample sizes, which are positive integers.")  
    
    if(sum(!is.numeric(co.data[,3])) > 0) return("Your 3rd column should contain numbers!")
    if(input$endpt == "Binary" & (sum(co.data[,2] < co.data[,3]) > 0)) return("Your number of events (r) cannot exceed your sample size (n)!")
    if(input$endpt == "Poisson") {
      if(sum(!is.numeric(co.data[,4])) > 0) return("Your 4th column should contain numbers!")
      if(((sum(co.data[,3] < 0) > 0) | (sum(co.data[,4] <= 0) > 0))) return("Your total counts (y) must be non-negative and your trial durations (t) must be positive!")
    }
    
    if(sum(co.data[,2] >= 100000) > 0) {
      output$samp_size_warn = renderText({"MAP prior is slower to compute for large sample sizes..."})
    } else{
      output$samp_size_warn = renderText({""})
    }
    
    if(input$beta_choice == "Default") {
      if(input$endpt == "Binary") {
        beta.prior = 2 
      } else if(input$endpt == "Normal") {
        beta.prior = 100*input$sigma
      } else{
        beta.prior =  sd(log(co.data[,3] + 0.5 + log(co.data[,2]*co.data[,4])))
      }
    } else{
      beta.prior = cbind(input$beta_mu, input$beta_sigma)
    }
    
    if(input$tau_dist == "HalfNormal") tau.prior = cbind(input$tau_arg1_HN, input$tau_arg2_HN)
    if(input$tau_dist == "TruncNormal") tau.prior = cbind(input$tau_arg1_TN, input$tau_arg2_TN)
    if(input$tau_dist == "Uniform") tau.prior = cbind(input$tau_arg1_Uni, input$tau_arg2_Uni)
    if(input$tau_dist == "Gamma") tau.prior = cbind(input$tau_arg1_G, input$tau_arg2_G)
    if(input$tau_dist == "InvGamma") tau.prior = cbind(input$tau_arg1_IG, input$tau_arg2_IG)
    if(input$tau_dist == "LogNormal") tau.prior = cbind(input$tau_arg1_LN, input$tau_arg2_LN)
    if(input$tau_dist == "TruncCauchy") tau.prior = cbind(input$tau_arg1_TC, input$tau_arg2_TC)
    if(input$tau_dist == "Exp") tau.prior = input$tau_arg_exp
    if(input$tau_dist == "Fixed") tau.prior = input$tau_fixed
    
    if(input$endpt == "Binary") {
      names(co.data) = c("study", "n", "r")
      base.MAP.mc = gMAP(cbind(r, n-r) ~ 1 | study, co.data, family = binomial, 
                         tau.dist = input$tau_dist, tau.prior = tau.prior, beta.prior = beta.prior)
    } else if (input$endpt == "Normal") {
      names(co.data) = c("study", "n", "m")
      m.se = input$sigma / sqrt(co.data$n)
      co.data = cbind(co.data, m.se)
      base.MAP.mc = gMAP(cbind(m, m.se) ~ 1 | study, family = gaussian, 
                         data = co.data, 
                         weights = n, beta.prior = beta.prior,
                         tau.dist = input$tau_dist, tau.prior = tau.prior)
    } else {
      names(co.data) = c("study", "n", "y", "t")
      base.MAP.mc = gMAP(y ~ 1 + offset(log(t*n)) | study, family = poisson, 
                         data = co.data, beta.prior = beta.prior,
                         tau.dist = input$tau_dist, tau.prior = tau.prior)
    }
      return(base.MAP.mc)
  }
  
  # Function to make forest plot.
  makeForest = function() {
    
    if(!input$use_hist) return("Be sure to check the box to use historical meta-data!")
    
    error_check = errCheck_MAP()
    if(!is.null(error_check)) return(error_check)
    
    withProgress(message = "Making plot...", value = 0, {
       base.MAP.mc = get.gMAP()
       }) 
    return(base.MAP.mc) 
  } # close makeForest() function
  
  # Call make forest function triggered by action button
  observeEvent(input$make.forest, {
    output$forest = renderPlot({})
    
    func.output = isolate(makeForest())
    if(class(func.output)[1] %in% c("character", "html")) {
      output$plot_err = renderText({func.output})
    } else{
      output$forest = renderPlot({
        forest_plot(func.output)
      })
      output$plot_err = renderText({""})
    }
    output$forest_plot_addendum = renderUI({
      if(is.null(errCheck_MAP())) {
        HTML("<p> To update the priors for &tau; and &beta;, see the 'Model specification' section of the 'PoS calculation' tab. </p>")
      } else{
        HTML("")
      }
    })
  }, ignoreInit = TRUE)
  
  # Function to compute PoS 
  getPoS = function() {
    error_check = errCheck_PoS(input$use_hist)
    if(!is.null(error_check)) return(error_check)
    
    withProgress(message = "Computing MAP prior...", {

      set.seed(input$seed)
    
      if(input$use_hist) {
        base.MAP.mc = get.gMAP()
        incProgress(amount = 0.5)
        setProgress(message = "Computing parametric approximation...")
      
        base.MAP = automixfit(base.MAP.mc) 

        incProgress(amount = 0.3)
        setProgress(message = "Robustifying prior...")
      
        if(input$endpt == "Binary") {
          robust.mean = 0.5
        } else if(input$endpt == "Normal") {
          robust.mean = ifelse(input$sided == "One", input$mu0, input$diff0)
        } else{
          robust.mean = ifelse(input$sided == "One", input$lambda0, input$diff0.pois)
        }
        MAP.robust = robustify(base.MAP, weight = input$wt, mean = robust.mean)
      }
      
      incProgress(amount = 0.1)
      setProgress(message = "Computing PoS...")
      
      pc = ifelse(input$sided == "One", 1-input$alpha/2, 1-input$alpha)
      
      lower.tail = ifelse(input$sidedness == "Lower", TRUE, FALSE)
      
      if(input$endpt == "Binary") {
        
        treat.prior = mixbeta(c(1, 1, 1))
        
        if(input$samp_bin == "One") {
          decision = decision1S(pc, qc = input$p0, lower.tail)
          interim = postmix(treat.prior, n = input$n.new, r = input$r.new)
          interim.PoS = pos1S(interim, n = input$n.trt.target - input$n.new, decision = decision)
          if(input$use_hist) {
            interim.combined = postmix(MAP.robust, n = input$n.new, r = input$r.new)
          } else{
            interim.combined = interim
          }
          PoS = round(interim.PoS(interim.combined), 3)
        } else {
          decision = decision2S(pc, qc = input$diff0, link = input$link)
          interim.trt = postmix(treat.prior, n = input$n.new, r = input$r.new)
          if(input$use_hist) {
            interim.ctrl = postmix(MAP.robust, n = input$n.ctrl, r = input$r.ctrl)
          } else{
            interim.ctrl = postmix(treat.prior, n = input$n.ctrl, r = input$r.ctrl)
          }
          interim.PoS = pos2S(interim.ctrl, interim.trt, n1 = input$n.ctrl.target - input$n.ctrl,
                              n2 = input$n.trt.target - input$n.new, decision = decision)
          PoS = round(interim.PoS(interim.ctrl, interim.trt), 3)
        }
        
      } else if(input$endpt == "Normal"){
        
        treat.prior = mixnorm(c(1, 0, 100), sigma = input$sigma)

        if(input$samp_norm == "One") {
          decision = decision1S(pc, qc = input$mu0, lower.tail)
          interim = postmix(treat.prior, n = input$n.new, m = input$m.new)
          interim.PoS = pos1S(interim, n = input$n.trt.target - input$n.new, decision = decision)
          if(input$use_hist) {
            interim.combined = postmix(MAP.robust, n = input$n.new, m = input$m.new)
          } else{
            interim.combined = interim
          }
          PoS = round(interim.PoS(interim.combined), 3)
        } else {
          decision = decision2S(pc, qc = input$diff0)
          if(input$use_hist) {
            interim.ctrl = postmix(MAP.robust, n = input$n.ctrl, m = input$m.ctrl)
          } else{
            interim.ctrl = postmix(treat.prior, n = input$n.ctrl, m = input$m.ctrl)
          }
          interim.trt = postmix(treat.prior, n = input$n.new, m = input$m.new)
          interim.PoS = pos2S(interim.ctrl, interim.trt, n1 = input$n.ctrl.target - input$n.ctrl,
                              n2 =  input$n.trt.target - input$n.new, decision = decision)
          PoS = round(interim.PoS(interim.ctrl, interim.trt), 3)
        }
        
        
      } else {
        
        treat.prior = mixgamma(c(1, 0.001, 0.001))
        
        if(input$samp_pois == "One") {
          decision = decision1S(pc, qc = input$lambda0, lower.tail)
          interim = postmix(treat.prior, n = input$n.new.pois, m = input$y.new.pois/input$n.new.pois)
          interim.PoS = pos1S(interim, input$n.trt.target - input$n.new, decision)
          if(input$use_hist) {
            interim.combined = postmix(MAP.robust, m = input$y.new.pois/input$n.new.pois, n = input$n.new.pois)
          } else{
            interim.combined = interim
          }
          PoS = round(interim.PoS(interim.combined), 3)
        } else {
          decision = decision2S(pc, qc = input$diff0.pois, link = input$link)
          interim.trt = postmix(treat.prior, n = input$n.new.pois, m = input$y.new.pois/input$n.new.pois)
          if(input$use_hist) {
            interim.ctrl = postmix(MAP.robust, n = input$n.ctrl.pois, m = input$y.ctrl.pois/input$n.ctrl.pois)
          } else{
            interim.ctrl = postmix(treat.prior, n = input$n.ctrl.pois, m = input$y.ctrl.pois/input$n.ctrl.pois)
          }
          interim.PoS = pos2S(interim.ctrl, interim.trt, input$n.ctrl.target - input$n.ctrl,
                              input$n.trt.target - input$n.new, decision = decision)
          PoS = round(interim.PoS(interim.ctrl, interim.trt), 3)
        }
      }

      return(PoS)
    })
  }
  
  # Compute PoS triggered by action button.
  observeEvent(input$compute.pos, {
    output$results = renderUI({
    func.output = isolate(getPoS())
    if(class(func.output)[1] %in% c("html", "character")) {
      func.output
    } else{
       paste0("The probability of trial success is ", func.output, ".")
    }
  })
  }, ignoreInit = TRUE)
  
  URL0 = a("Click here for a walkthrough of this Shiny app (Chrome or Firefox only).", href = "https://github.com/JamesNormington/PoS_Using_RBesT/blob/master/PoS%20Shiny%20App%20Walkthrough%2C%20v1.pdf")
  output$link0 = renderUI({
    tagList(URL0)
  }) 
  
  URL = a("Click here for a walkthrough of the methodology and code (Chrome or Firefox only).", href = "https://github.com/JamesNormington/PoS_Using_RBesT/blob/master/PoS%20Methodology%20and%20Code%20Walkthrough%2C%20v1.pdf")
  output$link = renderUI({
    tagList(URL)
  }) 
  
  URL2 = a("Click here for Sebastian Weber's 'Probability of Success with Co-Data' vignette.", href = "https://cran.r-project.org/web/packages/RBesT/vignettes/PoS_codata.html")
  output$link2 = renderUI({
    tagList(URL2)
  }) 
  
  URL3 = a("Click here for RBesT documentation on CRAN.", href = "https://cran.r-project.org/web/packages/RBesT/RBesT.pdf")
  output$link3 = renderUI({
    tagList(URL3)
  }) 
  
} # close server() function

shinyApp(ui, server)

