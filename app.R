
library(shiny)
library(tidyverse)



#rsconnect::writeManifest() #Generate a manifest.json file with the rsconnect library.
#rsconnect::writeManifest(contentCategory = "site")
#renv::dependencies() #查看各文件的依赖包
ui <- fluidPage(
  titlePanel("中心极限定理"),
  sidebarLayout(
    sidebarPanel(
      selectInput('seldist', '分布类型', choices = c('正态','均匀','二项式','泊松','指数'), selected = '正态'),
      sliderInput('nrandom','随机变量数', min = 1, max = 100, value = 5, step = 5),
      sliderInput('nsample','样本数', min = 500, max = 10000, value = 1000, step = 500),
      sliderInput('nbin','条形分组数', min = 10, max = 100, value = 10, step = 5),
    ),
    mainPanel(
      plotOutput('plothist')
    ),
    position = 'right' #用于确定slidebarPanel的位置
  ),
  
  h4("文字靠中间的常用方法"),br(),
  h5("方法1:用偏移量"),
  fluidRow(
    column(width = 12, offset = 3,
           h2('体会响应表达式的意义'),
           strong('如果改变条形分组数，恢复到原值时图形也会保持不变，而其他变量不会'),br(),
           strong('这说明：表达式保存首次运行结果，在数值变动时再进行计算')
    )
  ),
  
  h5("方法2:设置居中"),
  fluidRow(
    align = "center",
    h2("体会响应表达式的意义"),
    p("如果改变条形分组数，恢复到原值时图形也会保持不变，而其他变量不会"),
    "这说明：",strong('表达式保存首次运行结果，在数值变动时再进行计算')
  ),
  
  br(),
  br(),
  
  fluidRow(
    column(2, '2个宽度', br(),br(),
           checkboxGroupInput('x','多选框',choices = c(5,2,3,4),selected = 5),
           actionButton('xx','行为按钮'),br(),br(),
           textInput('xxx', '文本输入框', value = '在此输入'),br(),
           numericInput('xxxx', '数值输入框', value = 1, min = 1, max = 100, step = 10)
           
           #submitButton('提交按钮') #该类按钮没有标签,设置后若页面更新，需要点击后才生效
    ),
    column(3, '3个宽度', br(),br(),
           checkboxInput('y','单选框',value = F),
           radioButtons('z','单选按钮',choices = letters[1:3], selected = 'c'),
           dateRangeInput('w','日期区间',start = '2025-1-1',end = '2035-12-31')
    ),
    column(7, '7个宽度',br(),br(),
           tabsetPanel(
             id = "t",
             type = "pills", #"tabs",
             tabPanel("Plot", plotOutput("plot")),
             tabPanel("Summary", verbatimTextOutput("summary")),
             tabPanel("Table", DT::dataTableOutput("table") # tableOutput("table")
             )
           )
    )
  )
)

server <- function(input, output){
  active_data <- reactive({
    n <- input$nrandom
    m <- input$nsample
    totalnum <- switch(
      input$seldist,
      '正态' = rnorm(m * n),
      '均匀' = runif(m * n),
      '二项式' = rbinom(m * n, size = 10, prob = 0.2),
      '泊松' = rpois(m * n, lambda = 3),
      '指数' = rexp(m * n, rate = 2)
    )
    data <- data.frame(x = rowMeans(matrix(data = totalnum, ncol = n)))
  })
  
  # output$plothist <- renderPlot(ggplot(data = active_data(), aes(x = x)) +
  #                                   geom_histogram(bins = input$nbin, alpha = 0.2, color = 'darkgreen')
  # )
  
  # 追加密度拟合线
  # output$plothist <- renderPlot(ggplot(data = active_data(), aes(x = x)) +
  #                                 geom_histogram(aes(y = after_stat(density)),
  #                                                bins = input$nbin, alpha = 0.2, color = 'darkgreen') +
  #                                 geom_density(color = 'brown')
  # )
  
  ## 追加指定的分布线
  output$plothist <- renderPlot(ggplot(data = active_data(), aes(x = x)) +
                                  geom_histogram(aes(y = after_stat(density)),
                                                 bins = input$nbin, alpha = 0.2, color = 'darkgreen') +
                                  geom_density(color = 'brown') +
                                  stat_function(fun = dnorm, #dgamma
                                                args = list(mean = mean(active_data()$x),
                                                            sd = sd(active_data()$x)),
                                                color = 'blue',linetype = 2, linewidth = 0.8)
  )
  
  #-------演示用追加————————
  output$plot <- renderPlot(mosaicplot(Titanic))
  output$summary <- renderText(summary(cars))
  output$table <- DT::renderDataTable(mtcars |> select(1:5)) #renderTable(mtcars)
  #-------------------------
}

shinyApp(ui = ui, server = server)

