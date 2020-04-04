library(rmarkdown)

rmarkdown::render('covid_canada_dash.Rmd',
                  output_file = paste('report-', Sys.Date(),'.html', sep=''))