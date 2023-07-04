rmarkdown::render("WorldAnalysis.Rmd",
                  output_format = "github_document",  
                  output_dir = "WorldAnalysis" ,
                  output_options = list(html_preview = FALSE))