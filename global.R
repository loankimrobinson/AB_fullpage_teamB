library(shiny)
library(fullPage)




card <- function(.img, .name, .title, .email) {
  # If you want circle replace <img src="', .img, '" style="width:80%; padding-top:20px;">  to 
  # <img src="', .img, '" style="width:80%; padding-top:20px; border-radius:50% "> 
  HTML(
    paste0(
      '<div class="card">
      <img src="', .img, '" style="width:20%; padding-top:20px;"> 
      <div class="container">
      <h4 style="font-weight: bold" ><i>', .name, '</i></h4>
      <hr>
      <h6><i>', .title, '</i></h6>
      <p><a href="mailto:',.email,'?subject=Mail from Our Team">',.email,'</a></p>
      <p style = "border: none;outline: 0;display: inline-block; padding: 8px;color: white;background-color: grey;text-align: center;cursor: pointer; width: 100%;"><a style = "color: white" target="_blank" href="mailto:',.email,'?subject=Mail from Our Team">Contact</a></p>
      </div>
      </div>')
  )
}

img.src <- c("loan.jpg","loan.jpg","loan.jpg","loan.jpg")
name <- c("Loan K Robinson", "Loan K Robinson", "Loan K Robinson", "Loan K Robinson")
title <- c("IT - Scientific Application Specialist", "IT - Scientific Application Specialist", "IT - Scientific Application Specialist", "IT - Scientific Application Specialist")
email <- c("loan.robinson@bms.com","loan.robinson@bms.com","loan.robinson@bms.com","loan.robinson@bms.com")

