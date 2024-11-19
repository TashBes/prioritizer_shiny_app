

library(shiny)
library(vroom)
library(tidyverse)

dir.create("neiss")
#> Warning in dir.create("neiss"): 'neiss' already exists
download <- function(name) {
  url <- "https://raw.github.com/hadley/mastering-shiny/main/neiss/"
  download.file(paste0(url, name), paste0("neiss/", name), quiet = TRUE)
}
download("injuries.tsv.gz")
download("population.tsv")
download("products.tsv")


injuries <- vroom::vroom("neiss/injuries.tsv.gz")
injuries


products <- vroom::vroom("neiss/products.tsv")
products


population <- vroom::vroom("neiss/population.tsv")
population



selected <- injuries %>% filter(prod_code == 649)
nrow(selected)
#> [1] 2993

selected %>% count(location, wt = weight, sort = TRUE)
#> # A tibble: 6 × 2
#>   location                         n
#>   <chr>                        <dbl>
#> 1 Home                       99603. 
#> 2 Other Public Property      18663. 
#> 3 Unknown                    16267. 
#> 4 School                       659. 
#> 5 Street Or Highway             16.2
#> 6 Sports Or Recreation Place    14.8

selected %>% count(body_part, wt = weight, sort = TRUE)
#> # A tibble: 24 × 2
#>   body_part        n
#>   <chr>        <dbl>
#> 1 Head        31370.
#> 2 Lower Trunk 26855.
#> 3 Face        13016.
#> 4 Upper Trunk 12508.
#> 5 Knee         6968.
#> 6 N.S./Unk     6741.
#> # ℹ 18 more rows

selected %>% count(diag, wt = weight, sort = TRUE)
#> # A tibble: 20 × 2
#>   diag                       n
#>   <chr>                  <dbl>
#> 1 Other Or Not Stated   32897.
#> 2 Contusion Or Abrasion 22493.
#> 3 Inter Organ Injury    21525.
#> 4 Fracture              21497.
#> 5 Laceration            18734.
#> 6 Strain, Sprain         7609.
#> # ℹ 14 more rows


summary <- selected %>% 
  count(age, sex, wt = weight)
summary
#> # A tibble: 208 × 3
#>     age sex         n
#>   <dbl> <chr>   <dbl>
#> 1     0 female   4.76
#> 2     0 male    14.3 
#> 3     1 female 253.  
#> 4     1 male   231.  
#> 5     2 female 438.  
#> 6     2 male   632.  
#> # ℹ 202 more rows

summary %>% 
  ggplot(aes(age, n, colour = sex)) + 
  geom_line() + 
  labs(y = "Estimated number of injuries")


summary <- selected %>% 
  count(age, sex, wt = weight) %>% 
  left_join(population, by = c("age", "sex")) %>% 
  mutate(rate = n / population * 1e4)

summary
#> # A tibble: 208 × 5
#>     age sex         n population   rate
#>   <dbl> <chr>   <dbl>      <dbl>  <dbl>
#> 1     0 female   4.76    1924145 0.0247
#> 2     0 male    14.3     2015150 0.0708
#> 3     1 female 253.      1943534 1.30  
#> 4     1 male   231.      2031718 1.14  
#> 5     2 female 438.      1965150 2.23  
#> 6     2 male   632.      2056625 3.07  
#> # ℹ 202 more rows



summary %>% 
  ggplot(aes(age, rate, colour = sex)) + 
  geom_line(na.rm = TRUE) + 
  labs(y = "Injuries per 10,000 people")

selected %>% 
  sample_n(10) %>% 
  pull(narrative)
#>  [1] "89YOF GOT UP TO USE BATHROOM, REPORTS BEING DIZZY, FELL AND HIT HEAD ONTOILET. DX: LACERATION TO SCALP"                             
#>  [2] "90YOM WITH LACERATION TO FOREHEAD WITH BLEEDING AFTER FALLING WHILE STANDING FROM TOILET. ON WARFARIN. PT 83, INR 7.2."             
#>  [3] "16YOF STOOD UP QUICKLY IN RESTROOM AND BLACKED OUT AND HIT HEAD ON TOILET NO LOC DX HEAD INJURY; SYNCOPE; HYPOGLYCEMIA /"           
#>  [4] "95YOF LIVES AT NH FELL OFF OF TOILET HAS NO PAIN FALL"                                                                              
#>  [5] "85YOF FELL OFF THE TOILET AT THE NURSING HOME HIT HEAD LACERATION TO SCALP ADMITTED FOR SEPSIS"                                     
#>  [6] "59YOF W/ PMX OF CANCER ATTEMPED TO SIT ON TOILET WHEN SHE FELT HER RIGHT FEMUR SNAP DX: FEMUR FRACTURE"                             
#>  [7] "73YOF WAS SITTING ON TOILET WHEN SHE 'SLUMPED\" OVER, HAD CONVULSIONS, &WAS FOAMING AT THE MOUTH DX: HIP PAIN, ALTERED MENTAL STATE"
#>  [8] "2YOM LAC F'HD- FELL OFF TOILET ONTO SIDE OF TUB"                                                                                    
#>  [9] "83 YOF FELL WHILE HUSBAND WAS HELPING HER TRANSFER OFF OF TOILET.DX:  CERVICAL STRAIN, DEMENTIA, POSS UTI."                         
#> [10] "61 YOF WAS SITTING ON COMMODE AND SLIPPED OFF.DX:  LIP LAC 2 CM."



output$body_part <- renderPrint({
  dataset <- get(input$dataset, "package:datasets")
  summary(dataset)
})

output$location <- renderPrint({
  dataset <- get(input$dataset, "package:datasets")
  summary(dataset)
})

output$age_sex <- renderPrint({
  dataset <- get(input$dataset, "package:datasets")
  summary(dataset)
})


# install.packages('C:\\gurobi1200\\win64\\R\\gurobi_12.0-0.zip', repos=NULL)
# 
# install.packages('slam')

