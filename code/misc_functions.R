#Misc functions

#HELPER FUNCTIONS
#reduce need to type glimpse every time...
g <- function(x) tidyverse::glimpse(x)
v <- function(x) View(x)

#Wrap grepl to do tidier version of this when e.g. filtering for terms
#gq = "grepl quick!"
qg <- function(...) grepl(..., ignore.case = T)
