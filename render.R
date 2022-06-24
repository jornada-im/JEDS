# Render the markdown files to html and move to html directory
library(tidyverse)

rmd_list <- list.files('episodes', pattern='\\.Rmd$', full.names=T)
for (file in rmd_list){
	print(paste('Rendering', file, '...'))
	rmarkdown::render(file)
	html_file <- paste0(str_split(file, '\\.')[[1]][1], '.html')
	html_fname <- str_split(html_file, '\\/')[[1]][2]
	print(paste0('Moving rendered file to ../html/', html_fname))
	file.rename(html_file, paste0('html/', html_fname))
}
