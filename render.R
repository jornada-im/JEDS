# Render the markdown files to html and move to html directory

rmd_list <- list.files(pattern='\\.Rmd$')
for (file in rmd_list){
	print(paste('Rendering', file, '...'))
	rmarkdown::render(file)
	html_file <- paste0(str_split(file, '\\.')[[1]][1], '.html')
	print(paste0('Moving rendered file to ./html/', html_file))
	file.rename(html_file, paste0('./html/', html_file))
}
