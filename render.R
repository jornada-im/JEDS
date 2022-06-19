# Render the markdown files to html and move to html directory

rmarkdown::render("JEST-setup.Rmd")
file.rename('JEST-setup.html', 'html/JEST-setup.html')
