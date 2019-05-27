target=reportcards

build:
	echo "devtools::document(roclets = c('rd','collate','namespace','vignette'))" | R --no-save --no-restore
	echo "rmarkdown::render('README.Rmd', output_format='github_document')" | R --no-save --no-restore
	cd ..; R CMD INSTALL --preclean --no-multiarch --with-keep.source $(target)
