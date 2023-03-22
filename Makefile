## Build a single Rmd file

.DEFAULT_GOAL := render

# all:       clean_all pdf html rtim
render:    pdf
pdf:       p1 p2 upload
html:      h1 h2
rtim:      r1 r2
# clean_all: clean_cache clean_data clean_pdfs

## use a script to upload all pdfs
upload:
	./upload.sh


TARGET := Clear_sky_id_Reno-Hansen_apply_v14.1
RMD    := $(TARGET).R
PDF    := $(TARGET).pdf
SLIDY  := $(TARGET).html
RUNT   := ./runtime/$(TARGET).pdf

p1: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"

h1: $(SLIDY)
$(SLIDY): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::render('$?', output_format='rmarkdown::html_document', output_file='$@')"

r1: $(RUNT)
$(RUNT): $(RMD)
	-Rscript $?



TARGET := Clear_sky_id_Reno-Hansen_apply_v14
RMD    := $(TARGET).R
PDF    := $(TARGET).pdf
SLIDY  := $(TARGET).html
RUNT   := ./runtime/$(TARGET).pdf

p2: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"

h2: $(SLIDY)
$(SLIDY): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::render('$?', output_format='rmarkdown::html_document', output_file='$@')"

r2: $(RUNT)
$(RUNT): $(RMD)
	-Rscript $?










