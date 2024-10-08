## Build a single Rmd file

.DEFAULT_GOAL := render

# all:       clean_all pdf html rtim
render:    pdf
pdf:       p1 d1  upload
html:      h1
rtim:      r1

## use a script to upload all pdfs
upload:
	./upload.sh

TARGET := Clear_sky_id_Reno-Hansen_apply_v14.2
RMD    := $(TARGET).R
PDF    := $(TARGET).pdf
SLIDY  := $(TARGET).html
RUNT   := ./REPORTS/RUNTIME/$(TARGET).pdf

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


TARGET := Clear_sky_id_Reno-Hansen_description_v14.2
RMD    := $(TARGET).Rmd
PDF    := $(TARGET).pdf
SLIDY  := $(TARGET).html
RUNT   := ./REPORTS/RUNTIME/$(TARGET).pdf

d1: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"






TARGET := Clear_sky_id_Reno-Hansen_apply_v15
RMD    := $(TARGET).R
PDF    := $(TARGET).pdf
SLIDY  := $(TARGET).html
RUNT   := ./REPORTS/RUNTIME/$(TARGET).pdf

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
RUNT   := ./REPORTS/RUNTIME/$(TARGET).pdf

p3: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"

h3: $(SLIDY)
$(SLIDY): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::render('$?', output_format='rmarkdown::html_document', output_file='$@')"

r3: $(RUNT)
$(RUNT): $(RMD)
	-Rscript $?




TARGET := Clear_sky_id_Reno-Hansen_apply_v13.1
RMD    := $(TARGET).R
PDF    := $(TARGET).pdf
SLIDY  := $(TARGET).html
RUNT   := ./REPORTS/RUNTIME/$(TARGET).pdf

p4: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"

h4: $(SLIDY)
$(SLIDY): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::render('$?', output_format='rmarkdown::html_document', output_file='$@')"

r4: $(RUNT)
$(RUNT): $(RMD)
	-Rscript $?



