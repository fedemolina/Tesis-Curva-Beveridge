---
author: 'Federico Molina Magne'
date: 'May 20xx'
institution: 'Universidad de la República'
division: 'Facultad de Ciencias Económicas y de Administración'
advisor: 'Tutor Rodrigo Ceni'
# If you have more two advisors, un-silence line 7
#altadvisor: 'Your Other Advisor'
department: 'Economía'
degree: 'Maestría en Economía'
title: 'Tesis de Maestría'
knit: "bookdown::render_book"
site: bookdown::bookdown_site
output: 
  thesisdown::thesis_pdf: default
  # thesisdown::thesis_gitbook: default
#  thesisdown::thesis_word: default
#  thesisdown::thesis_epub: default
# If you are creating a PDF you'll need to write your preliminary content (e.g., abstract, acknowledgements) here or
# use code similar to line 22-23 for the .RMD files. If you are NOT producing a PDF, you can delete or silence lines 21-32 in this YAML header.
abstract: |
  The preface pretty much says it all.  \par
  
  Second paragraph of abstract starts here.
# If you'd rather include the preliminary content in files instead of inline
# like below, use a command like that for the abstract above.  Note that a tab is 
# needed on the line after the `|`.
agradecimientos: |
  I want to thank a few people.
dedicacion: |
  You can have a dedication here if you wish. 
preface: |
  This is an example of a thesis setup to use the reed thesis document class
  (for LaTeX) and the R bookdown package, in general.
# Specify the location of the bibliography below
bibliography: bib/thesis.bib
# Download your specific csl file and refer to it in the line below.
csl: csl/apa.csl
lot: true
lof: true
# If you prefer blank lines between paragraphs, un-silence lines  40-41 (this requires package tikz)
header-includes:
#- \usepackage{tikz}
- \usepackage[spanish]{babel}
---

<!--
Above is the YAML (YAML Ain't Markup Language) header that includes a lot of metadata used to produce the document.  Be careful with spacing in this header!

If you'd prefer to not include a Dedication, for example, simply delete the section entirely, or silence (add #) them. 

If you have other LaTeX packages you would like to include, delete the # before header-includes and list the packages after hyphens on new lines.

If you'd like to include a comment that won't be produced in your resulting file enclose it in a block like this.

If you receive a duplicate label error after knitting, make sure to delete the index.Rmd file and then knit again.
-->



<!-- On ordering the chapter files:
There are two options:
1. Name your chapter files in the order in which you want them to appear (e.g., 01-Inro, 02-Data, 03-Conclusions). 
2. Otherwise, you can specify the order in which they appear in the _bookdown.yml (for PDF only).

Do not include 00(two-hyphens)prelim.Rmd and 00-abstract.Rmd in the YAML file--they are handled in the YAML above differently for the PDF version.
-->

<!-- The {.unnumbered} option here means that the introduction will be "Chapter 0." You can also use {-} for no numbers
on chapters.
-->

# Introduction {.unnumbered}

