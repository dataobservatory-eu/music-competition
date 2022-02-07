# README: The Relevant Market for Music Streaming: Market Definition and Measurement Challanges

Our dynamic [The Relevant Market for Music Streaming: Market Definition and Measurement Challanges](https://music-competition.dataobservatory.eu/) document is a versioned work-in-progress publication, available from the Zenodo open science repository. It is built upon the actual application of competition law in non-UK jurisdictions, and it is intended for further research and publication. After reviewing some methods to empirically collect data about quantities, prices, market shares and market power in the music markets, we present some ideas on the definition of music markets.

## Folders

**root** - The two articles, `.bib` bibliography files, and `yml` files for markdown conversions, plus reproducbile `docx`, `pdf`, `epub` versions. Work in the `Rmd` markdown files. If you do not write R code, just ignore the R code chunks, and use it as a clean markdown text.

**bib** - please save here individual BibTex entries.  The consolidated entries will should be placed in one of the main `.bib` files in the root folder. The pandoc / knitr / RStudio workflow can have hickuups with bib files, so try to save individual files first in `bib/xyz.bib`

**not_included** - user's scrap directory, excluded by `.gitignore`.  Please put your non-synchronized scaps and code doodles here.

**data-raw** - data as downloaded, received, as a starting point of our reproducible work.

**R** - R code written for the publication.  It is better to write stand-alone R codes, and put final 'chunks' into the `.Rmd` files.

**data** - Final data outputs that will be placed in the articles.

