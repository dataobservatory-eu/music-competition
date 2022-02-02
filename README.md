# README: An Empirical Analysis of Music Streaming Volumes, Revenues and Their Market Conditions

Do not use or cite this report yet.  It is a preliminary document based on [An Empirical Analysis of Music Streaming Revenues and Their Distribution](https://reprex.nl/publication/mce_empirical_streaming_2021/) and will go through a very thorough re-editing, leaving irrelevant parts out, and adding more relevant parts from a competition economics and competition law point of view.

The new working paper **An Empirical Analysis of Music Streaming Volumes, Revenues and Their Market Conditions** in making is placed on [music-competition.dataobservatory.eu](https://music-competition.dataobservatory.eu/)

## Folders

**root** - The two articles, `.bib` bibliography files, and `yml` files for markdown conversions, plus reproducbile `docx`, `pdf`, `epub` versions. Work in the `Rmd` markdown files. If you do not write R code, just ignore the R code chunks, and use it as a clean markdown text.

**bib** - please save here individual BibTex entries.  The consolidated entries will should be placed in one of the main `.bib` files in the root folder. The pandoc / knitr / RStudio workflow can have hickuups with bib files, so try to save individual files first in `bib/xyz.bib`

**not_included** - user's scrap directory, excluded by `.gitignore`.  Please put your non-synchronized scaps and code doodles here.

**data-raw** - data as downloaded, received, as a starting point of our reproducible work.

**R** - R code written for the publication.  It is better to write stand-alone R codes, and put final 'chunks' into the `.Rmd` files.

**data** - Final data outputs that will be placed in the articles.

