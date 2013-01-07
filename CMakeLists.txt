cmake_minimum_required(VERSION 2.8)

project(MilestoneFY13 NONE)

include(UseLATEX.cmake)

set(tex_sources
  Abstract.tex
  ExecutiveSummary.tex
  Background.tex
  OfficialMilestone.tex
  Catalyst.tex
  Nessie.tex
  UseCase.tex
  Results.tex
  Conclusion.tex
  Distribution.tex
  NewChallenges.tex
  )

set(images
  # images/TimingPlot.png
  )

add_latex_document(MilestoneFY13.tex
  INPUTS ${tex_sources}
  IMAGES ${images}
  BIBFILES MilestoneFY13.bib
  IMAGE_DIRS SANDimages figures
  DEFAULT_PDF
  )
