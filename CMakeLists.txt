cmake_minimum_required(VERSION 2.8)

project(MilestoneFY13 NONE)

include(UseLATEX.cmake)

set(tex_sources
  Abstract.tex
  ExecutiveSummary.tex
  Acknowledgements.tex
  Background.tex
  OfficialMilestone.tex
  MilestoneScope.tex
  Catalyst.tex
  Nessie.tex
  UseCase.tex
  Results.tex
  Conclusion.tex
  Distribution.tex
  )

add_latex_document(MilestoneFY13.tex
  INPUTS ${tex_sources}
  BIBFILES MilestoneFY13.bib oldfield.bib
  IMAGE_DIRS SANDimages figures Summary
  DEFAULT_PDF
  )
