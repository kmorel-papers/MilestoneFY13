cmake_minimum_required(VERSION 2.8)

project(MilestoneFY13 NONE)

include(UseLATEX.cmake)

set(tex_sources
  )

set(images
  # images/TimingPlot.png
  )

add_latex_document(MilestoneFY13.tex
  INPUTS ${tex_sources}
  IMAGES ${images}
  IMAGE_DIRS SANDimages
  )
