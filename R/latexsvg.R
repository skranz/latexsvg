example.latexsvg = function() {
  setwd("D:/libraries/latexsvg")
  res = latexsvg(file="latex.svg")

  svg = paste0(readLines("latex_latex.svg"), collapse="\n")
  app = eventsApp()
  app$ui = bootstrapPage(
    h4("An SVG"),
    HTML(svg)
  )
  viewApp(app)
}

#' Takes the code of an svg file that has <text> blocks with latex math
#' E.g. <text>\\(x^2\\)</text>
#' Renders the latex via MathJax and inlines it into the svg.
#' The function briefly starts a shinyapp since the transformation needs
#' MathJax to be run in the browser. Will work in the RStudio viewer
#' and in Firefox. Does not seem to work in Chrome.
#'
#' The javascript code that does the actually work is a minimally adapted
#' version of the code by Jason Sachs. See the following references:
#'
#' https://www.embeddedrelated.com/showarticle/599.php
#' https://bitbucket.org/jason_s/svg_mathjax
#'
#' @param svg the code of svg as text
#' @param file a file name of the svg if no code is given
#' @param outfile if not null the created svg will be written to this file
#' @return The code of the generated svg file
#' @export
latexsvg = function(svg=paste0(readLines(file),collapse="\n"), file=NULL, outfile=if (!is.null(file)) paste0(tools::file_path_sans_ext(file),"_latex.svg")) {
  restore.point("latexsvg")
  svg = paste0(svg, collapse="\n")
  app = eventsApp(add.events = list())
  dir = system.file("www",package="latexsvg")
  shiny::addResourcePath("latexsvg",dir)
  app$ui = bootstrapPage(
    tags$script(type="text/javascript",
      src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_SVG"
    ),
    tags$script(src="latexsvg/svg_mathjax.js"),
    tags$script(type="text/javascript",
        "new Svg_MathJax().install();"
    ),
    h4("Render Latex in SVG via Mathjax..."),
    div(id="svgdiv",
      HTML(svg)
    )
  )

  def.id = paste0(sample(c(letters,LETTERS,0:9),10, replace = TRUE), collapse="")
  js = paste0('
$(document).on("EndSVGLatex",function(e) {
  // Add glyph directly to svg code
  var defs = $("#MathJax_SVG_glyphs").clone().wrapAll("<div>").attr("id","SVG_glyphs_',def.id,'").parent().html();
  var svg = $("#svgdiv svg").clone();
  svg.prepend(defs);
  var svgHtml = $("<div>").append(svg).html()

  Shiny.onInputChange("saveSVG", {eventId:"saveSVG",id: "saveSVG", value: $(this).val(),  data: $(this).data(),nonce: Math.random(), svg: svgHtml});
});
  ')

  save.svg.handler =  function(...) {
    args = list(...)
    restore.point("save.svg.handler")
    svg = args$svg

    if (!is.null(outfile)) {
      writeLines(svg,outfile)
    }
    ret.svg <<- svg
    cat("\nsvg generated: quit app...")
    stopApp()
  }

  eventHandler(eventId = "saveSVG",id="saveSVG",fun=save.svg.handler,jscript = js)

  viewApp(app)
  return(ret.svg)
}

#' A function that converts a graphic in PDF format to an svg file
#' and saves the svg file in the same directory as the pdf file
#' Needs inkscape and the path to it binary
#' @param file the pdf file
#' @param svg.file the output svg file; by default the same name as the pdf file but with extension .svg.
#' @param inkscape.bin path and file name of the inkscape binary
#' @export
pdf2svg = function(file, inkscape.bin,svg.file = paste0(tools::file_path_sans_ext(file),".svg")) {
  com = paste0(inkscape.bin," ",file," --export-plain-svg=",svg.file)
  system(com)
}
