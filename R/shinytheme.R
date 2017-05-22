#' Return the URL for a Shiny theme
#'
#' The result of this function should be used as the \code{theme} argument for
#' \code{\link[shiny]{bootstrapPage}}, \code{\link[shiny]{fluidPage}},
#' \code{\link[shiny]{navbarPage}}, or \code{\link[shiny]{fixedPage}}.
#'
#'
#' @param theme Name of a theme.
#'
#' @examples
#' \dontrun{
#' shinyApp(
#'   ui = fluidPage(theme = shinytheme("united"),
#'     ...
#'   ),
#'   server = function(input, output) { }
#' )
#' }
#' @seealso The main \link{shinythemes} page for information about available
#'   themes and more detailed examples.
#' @export
shinytheme <- function(theme = NULL) {
  # Check that theme exists
  if (is.null(theme) || !theme %in% allThemes()) {
    stop(theme, " is not an available theme. Valid themes are: ",
      paste(allThemes(), collapse = ", "), ".")
  }

  paste0("shinythemes/css/", theme, ".min.css")
}


allThemes <- function() {
  themes <- dir(system.file("shinythemes/css", package = "shinythemes"),
                "*.min.css")
  sub(".min.css", "", themes)
}

#' Slightly modified version of the theme setter functionaliity from
#' the original forked package. Same base code, but without the forced style for
#' the chooser, in order to allow people to inline code like any other UI gadget.
#' @export
themeSelector <- function() {
  div(
    selectInput("shinytheme-selector", NULL,
          c("default", allThemes()),
          selectize = FALSE
        ),
    tags$script(
"$('#shinytheme-selector')
  .on('change', function(el) {
    var allThemes = $(this).find('option').map(function() {
      if ($(this).val() === 'default')
        return 'bootstrap';
      else {
        return $(this).val();
      }
    });

    // Find the current theme
    var curTheme = el.target.value;
    Cookies.set('theme',curTheme);
    if (curTheme === 'default') {
      curTheme = 'bootstrap';
      curThemePath = 'shared/bootstrap/css/bootstrap.min.css';
    } else {
      curThemePath = 'shinythemes/css/' + curTheme + '.min.css';
    }

    // Find the <link> element with that has the bootstrap.css
    var $link = $('link').filter(function() {
      var theme = $(this).attr('href');
      theme = theme.replace(/^.*\\//, '').replace(/(\\.min)?\\.css$/, '');
      return $.inArray(theme, allThemes) !== -1;
    });

    // Set it to the correct path
    $link.attr('href', curThemePath);
  });"
    )
  )
}
#' An helper to manage cookies easily. Courtesy of the project
#' JavaScript Cookie v2.1.4
#' https://github.com/js-cookie/js-cookie
#' Both this function and the other are needed to work, for now.
#' Include in the head of the shiny document in order to allow
#' the theme system to work.
#' @export
cookieHelper <- function(){
  shiny::tags$head(shiny::tags$script(src = "shinythemes/js/js.cookie.js"))
}
#' Cookie saver manager. Include this on the head of the document to allow to load
#' and save the last known state of the theme; after cookieHelper, which is a dependency
#' @export
cookieSaver <- function(){
  shiny::tags$head(shiny::tags$script("
$(document).on('shiny:connected', function(event) {
                                      selectedTheme = Cookies.get('theme');
                                      if (!(selectedTheme == undefined)){
                                      $('#shinytheme-selector').val(selectedTheme);
                                      $('#shinytheme-selector').change();
                                      }});
                                      "))
}
