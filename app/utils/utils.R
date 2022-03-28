save_to <- function(x, v, fun) {
  var <- substitute(v)
  eval(bquote(.(var) <- .(fun(x))), envir = globalenv())
  x
}

customInfoBox <- function (title, tab = NULL, value = NULL, subtitle = NULL, icon = shiny::icon("bar-chart"), color = "aqua", width = 4, href = NULL, fill = FALSE) {
  colorClass <- paste0("bg-", color)
  boxContent <- div(class = "info-box", class = if (fill) colorClass, 
                    onclick = if(!is.null(tab)) paste0("$('.sidebar a')).filter(function() { return ($(this).attr('data-value') == ", tab, ")}).click()"),
                    span(class = "info-box-icon", class = if (!fill) colorClass, icon),  
                    div(class = "info-box-content", 
                        span(class = "info-box-text", title), 
                        if (!is.null(value)) span(class = "info-box-number", value), 
                        if (!is.null(subtitle)) p(subtitle)
                    )
  )
  if (!is.null(href)) boxContent <- a(href = href, boxContent)
  div(class = if (!is.null(width)) paste0("col-sm-", width), boxContent)
}