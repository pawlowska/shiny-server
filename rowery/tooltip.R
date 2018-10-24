tooltip_position<-function(hover, w=130) {   #calculate the position of the tooltip
  #in relative units
  left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
  top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
  #in pixel units
  left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
  if(hover$range$right-left_px<w) left_px<-left_px-(w+5) #tooltips too far right were hidden
  
  top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
  
  c(left_px, top_px)
}

tooltip_html<-function(pos_px) {
  left_px<-pos_px[1]
  top_px<-pos_px[2]
  paste0("position:absolute; z-index:100; padding: 0 6px 0 6px; height: 22px; overflow: hidden;
         background-color: rgba(245, 245, 245, 0.85); ",
         "left:", left_px + 2, "px; top:", top_px + 2, "px;")
}

tooltipWellPanel<-function(hover, string) {
  wellPanel(
    style = tooltip_html(tooltip_position(hover)),
    p(HTML(string))
  )
}