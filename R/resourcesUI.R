resourcesUI <- function(id) {
  fluidPage(
    column(2),
    column(
      8,
      h3("Resources", style = "color:#2f5496"),
      p("This dashboard is based on findings from SCCWRP Technical Report #1367, the data and code for which is hosted on ", a(href = "https://github.com/SCCWRP/BiointegrityBiostimulatoryIndicators", target = "_blank", .noWS = "outside", "GitHub"), "."),
      p("The code for this dashboard is also hosted on ", a(href = "https://github.com/SCCWRP/ModifiedChannelThresholds", target = "_blank", .noWS = "outside", "GitHub"), "."),
      p("To download the full thresholds table used in this application, click ", downloadLink(NS(id, "thresh_table"), label = "here", .noWS = "outside"), ".")
    ),
    column(2)
  )
}