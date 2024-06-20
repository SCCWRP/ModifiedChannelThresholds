resourcesUI <- function(id) {
  tagList(
      h3("Resources"),
      p("This dashboard is based on findings from SCCWRP Technical Report #1367, the data and code for which is hosted on ", a(href = "https://github.com/SCCWRP/BiointegrityEutrophicationIndicators", target = "_blank", .noWS = "outside", "GitHub"), "."),
      p("The code for this dashboard is also hosted on ", a(href = "https://github.com/SCCWRP/ModifiedChannelThresholds", target = "_blank", .noWS = "outside", "GitHub"), "."),
      p("To download the full thresholds table used in this application, click ", downloadLink(NS(id, "thresh_table"), label = "here", .noWS = "outside"), "."),
      p("To download a spreadsheet containing data sets used in each analysis, click ", downloadLink(NS(id, "data_sets"), label = "here", .noWS = "outside"), ". A data dictionary is included.")
    )
}