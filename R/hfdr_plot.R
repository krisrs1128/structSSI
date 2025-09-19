#' @importFrom jsonlite toJSON
#' @import igraph
HypTreeJSON <- function(hyp.tree, type = 'unadjusted') {
  tree <- graph.edgelist(hyp.tree@tree)
  V(tree)$names <- rownames(hyp.tree@p.vals)
  if(type == 'adjusted') {
    V(tree)$pval <- round(hyp.tree@p.vals[, 'adjp'], 5)
  } else {
    V(tree)$pval <- round(hyp.tree@p.vals[, 'unadjp'], 5)
  }
  toJSON(ListTreePval(tree))
}

#' @importFrom utils browseURL
#' @export
PlotHypTree <- function(hyp.tree, adjust = TRUE, return_script = FALSE,
                        width = 900, height = 500, base_font_size = 12,
                        output_file_name = NULL) {
  if (is.null(output_file_name)) {
    output_file_naem <- paste('hyp_tree', gsub("[^\\d]+", "", Sys.time(), perl=TRUE), '.html')
  }

  if(adjust) {
    tree.json <- HypTreeJSON(hyp.tree, type = 'adjusted')
  } else {
    tree.json <- HypTreeJSON(hyp.tree, type = 'unadjusted')
  }

  hyp.tree.script <- paste(
    '
<style>

.node circle {
  stroke: #fff;
  stroke-width: 1.5px;
}

.link {
  fill: none;
  stroke: #ccc;
  stroke-width: 2px;
}

</style>

<body>
<script src="https://d3js.org/d3.v3.min.js"></script>
<script>

var width = ', width,',
    height = ', height, ';

var cluster = d3.layout.cluster()
    .size([height, width - 200]);

var diagonal = d3.svg.diagonal()
    .projection(function(d) { return [d.y, d.x]; });

var svg = d3.select("body").append("svg")
    .attr("width", width)
    .attr("height", height)
    .append("g")
    .attr("transform", "translate(40,0)");

var color = d3.scale.linear()
   .domain([0,', hyp.tree@alpha, ', 1])
   .range(["steelblue", "magenta", "orange"]);

function newScale(d){
  var nullColor = "grey"
  return (d === undefined || d === "NA")?nullColor:color(d)}

var json = ', tree.json,';

var nodes = cluster.nodes(json),
    links = cluster.links(nodes);

var link = svg.selectAll("path.link")
    .data(links)
   .enter().append("path")
    .attr("class", "link")
    .attr("d", diagonal);

var node = svg.selectAll("g.node")
    .data(nodes)
   .enter().append("g")
    .attr("class", "node")
    .on("mouseover", mouseover)
    .on("mouseout", mouseout)
    .attr("transform", function(d) { return "translate(" + d.y + "," + d.x + ")"; });

node.append("circle")
    .style("fill", function(d) { return newScale(d.pval); })
    .attr("r", 5);

node.append("text")
    .attr("dx", function(d) {return d.children ? -8 : 8;})
    .attr("dy", function(d) {return d.children ? -8 : 0;})
    .attr("text-anchor","start")
    .style("font", "', base_font_size, 'px sans-serif")
    .style("fill", function(d) { return newScale(d.pval);})
    .text(function(d) { return d.name; });

function mouseover(d) {
    d3.select(this).select("circle").transition()
    .duration(350)
    .attr("r", 7);
    d3.select(this).select("text").transition()
    .text(function(d) {return d.name + ": " + d.pval;})
    .duration(350)
    .style("font", "', base_font_size + 3, 'px sans-serif");
}

function mouseout() {
     d3.select(this).select("circle").transition()
      .duration(350)
      .attr("r", 5);
     d3.select(this).select("text").transition()
      .duration(350)
      .text(function(d) {return d.name;})
      .style("font", "', base_font_size, 'px sans-serif");
}

d3.select(self.frameElement).style("height", height + "px");
</script>', sep = '')

  if(return_script) {
    return(hyp.tree.script)
  } else {
    html_path <- paste(tempdir(), output_file_name, sep = "/")
    unlink(html_path)
    cat(paste("<!DOCTYPE html>
<meta charset=\"utf-8\">", hyp.tree.script, sep = ""), file = html_path)
    if (interactive()) {
      browseURL(html_path)
    } else {
      message("Plot available at: ", html_path)
    }
  }
}
#' Interactively VIsualize a HypothesisTree
#'
#' Generates a D3.js-based visualization of a \code{HypothesisTree}
#' object. Nodes are colored by p-value, and hovering over a node displays its
#' name and either the raw or adjusted p-value, depending on the \code{adjust}
#' argument.
#'
#' @param hyp.tree An object of class \code{HypothesisTree}.
#' @param adjust Logical; if \code{TRUE}, use adjusted p-values for coloring.
#'   If \code{FALSE}, use raw p-values.
#' @param return_script Logical; if \code{TRUE}, return the HTML/JS script as a
#'   character string instead of writing to a file and opening in a browser.
#' @param width Width of the plot in pixels. Default is 900.
#' @param height Height of the plot in pixels. Default is 500.
#' @param base_font_size Base font size for node labels. Default is 12.
#' @param output_file_name Optional file name for the HTML output. If
#'   \code{NULL}, a name is generated automatically.
#'
#' @return If \code{return_script} is \code{TRUE}, returns a character string
#'   containing the HTML/JS code. Otherwise, writes the visualization to an HTML
#'   file and opens it in the default browser (if interactive).
#'
#' @details
#' The visualization displays the hierarchical structure of hypotheses. Node
#' color reflects the p-value, and hovering over a node shows its name and
#' p-value.
#'
#' @importFrom utils browseURL
#' @export
PlotHypTree <- function(hyp.tree, adjust = TRUE, return_script = FALSE,
                        width = 900, height = 500, base_font_size = 12,
                        output_file_name = NULL) {
  if (is.null(output_file_name)) {
    output_file_naem <- paste('hyp_tree', gsub("[^\\d]+", "", Sys.time(), perl=TRUE), '.html')
  }

  if(adjust) {
    tree.json <- HypTreeJSON(hyp.tree, type = 'adjusted')
  } else {
    tree.json <- HypTreeJSON(hyp.tree, type = 'unadjusted')
  }

  hyp.tree.script <- paste(
    '
<style>

.node circle {
  stroke: #fff;
  stroke-width: 1.5px;
}

.link {
  fill: none;
  stroke: #ccc;
  stroke-width: 2px;
}

</style>

<body>
<script src="https://d3js.org/d3.v3.min.js"></script>
<script>

var width = ', width,',
    height = ', height, ';

var cluster = d3.layout.cluster()
    .size([height, width - 200]);

var diagonal = d3.svg.diagonal()
    .projection(function(d) { return [d.y, d.x]; });

var svg = d3.select("body").append("svg")
    .attr("width", width)
    .attr("height", height)
    .append("g")
    .attr("transform", "translate(40,0)");

var color = d3.scale.linear()
   .domain([0,', hyp.tree@alpha, ', 1])
   .range(["steelblue", "magenta", "orange"]);

function newScale(d){
  var nullColor = "grey"
  return (d === undefined || d === "NA")?nullColor:color(d)}

var json = ', tree.json,';

var nodes = cluster.nodes(json),
    links = cluster.links(nodes);

var link = svg.selectAll("path.link")
    .data(links)
   .enter().append("path")
    .attr("class", "link")
    .attr("d", diagonal);

var node = svg.selectAll("g.node")
    .data(nodes)
   .enter().append("g")
    .attr("class", "node")
    .on("mouseover", mouseover)
    .on("mouseout", mouseout)
    .attr("transform", function(d) { return "translate(" + d.y + "," + d.x + ")"; });

node.append("circle")
    .style("fill", function(d) { return newScale(d.pval); })
    .attr("r", 5);

node.append("text")
    .attr("dx", function(d) {return d.children ? -8 : 8;})
    .attr("dy", function(d) {return d.children ? -8 : 0;})
    .attr("text-anchor","start")
    .style("font", "', base_font_size, 'px sans-serif")
    .style("fill", function(d) { return newScale(d.pval);})
    .text(function(d) { return d.name; });

function mouseover(d) {
    d3.select(this).select("circle").transition()
    .duration(350)
    .attr("r", 7);
    d3.select(this).select("text").transition()
    .text(function(d) {return d.name + ": " + d.pval;})
    .duration(350)
    .style("font", "', base_font_size + 3, 'px sans-serif");
}

function mouseout() {
     d3.select(this).select("circle").transition()
      .duration(350)
      .attr("r", 5);
     d3.select(this).select("text").transition()
      .duration(350)
      .text(function(d) {return d.name;})
      .style("font", "', base_font_size, 'px sans-serif");
}

d3.select(self.frameElement).style("height", height + "px");
</script>', sep = '')

  if(return_script) {
    return(hyp.tree.script)
  } else {
    html_path <- paste(tempdir(), output_file_name, sep = "/")
    unlink(html_path)
    cat(paste("<!DOCTYPE html>
<meta charset=\"utf-8\">", hyp.tree.script, sep = ""), file = html_path)
    if (interactive()) {
      browseURL(html_path)
    } else {
      message("Plot available at: ", html_path)
    }
  }
}
