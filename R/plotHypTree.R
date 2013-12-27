PlotHypTree <- function(hyp.tree, adjust = T, json_file = 'hyp_tree.JSON', html_file = 'hyp_tree.html') {
    tmp <- tempdir()
    json_path <- paste(tmp, json_file, sep = "/")
    unlink(json_path)
    if(adjust) {
        tree.json <- HypTreeJSON(hyp.tree, type = 'unadjusted',
                                 file = json_path)
    } else {
        tree.json <- HypTreeJSON(hyp.tree, type = 'adjusted',
                                 file = json_path)
    }

    html <- paste("<!DOCTYPE html>
<meta charset=\"utf-8\">
<style>
div.tooltip {
position: absolute;
text-align: left;
width: 90px;
height: 10px;
padding: 8px;
font: 20px sans-serif;
background-color: transparent;
border: transparent;
border-radius: 8px;
pointer-events: none;
}

.node circle {
  stroke: #fff;
  stroke-width: 1.5px;
p}

.node {
  font: 10px sans-serif;
}

.link {
  fill: none;
  stroke: #ccc;
  stroke-width: 1.5px;
}

</style>

<body>
<script src=\"http://d3js.org/d3.v3.min.js\"></script>
<script>

var width = 800,
    height = 500;

var tree = d3.layout.tree()
    .size([height, width - 160]);

var diagonal = d3.svg.diagonal()
    .projection(function(d) { return [d.y, d.x]; });

var svg = d3.select(\"body\").append(\"svg\")
    .attr(\"width\", width)
    .attr(\"height\", height)
  .append(\"g\")
    .attr(\"transform\", \"translate(40,0)\");

var div = d3.select(\"body\").append(\"div\")
  .attr(\"class\", \"tooltip\")
  .style(\"opacity\", 1e-6);

var color = d3.scale.linear()
   .domain([0,", hyp.tree@alpha, ", 1])
   .range([\"steelblue\", \"magenta\", \"orange\"]);

function newScale(d){
  var nullColor = \"grey\"
  return (d === undefined || d === null)?nullColor:color(d)}

d3.json(\"", json_file,  "\", function(error, json) {
  var nodes = tree.nodes(json),
      links = tree.links(nodes);

  var link = svg.selectAll(\"path.link\")
      .data(links)
    .enter().append(\"path\")
      .attr(\"class\", \"link\")
      .attr(\"d\", diagonal);

  var node = svg.selectAll(\"g.node\")
      .data(nodes)
    .enter().append(\"g\")
      .attr(\"class\", \"node\")
      .on(\"mouseover\", mouseover)
      .on(\"mouseout\", mouseout)
      .attr(\"transform\", function(d) { return \"translate(\" + d.y + \",\" + d.x + \")\"; });

  node.append(\"circle\")
   .style(\"fill\", function(d) { return newScale(d.pval); })
   .attr(\"r\", 5);

  node.append(\"text\")
      .attr(\"dx\", -8)
      .attr(\"dy\", -8)
      .attr(\"text-anchor\",\"start\")
      .style(\"font\", \"12px sans-serif\")
      .style(\"fill\", function(d) { return newScale(d.pval);})
      .text(function(d) { return d.name; });
});

function mouseover(d) {
  d3.select(this).select(\"circle\").transition()
  .duration(350)
  .attr(\"r\", 7);
  d3.select(this).select(\"text\").transition()
  .duration(350)
 p .style(\"font\", \"15px sans-serif\");

  div
  .style(\"opacity\", 1)
  .style(\"color\", newScale(d.pval))
  .text(\"p-value: \" + d.pval)
}

function mouseout() {
  d3.select(this).select(\"circle\").transition()
  .duration(350)
  .attr(\"r\", 5);
  d3.select(this).select(\"text\").transition()
  .duration(350)
  .style(\"font\", \"12px sans-serif\");
}

d3.select(self.frameElement).style(\"height\", height + \"px\");
</script>", sep = "")

    html_path <- paste(tmp, html_file, sep = "/")
    unlink(html_path)
    HTML(html, file = html_path)
    browseURL(html_path)
}
