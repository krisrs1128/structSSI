PlotHypTree <- function(hyp.tree, adjust = TRUE,
                        output_script = FALSE, width = 1200,
                        height = 1000, base_font_size = 12,
                        json_file = 'hyp_tree.JSON',
                        html_file = 'hyp_tree.html') {
    tmp <- tempdir()
    json_path <- paste(tmp, json_file, sep = "/")
    unlink(json_path)
    if(adjust) {
        tree.json <- HypTreeJSON(hyp.tree, type = 'adjusted',
                                 file = json_path)
    } else {
        tree.json <- HypTreeJSON(hyp.tree, type = 'unadjusted',
                                 file = json_path)
    }

    hyp.tree.script <- paste(
        '<style>

        .node circle {
            stroke: #fff;
            stroke-width: 1.5px;
        }

        .link {
            fill: none;
            stroke: #ccc;
            stroke-width: 3px;
        }

        </style>

        <body>
        <script src="http://d3js.org/d3.v3.min.js"></script>
        <script>

        var width = ', width,',
            height = ', 500, ';

        var tree = d3.layout.tree()
         .size([height, width - 160]);

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
        return (d === undefined || d === null)?nullColor:color(d)}

       d3.json("', json_path, '", function(error, json) {
        var nodes = tree.nodes(json),
        links = tree.links(nodes);

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
       .attr("dx", -8)
       .attr("dy", -8)
       .attr("text-anchor","start")
       .style("font", "', base_font_size, 'px sans-serif")
       .style("fill", function(d) { return newScale(d.pval);})
       .text(function(d) { return d.name; });
});

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

   if(output_script) {
       cat(hyp.tree.script)
   } else {
        html_path <- paste(tmp, html_file, sep = "/")
        unlink(html_path)
        HTML(paste("<!DOCTYPE html>
<meta charset=\"utf-8\">", hyp.tree.script, sep = ""), file = html_path)
        browseURL(html_path)
    }
}
