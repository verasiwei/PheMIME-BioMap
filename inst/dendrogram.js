// Set up the margin, width, and height
var margin = {top: 20, right: 120, bottom: 20, left: 120},
    width = 960 - margin.left - margin.right,
    height = 800 - margin.top - margin.bottom;

// Append the svg object to the body of the page
var svg = d3.select("body").append("svg")
    .attr("width", width + margin.right + margin.left)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

// Create a cluster layout
var cluster = d3.cluster()
    .size([height, width - 160]);

// Function to render the dendrogram using the provided data
function drawDendrogram(data) {
  var root = d3.hierarchy(data, function(d) { return d.children; });

  cluster(root);

  // Draw the links
  var link = svg.selectAll(".link")
      .data(root.links())
      .enter().append("path")
      .attr("class", "link")
      .attr("d", d3.linkVertical()
                    .x(d => d.x)
                    .y(d => d.y));

  // Draw the nodes
  var node = svg.selectAll(".node")
      .data(root.descendants())
      .enter().append("g")
      .attr("class", function(d) { 
        return "node" + (d.children ? " node--internal" : " node--leaf"); 
      })
      .attr("transform", function(d) { 
        return "translate(" + d.x + "," + d.y + ")"; 
      });

  node.append("circle")
      .attr("r", 2.5);

  node.append("text")
      .attr("dy", 3)
      .attr("x", function(d) { return d.children ? -8 : 8; })
      .style("text-anchor", function(d) { return d.children ? "end" : "start"; })
      .text(function(d) { return d.data.name; });
}

// Call the function with the data provided by r2d3
drawDendrogram(data);
