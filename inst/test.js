// Set up the SVG canvas dimensions
var width = 600;
var height = 400;

// Create SVG element
var svg = d3.select("body").append("svg")
  .attr("width", width)
  .attr("height", height)
  .append("g") // Create a group element to allow for translation
  .attr("transform", "translate(" + width / 2 + "," + height / 2 + ")");

// Create simulation with forces
var simulation = d3.forceSimulation()
  .force("link", d3.forceLink().id(d => d.id).distance(100))
  .force("charge", d3.forceManyBody().strength(-200))
  .force("center", d3.forceCenter(0, 0)); // Center the force layout at (0,0)

// Convert data from R to D3-friendly format
var nodes = HTMLWidgets.dataframeToD3(data.nodes);
var edges = HTMLWidgets.dataframeToD3(data.edges);

// Create links (edges)
var link = svg.append("g")
  .attr("class", "links")
  .selectAll("line")
  .data(edges)
  .enter().append("line")
  .attr("stroke-width", 2);

// Create nodes
var node = svg.append("g")
  .attr("class", "nodes")
  .selectAll("circle")
  .data(nodes)
  .enter().append("circle")
  .attr("r", 10)
  .attr("fill", "steelblue")
  .call(d3.drag()
    .on("start", dragstarted)
    .on("drag", dragged)
    .on("end", dragended));

// Add labels
var label = svg.append("g")
  .attr("class", "labels")
  .selectAll("text")
  .data(nodes)
  .enter().append("text")
  .attr("dy", -3)
  .attr("text-anchor", "middle")
  .text(d => d.label);

// Apply the simulation to the graph
simulation
  .nodes(nodes)
  .on("tick", ticked);

simulation.force("link")
  .links(edges);

// Update positions
function ticked() {
  link
    .attr("x1", d => d.source.x)
    .attr("y1", d => d.source.y)
    .attr("x2", d => d.target.x)
    .attr("y2", d => d.target.y);

  node
    .attr("cx", d => d.x)
    .attr("cy", d => d.y);

  label
    .attr("x", d => d.x)
    .attr("y", d => d.y - 12);
}

// Functions for dragging nodes
function dragstarted(event, d) {
  if (!event.active) simulation.alphaTarget(0.3).restart();
  d.fx = d.x;
  d.fy = d.y;
}

function dragged(event, d) {
  d.fx = event.x;
  d.fy = event.y;
}

function dragended(event, d) {
  if (!event.active) simulation.alphaTarget(0);
  d.fx = null;
  d.fy = null;
}
