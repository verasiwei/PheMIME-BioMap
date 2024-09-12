if(!data || data.length === 0){
  // Data is empty or condition is met - display message
        svg.selectAll('*').remove(); // Clear the SVG in case it was previously used
        svg.append('text')
          .attr('x', 50) // Adjust x position as needed
          .attr('y', 50) // Adjust y position as needed
          .attr('text-anchor', 'start')
          .style('font-size', '20px')
          .text('No enriched terms shown under pvalue 0.05, please select more nodes from the bipartite network');
} else {
  svg.selectAll("*").remove();
//svg.style('background-color', "#F2F3F6");
const padding = 20;
const X = d3.scaleLinear()
  .range([10, width - 10]);
  
const Y = d3.scaleLinear()
  .range([10, height-20]);
  
const nodes = HTMLWidgets.dataframeToD3(data.nodes);
const links = HTMLWidgets.dataframeToD3(data.edges);
//console.log(nodes);

///removing force-directed network
///begin to force-directed simulation
const simulation = d3.forceSimulation(nodes)
  .force(
    "link", 
    d3.forceLink(links)
      .id(d => d.id)
      .distance(200)
      .strength(1) // Increase the strength to make it stable
      //.iterations(2)
  )
  .force(
    "collision", 
    d3.forceCollide()
      .radius(d => 17)
   )
  .force(
    "charge", 
    d3.forceManyBody()
     .strength(-30)
  ) 
  .on("tick", ticked);
// Run the simulation for a few iterations to stabilize the positions
for (let i = 0; i < 500; i++) {
  simulation.tick();
}

///create the network
const link = svg.append("g")
   .attr("stroke", "#999")
   .attr("stroke-opacity", 0.1)
   .selectAll("line")
   .data(links)
   .enter().append("line")
   .attr("stroke-width", 1.5)
   .attr("class", "edge"); // Add the 'edge' class to the edges;

// Replace the node creation code with pie chart rendering for each node
// Create node groups that will contain the pie charts
const node = svg.append("g")
    .attr("class", "nodes")
  .selectAll("g")
  .data(nodes)
  .enter().append("g")
    .attr("transform", d => `translate(${d.x},${d.y})`)
    .call(drag(simulation));

// Include frequency data for each node, assuming freq is aligned with nodes by index
nodes.forEach((node, i) => {
  node.freq = data.freq[i];
});
// Create a pie generator
const pie = d3.pie().value(d => d);
const arc = d3.arc().innerRadius(0).outerRadius(10); // Adjust outerRadius as needed
 
// Determine unique disease phenotypes
  let uniqueDiseases = new Set();
  data.freq.forEach(freq => {
    Object.keys(freq).forEach(key => {
      uniqueDiseases.add(key);
    });
  });
  uniqueDiseases = Array.from(uniqueDiseases);

  const colorScale = d3.scaleOrdinal(d3.schemeCategory10);

  node.each(function(d, i) {
    const nodeGroup = d3.select(this);
    nodeGroup.selectAll("path")
      .data(pie(Object.values(d.freq)))
      .enter().append("path")
        .attr("d", arc)
        .attr("fill", (d, i) => colorScale(uniqueDiseases[i % uniqueDiseases.length]));
  });
  
// Create legend
  const legend = svg.append("g")
    .attr("class", "legend")
    .attr("transform", "translate(10, 10)");

  uniqueDiseases.forEach((disease, i) => {
    legend.append("rect")
      .attr("x", 0)
      .attr("y", i * 20)
      .attr("width", 18)
      .attr("height", 18)
      .style("fill", colorScale(disease));

    legend.append("text")
      .attr("x", 24)
      .attr("y", i * 20 + 9)
      .attr("dy", ".35em")
      .style("text-anchor", "start")
      .text(disease);
  });

// Calculate the centroid of the network
let centroidX = d3.mean(nodes, d => d.x);
let centroidY = d3.mean(nodes, d => d.y);

// Determine the SVG center
let svgCenterX = width / 4;
let svgCenterY = height / 4;

// Calculate translation needed to center the network
let translateX = svgCenterX - centroidX;
let translateY = svgCenterY - centroidY;

// Apply the translation to each node's position
nodes.forEach(node => {
  node.x += translateX;
  node.y += translateY;
});

/// Add CSS styles for the "hovered" class
const hoverStyles = {
  cursor: 'pointer', // Change cursor to a hand
  r: 20, // Increase node size on hover
};
const unhoverStyles = {
  cursor: 'pointer', // Reset cursor
  r: 17, // Reset node size
};

// Make sure each node has a unique ID for selection
node.attr('id', d => `node-${d.id}`);

// Add event listeners to show and hide the table on hover
node.on('mouseover', function(d){
  //const isPreSelected = d.selected === 'yes';
  //const isSelected = selectedNodes.has(d);

  d3.select(this)
    //.classed('hovered', true) // Add the "hovered" class
    .style('cursor', hoverStyles.cursor) // Change cursor to a hand
    .attr('r', hoverStyles.r); // Apply hover styles
    
  // Show the table and populate it with node information
  tooltipDiv.transition()
    .duration(500)
    .style("opacity", 1);
  tooltipDiv.html(generateTable(d))
    .style("left", (d3.event.pageX + 20) + "px")
    .style("top", (d3.event.pageY + 20) + "px");
    
  // Highlight connected edges
  link.filter(l => l.source === d || l.target === d)
        .style('stroke', 'black') // Change to black or any other color
        .style('stroke-width', '5px'); // Increase stroke-width to highlight
    
})
.on('mouseout', function(d){

  d3.select(this)
    //.classed('hovered', false) // Remove the "hovered" class
    .style('cursor', unhoverStyles.cursor) // Reset cursor
    .attr('r', unhoverStyles.r); // Reset node size
    
  // Hide the table when the mouse moves away
  tooltipDiv.transition()
    .duration(500)
    .style("opacity", 0);
  
  // Reset connected edges to original style
  link.filter(l => l.source === d || l.target === d)
        .style('stroke', '#999') // Reset to original color
        .style('stroke-width', '1.5px'); // Reset to original stroke-width
    
});


function ticked(){
  
  link
      .attr("x1", d => d.source.x + translateX)
      .attr("y1", d => d.source.y + translateY)
      .attr("x2", d => d.target.x + translateX)
      .attr("y2", d => d.target.y + translateY);

  node
      .attr("cx", d => d.x + translateX)
      .attr("cy", d => d.y + translateY);
  node.attr("transform", d => `translate(${d.x + translateX},${d.y + translateY})`);
  
}

function drag(simulation) {
  
function dragstarted(d) {
    if (!d3.event.active) simulation.alphaTarget(0.3).restart();
    d.fx = d.x;
    d.fy = d.y;
  }
  
  function dragged(d) {
    d.fx = d3.event.x;
    d.fy = d3.event.y;
  }
  
  function dragended(d) {
    if (!d3.event.active) simulation.alphaTarget(0);
    d.fx = d.x = d3.event.x;
    d.fy = d.y = d3.event.y; // Fix the node position
  }
  
  return d3.drag()
    .on("start", dragstarted)
    .on("drag", dragged)
    .on("end", dragended);
}

///adding node information table
// Create a div element to hold the table
const tooltipDiv = d3.select("body").append("div")
  .attr("class", "tooltip")
  .style("opacity", 0);
// Function to generate the HTML table for a node with styles
function generateTable(node) {
  return `
    <table style="font-size: 14px; border-collapse: collapse; width: 150px;">
      <tr style="background-color: grey; border: 1px solid #ddd;">
        <th style="padding: 8px; text-align: left;">ID</th>
      </tr>
      <tr style="background-color: white;border: 1px solid #ddd;">
        <td style="padding: 8px; text-align: left;">${node.id}</td>
      </tr>
    </table>
  `;
}
// Function to generate a simplified HTML tooltip with only the ID
function generateSimplifiedTooltip(node) {
  return `
    <div style="font-size: 12px; padding: 4px; background-color: white; border: 1px solid #ddd;">
      <strong>ID:</strong> ${node.id}
    </div>
  `;
}
}