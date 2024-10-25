// Clear previous elements
if (!data || data.length === 0) {
  svg.selectAll('*').remove();
  svg.append('text')
    .attr('x', 50)
    .attr('y', 50)
    .attr('text-anchor', 'start')
    .style('font-size', '20px')
    .text('No enriched terms shown under pvalue 0.05, please select more nodes from the bipartite network');
} else {
  svg.selectAll("*").remove();

  const padding = 20;
  const X = d3.scaleLinear().range([10, width - 10]);
  const Y = d3.scaleLinear().range([10, height - 10]);

  const nodes = HTMLWidgets.dataframeToD3(data.nodes);

  const clusters = d3.nest()
    .key(d => d.cluster)
    .entries(nodes);

  // Calculate cluster positions
  const clusterPositions = clusters.reduce((acc, cluster, i) => {
    acc[cluster.key] = { x: Math.random() * width, y: Math.random() * height };
    return acc;
  }, {});

  // Initialize node positions
  nodes.forEach(node => {
    node.x = clusterPositions[node.cluster].x + Math.random() * 50;
    node.y = clusterPositions[node.cluster].y + Math.random() * 50;
  });

  // Create intra-cluster links
  const intraClusterLinks = [];
  clusters.forEach(cluster => {
    const clusterNodes = cluster.values;
    for (let i = 0; i < clusterNodes.length - 1; i++) {
      for (let j = i + 1; j < clusterNodes.length; j++) {
        intraClusterLinks.push({
          source: clusterNodes[i],
          target: clusterNodes[j]
        });
      }
    }
  });

  // Force simulation with link distance force to control edge length
  const clusterForce = alpha => {
  clusters.forEach(cluster => {
    const clusterNodes = cluster.values;
    const centroidX = d3.mean(clusterNodes, d => d.x);
    const centroidY = d3.mean(clusterNodes, d => d.y);

    clusterNodes.forEach(node => {
       node.vx += (width / 2 - node.x) * alpha * 0.002;  // Small attractive force towards center
      node.vy += (height / 2 - node.y) * alpha * 0.002;
    });
  });
};

// Add the custom cluster force to the simulation
const simulation = d3.forceSimulation(nodes)
  .force("collision", d3.forceCollide().radius(10).iterations(2))
  .force("charge", d3.forceManyBody().strength(1))  // Weaker repulsion
  .force("center", d3.forceCenter(width / 2, height / 2))
  .force("link", d3.forceLink(intraClusterLinks).distance(50))  // Control link distance
  .force("cluster", clusterForce)  // Custom cluster force
  .on("tick", ticked);


  // Adding edges between nodes
  const link = svg.append("g")
    .attr("stroke", "#999")
    .attr("stroke-opacity", 0.6)
    .selectAll("line")
    .data(intraClusterLinks)
    .enter().append("line")
    .attr("stroke-width", 1.5)
    .attr("class", "edge");

  // Adding nodes with drag behavior
  const node = svg.append("g")
    .attr("class", "nodes")
    .selectAll("g")
    .data(nodes)
    .enter().append("g")
    .attr("transform", d => `translate(${d.x},${d.y})`)
    .call(drag(simulation));

  // Apply frequencies and pies
  nodes.forEach((node, i) => {
    node.freq = data.freq[i];
  });

  const uniqueDiseases = Array.from(new Set(data.freq.flatMap(Object.keys)));
  const colorScale = d3.scaleOrdinal().domain(uniqueDiseases).range(d3.schemeCategory10);
  const pie = d3.pie().value(d => d.value);
  const arc = d3.arc().innerRadius(0).outerRadius(10);

  node.each(function(d) {
    const nodeGroup = d3.select(this);
    const pieData = uniqueDiseases.map(disease => ({ key: disease, value: d.freq[disease] || 0 }));

    nodeGroup.selectAll("path")
      .data(pie(pieData))
      .enter().append("path")
      .attr("d", arc)
      .attr("fill", d => colorScale(d.data.key));
  });

  // Add legend for node pies
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

  // Tick function to update node and link positions
  function ticked() {
    link
      .attr("x1", d => d.source.x)
      .attr("y1", d => d.source.y)
      .attr("x2", d => d.target.x)
      .attr("y2", d => d.target.y);

    node
      .attr("transform", d => `translate(${d.x},${d.y})`);
  }

  // Drag behavior for nodes
  function drag(simulation) {
    function dragstarted(d) {
      if (!d3.event.active) simulation.alphaTarget(0.3).restart();  // Activate simulation during drag
      d.fx = d.x;  // Fix node x position
      d.fy = d.y;  // Fix node y position
    }

    function dragged(d) {
      d.fx = d3.event.x;  // Update node's x during drag
      d.fy = d3.event.y;  // Update node's y during drag
    }

    function dragended(d) {
      if (!d3.event.active) simulation.alphaTarget(0);  // End the simulation after dragging
      d.fx = null;  // Release node x after drag
      d.fy = null;  // Release node y after drag
    }

    return d3.drag()
      .on("start", dragstarted)
      .on("drag", dragged)
      .on("end", dragended);
  }

  // Tooltip for hovering nodes
  const tooltipDiv = d3.select("body").append("div")
    .attr("class", "tooltip")
    .style("opacity", 0);

  // Function to generate tooltip HTML content
  function generateTable(node) {
    return `
      <table style="font-size: 14px; border-collapse: collapse; width: 150px;">
        <tr style="background-color: grey; border: 1px solid #ddd;">
          <th style="padding: 8px; text-align: left;">ID</th>
        </tr>
        <tr style="background-color: white; border: 1px solid #ddd;">
          <td style="padding: 8px; text-align: left;">${node.id}</td>
        </tr>
      </table>
    `;
  }

  // Add hover effects
  node.on('mouseover', function(d) {
    d3.select(this)
      .style('cursor', 'pointer')
      .attr('r', 20);

    tooltipDiv.transition()
      .duration(500)
      .style("opacity", 1);
    tooltipDiv.html(generateTable(d))
      .style("left", (d3.event.pageX + 20) + "px")
      .style("top", (d3.event.pageY + 20) + "px");

  }).on('mouseout', function(d) {
    d3.select(this)
      .style('cursor', 'pointer')
      .attr('r', 17);

    tooltipDiv.transition()
      .duration(500)
      .style("opacity", 0);
  });
}
