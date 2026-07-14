document.addEventListener('DOMContentLoaded', function () {
  const mapContainerId = 'expat_repat-icb_map';
  const mapEl = document.getElementById(mapContainerId);

  if (!mapEl) {
    console.error(`Map container not found: #${mapContainerId}`);
    return;
  }

  if (mapEl.getBoundingClientRect().height === 0) {
    console.warn(
      `Map container #${mapContainerId} has zero height. Set an explicit CSS height.`
    );
  }

  // Initialize the map and centre it roughly on the UK
  const map = L.map(mapContainerId).setView([54.7, -2.6], 6);
  let resizeScheduled = false;
  // set up some variables which we will load values into later
  let icbBoundaries = null;
  let icbLayer = null;
  let selectedIcbs = {};

  // Add a tile layer to the map (using Carto's light basemap)
  L.tileLayer(
    'https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png',
    {
      subdomains: 'abcd',
      maxZoom: 10,
      attribution: '&copy; OpenStreetMap contributors &copy; CARTO'
    }
  ).addTo(map);

  const viridisStops = ['#440154', '#21908c', '#fde725'];

  function hexToRgb(hex) {
    const clean = hex.replace('#', '');
    return {
      r: parseInt(clean.slice(0, 2), 16),
      g: parseInt(clean.slice(2, 4), 16),
      b: parseInt(clean.slice(4, 6), 16)
    };
  }

  function rgbToHex({ r, g, b }) {
    return '#' + [r, g, b]
      .map((x) => Math.round(x).toString(16).padStart(2, '0'))
      .join('');
  }

  function interpolateColour(a, b, t) {
    const c1 = hexToRgb(a);
    const c2 = hexToRgb(b);

    return rgbToHex({
      r: c1.r + (c2.r - c1.r) * t,
      g: c1.g + (c2.g - c1.g) * t,
      b: c1.b + (c2.b - c1.b) * t
    });
  }

  function viridisColour(value) {
    if (!Number.isFinite(value)) {
      return '#cccccc';
    }

    const clamped = Math.max(0, Math.min(1, value));
    const scaled = clamped * (viridisStops.length - 1);
    const idx = Math.min(Math.floor(scaled), viridisStops.length - 2);
    const t = scaled - idx;

    return interpolateColour(viridisStops[idx], viridisStops[idx + 1], t);
  }

  function fillColour(icbCode) {
    let pcnt = selectedIcbs?.[icbCode];
    let total = Object.values(selectedIcbs).reduce((a, v) => a + v, 0);

    return viridisColour(pcnt / total);
  }

  function invalidateMapSize() {
    if (resizeScheduled) {
      return;
    }

    resizeScheduled = true;
    requestAnimationFrame(() => {
      resizeScheduled = false;
      map.invalidateSize();
    });
  }

  // Function to render the points for the selected peers on the map
  function renderIcbLayer() {
    // check that the icbBoundaries has been loaded before trying to filter and render it
    if (!icbBoundaries) {
      return;
    }

    // the icbBoundaries contains all the providers, we filter it to only include the current
    // provider and its peers
    const filteredFeatures = icbBoundaries.features.filter((feature) => {
      // check that shiny has sent the peers to filter to. if it hasn't yet, don't show any points
      if (!selectedIcbs || Object.keys(selectedIcbs).length === 0) {
        return false;
      }
      // check if the current feature is in the selected peers list
      return Object.hasOwn(selectedIcbs, feature.properties.icb22cdh);
    });

    // remove the existing icbLayer from the map if it exists, so we can replace it with the
    // new filtered layer
    if (icbLayer) {
      map.removeLayer(icbLayer);
    }

    // generate a new layer with the filtered features and add it to the map. this layer will be
    // styled with circle markers and popups for each provider's name, coloured based on whether it
    // is the selected dataset or a peer.
    icbLayer = L.geoJSON(
      {
        type: 'FeatureCollection',
        features: filteredFeatures
      },
      {
        style: function (feature) {
          return {
            fillColor: fillColour(feature.properties.icb22cdh),
            color: '#000000',
            weight: 1,
            opacity: 1,
            fillOpacity: 0.7
          }
        },
        onEachFeature: function (feature, layer) {
          if (feature.properties && feature.properties.icb22nm) {
            pcnt = selectedIcbs[feature.properties.icb22cdh];
            layer.bindPopup(`${feature.properties.icb22nm}: ${(pcnt * 100).toFixed(1)}%`);
          }
        }
      }

    ).addTo(map);

    invalidateMapSize();

    const bounds = icbLayer.getBounds();
    if (bounds.isValid()) {
      map.fitBounds(bounds, {
        padding: [10, 10]
      });
    }
  }

  // asynchronously load the icb_boundaries.geojson file and store it in the icbBoundaries
  // variable. once loaded, call renderIcbLayer to display the points on the map.
  fetch('www/icb_boundaries.geojson')
    .then((response) => {
      if (!response.ok) {
        throw new Error('Failed to load icb_boundaries.geojson');
      }
      return response.json();
    })
    .then((geojson) => {
      icbBoundaries = geojson;
      renderIcbLayer();
    })
    .catch((error) => {
      console.error('Error loading icb_boundaries.geojson:', error);
    });

  // listen for messages from shiny containing the selected peers. when received, store the org_ids
  // in a Set and call renderProvidersLayer to update the map with the new selection.
  Shiny.addCustomMessageHandler('selectedIcbs', function (message) {
    selectedIcbs = message;
    renderIcbLayer();
  });

  const resizeObserver = new ResizeObserver(() => {
    if (mapEl.getBoundingClientRect().height > 0) {
      invalidateMapSize();
    }
  });

  resizeObserver.observe(mapEl);

});