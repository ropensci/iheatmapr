// Function adapted from Plotly R Package 3.60, 
HTMLWidgets.widget({
  name: "iheatmapr",
  type: "output",
  
  initialize: function(el, width, height) {
    return {};
  },
  
  resize: function(el, width, height, instance) {
    Plotly.relayout(el.id, {width: width, height: height});
  },  
  
  renderValue: function(el, x, instance) {
    
    var shinyMode;
    if (typeof(window) !== "undefined") {
      // make sure plots don't get created outside the network
      window.PLOTLYENV = window.PLOTLYENV || {};
      window.PLOTLYENV.BASE_URL = x.base_url;
      shinyMode = !!window.Shiny;
    }
    
    var graphDiv = document.getElementById(el.id);
    
    // if no plot exists yet, create one with a particular configuration
    if (!instance.plotly) {
      Plotly.newPlot(graphDiv, x.data, x.layout, x.config);
      instance.plotly = true;
      instance.autosize = x.layout.autosize;
    } else {
      Plotly.newPlot(graphDiv, x.data, x.layout);
    }
    
    sendEventData = function(eventType) {
      return function(eventData) {
        if (eventData === undefined || !eventData.hasOwnProperty("points")) {
          return null;
        }
        var d = eventData.points.map(function(pt) {
          var obj = {
                curveNumber: pt.curveNumber, 
                pointNumber: pt.pointNumber, 
                x: pt.x,
                y: pt.y
          };
          // grab the trace corresponding to this point
          var tr = x.data[pt.curveNumber];
          // add on additional trace info, if it exists
          attachKey = function(keyName) {
            if (tr.hasOwnProperty(keyName) && tr[keyName] !== null) {
              if (typeof pt.pointNumber === "number") {
                obj[keyName] = tr[keyName][pt.pointNumber];
              } else {
                obj[keyName] = tr[keyName][pt.pointNumber[0]][pt.pointNumber[1]];
              }// TODO: can pointNumber be 3D?
            }
          };
          attachKey("z");
          attachKey("key");
          return obj; 
        });
        Shiny.onInputChange(
          ".clientValue-" + eventType + "-" + x.source, 
          JSON.stringify(d)
        );
      };
    };
    
    // send user input event data to shiny
    if (shinyMode) {
      // https://plot.ly/javascript/zoom-events/
      graphDiv.on('plotly_relayout', function(d) {
        Shiny.onInputChange(
          ".clientValue-" + "iheatmapr_relayout" + "-" + x.source, 
          JSON.stringify(d)
        );
      });
      graphDiv.on('plotly_hover', sendEventData('iheatmapr_hover'));
      graphDiv.on('plotly_click', sendEventData('iheatmapr_click'));
      graphDiv.on('plotly_selected', sendEventData('iheatmapr_selected'));
      graphDiv.on('plotly_unhover', function(eventData) {
        Shiny.onInputChange(".clientValue-iheatmapr_hover-" + x.source, null);
      });
      graphDiv.on('plotly_doubleclick', function(eventData) {
        Shiny.onInputChange(".clientValue-iheatmapr_click-" + x.source, null);
      });
      // 'plotly_deselect' is code for doubleclick when in select mode
      graphDiv.on('plotly_deselect', function(eventData) {
        Shiny.onInputChange(".clientValue-iheatmapr_selected-" + x.source, null);
        Shiny.onInputChange(".clientValue-iheatmapr_click-" + x.source, null);
      });
    } 
    
  } 
  
});
