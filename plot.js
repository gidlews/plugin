Plot = function(hist) {
    this.isTProfile = function() {
        return (this.type == "TProfile")
    }
    this.isTH1 = function() {
        return (this.type == "TH1" || this.type == "TProfile")
    }
    this.isTH2= function() {
        return (this.type == "TH2" || this.type == "TProfile2D")
    }
    this.type = hist.type;
    this.area = {
            margin : "", 
            width : 0,
            height : 0};
    this.x = {
        min : {
            id : 0,
            value: hist.xaxis.first.value
        },
        max : {
            id : hist.xaxis.last.id,
            value: hist.xaxis.last.value
        },
        customised : false,
        width : false,
        domain : {
            min : null,
            max : null
        },
        scale : null,
        bins: null,
        plot: this
    };
    this.x.isInDomain = function(value) {
        return (value >= this.min.value && value <= this.max.value);
    }
    this.x.isWithinDomain = function(value) {
        return (value > this.min.value && value < this.max.value);
    }
    this.x.setScaleLimit = function(minId,maxId) {
        if(!this.scale)
            return;
        if(minId != null && maxId != null)
            if(maxId > minId) {
               this.scale.domain([this.bins.lowEdgeX(minId),this.bins.lowEdgeX(maxId)])
               this.min.value = this.bins.lowEdgeX(minId);
               this.max.value = this.bins.lowEdgeX(maxId);
               this.max.id = this.min.id + maxId;
               this.min.id += minId;
            }
            else
                return;
        if(minId != null && maxId == null)
            if(this.max.id > this.min.id + minId) {
                this.scale.domain([this.bins.lowEdgeX(minId),this.max.value ])
                this.min.value = this.bins.lowEdgeX(minId);
                this.min.id += minId;
            }
            else
                return;
        if(minId == null && maxId != null)
            if(this.min.id + maxId > this.min.id) {
                this.scale.domain([this.min.value , this.bins.lowEdgeX(maxId)])
                this.max.value = this.bins.lowEdgeX(maxId);
                this.max.id = this.min.id + maxId;
            }
            else
                return;
        if(this.plot.isTH1()) {
            if(this.plot.y.customised)
                return;
            if(this.plot.isTProfile()) {
                var maxVals = []; minVals = [];
                for(var i = 0; i != this.plot.bins.xLen(); ++i) {
                    if(!this.plot.y.log || this.plot.bins.bins(i)+this.plot.bins.error(i) > 0)
                        maxVals.push(this.plot.bins.bins(i)+this.plot.bins.error(i));
                    if(!this.plot.y.log || this.plot.bins.bins(i)-this.plot.bins.error(i) > 0)
                        minVals.push(this.plot.bins.bins(i)-this.plot.bins.error(i));
                }
                this.plot.y.setScaleDomain(d3.min(minVals), d3.max(maxVals))
            } else    
                var maxVals = []; minVals = [];
                for(var i = 0; i != this.plot.bins.xLen(); ++i) {
                    if(!this.plot.y.log || this.plot.bins.bins(i)> 0) {
                        maxVals.push(this.plot.bins.bins(i));
                        minVals.push(this.plot.bins.bins(i));
                    }
                }
                this.plot.y.setScaleDomain(d3.min(minVals), d3.max(maxVals))
        }
        if(this.plot.isTH2()) {
            if(this.plot.z.customised)
                return;
            if(this.plot.z.log) {
                var maxOfCols = []; minOfCols = [];
                for(var col = 0; col != this.plot.bins.yLen(); ++col) {
                    maxOfCols.push(d3.max(this.plot.bins.binsRow(col)));
                    for(var row = 0; row != this.plot.bins.xLen(); ++row) {
                        if(this.plot.bins.bins(row,col)>0)
                            minOfCols.push(this.plot.bins.bins(row,col));
                    }
                }
                this.plot.z.setScaleDomain(d3.min(minOfCols), d3.max(maxOfCols))
            } else {
                var maxOfCols = []; minOfCols = [];
                for(var col = 0; col != this.plot.bins.yLen(); ++col) {
                    maxOfCols.push(d3.max(this.plot.bins.binsRow(col)));
                    minOfCols.push(d3.min(this.plot.bins.binsRow(col)));
                }
                this.plot.z.setScaleDomain(d3.min(minOfCols), d3.max(maxOfCols))                
            }
        }
    }
    if(this.isTH2()) {
        this.y = {
            min : {
                id : 0,
                value: hist.yaxis.first.value
            },
            max : {
                id : hist.yaxis.last.id,
                value: hist.yaxis.last.value
            },
            customised : false,
            width : false,
            scale : null,
            bins: null,
            plot: this
        };
        this.y.setScaleLimit = function(minId,maxId) {
            if(!this.scale)
                return;
            if(minId != null && maxId != null)
                if(maxId > minId) {
                   this.scale.domain([this.bins.lowEdgeY(minId),this.bins.lowEdgeY(maxId)])
                   this.min.value = this.bins.lowEdgeY(minId);
                   this.max.value = this.bins.lowEdgeY(maxId);
                   this.max.id = this.min.id + maxId;
                   this.min.id += minId;
                }
            if(minId != null && maxId == null)
                if(this.max.id > this.min.id + minId) {
                    this.scale.domain([this.bins.lowEdgeY(minId),this.max.value ])
                    this.min.value = this.bins.lowEdgeY(minId);
                    this.min.id += minId;
                }
            if(minId == null && maxId != null)
                if(this.min.id + maxId > this.min.id) {
                    this.scale.domain([this.min.value , this.bins.lowEdgeY(maxId)])
                    this.max.value = this.bins.lowEdgeY(maxId);
                    this.max.id = this.min.id + maxId;
                }
            var maxOfCols = []; minOfCols = [];
            if(this.plot.z.customised)
                return;
            if(this.plot.z.log) {
                for(var col = 0; col != this.plot.bins.yLen(); ++col) {
                    maxOfCols.push(d3.max(this.plot.bins.binsRow(col)));
                    for(var row = 0; row != this.plot.bins.xLen(); ++row) {
                        if(this.plot.bins.bins(row,col)>0)
                            minOfCols.push(this.plot.bins.bins(row,col));
                    }
                }
                this.plot.z.setScaleDomain(d3.min(minOfCols), d3.max(maxOfCols))
            } else {
                for(var col = 0; col != this.plot.bins.yLen(); ++col) {
                    maxOfCols.push(d3.max(this.plot.bins.binsRow(col)));
                    minOfCols.push(d3.min(this.plot.bins.binsRow(col)));
                }
                this.plot.z.setScaleDomain(d3.min(minOfCols), d3.max(maxOfCols))
            }
        }
        this.y.isInDomain = function(value) {
            return (value >= this.min.value && value <= this.max.value);
        }
        this.y.isWithinDomain = function(value) {
            return (value > this.min.value && value < this.max.value);
        }
        this.z = {
                log: false,
                min : hist.values.min,
                max : hist.values.max,
                customised : false,
                scale : null
        }
        this.z.setScaleDomain = function(min,max) {
            if(!this.scale)
                return;
            if(this.log && min <= 0)
                min = 0.0001;
            if(min != null && max != null)
                if(max > min) {
                   this.scale.domain([min, max])
                   this.min = min;
                   this.max = max;
                }
            if(min != null && max == null)
                if(this.max > min) {
                   this.scale.domain([min, this.max])
                   this.min = min;
                }
            if(min == null && max != null)
                if(max > this.min) {
                   this.scale.domain([this.min, max])
                   this.max = max;
                }
            if(!this.customised) {
//                if(this.log && this.min == 1)
//                    this.scale.domain([this.min-0.0001, this.max]) 
                this.scale.nice();
                this.min = this.scale.domain()[0];  
                this.scale.domain([this.min, this.max])
            }
        }
        this.z.isInDomain = function(value) {
            return (value >= this.min && value <= this.max);
        }
        this.z.isWithinDomain = function(value) {
            return (value > this.min && value < this.max);
        }
    } else {
        this.y = {
            log: false,
            min : hist.values.min,
            max : hist.values.max,
            customised : false,
            scale : null
        }
        this.y.setScaleDomain = function(min,max) {
            if(!this.scale)
                return;
            if(min != null && max != null)
                if(max > min) {
                   this.scale.domain([min, max])
                   this.min = min;
                   this.max = max;
                }
            if(min != null && max == null)
                if(this.max > min) {
                   this.scale.domain([min, this.max])
                   this.min = min;
                }
            if(min == null && max != null)
                if(max > this.min) {
                   this.scale.domain([this.min, max])
                   this.max = max;
                }
        }
        this.y.isInDomain = function(value) {
            return (value >= this.min && value <= this.max);
        }
        this.y.isWithinDomain = function(value) {
            return (value > this.min && value < this.max);
        }
    }
    this.w = null;
    this.h = null;

    Bins = function(plot, data) {
        this.plot = plot;
        this.defWidthX = null;
        this.defWidthY = null;
        this.getXBinBiggerOf= function (value) {
            for(var j=0; j!= this.xLen() ; ++j)
                if(this.lowEdgeX(j) > value) {
                    return j;
                }
            return this.plot.x.max.id-this.plot.x.min.id;
        }
        this.getXValueBiggerOf= function (value) {
            for(var j=0; j!= this.xLen() ; ++j)
                if(this.lowEdgeX(j) > value) {
                    return this.lowEdgeX(j);
                }
            return this.plot.x.max.value;
        }
        this.getXBinBiggerOrEqualOf= function (value) {
            for(var j=0; j!= this.plot.x.max.id-this.plot.x.min.id ; ++j)
                if(this.lowEdgeX(j) >= value)
                    return j;
            return this.plot.x.max.id-this.plot.x.min.id;
        }
        this.getYBinBiggerOf= function (value) {
            for(var j=0; j!= this.plot.y.max.id-this.plot.y.min.id ; ++j)
                if(this.lowEdgeY(j) > value)
                    return j;
            return this.plot.y.max.id-this.plot.y.min.id;
        }
        this.getYValueBiggerOf= function (value) {
            for(var j=0; j!= this.yLen() ; ++j)
                if(this.lowEdgeY(j) > value) {
                    return this.lowEdgeY(j);
                }
            return this.plot.y.max.value;
        }
        this.getYBinBiggerOrEqualOf= function (value) {
            for(var j=0; j!= this.plot.y.max.id-this.plot.y.min.id ; ++j)
                if(this.lowEdgeY(j) >= value)
                    return j;
            return this.plot.y.max.id-this.plot.y.min.id;
        }
        this.binsRow= function(i) {
            return this.data.content[this.plot.y.min.id+i].slice(plot.x.min.id, plot.x.max.id);
        } 
        this.bins = function(i,j) {
            if(data && this.plot.isTH1())
                return this.data.content[this.plot.x.min.id+i];
            if(this.plot.isTH2())
                return this.data.content[this.plot.y.min.id+j][this.plot.x.min.id+i];
            return null;
        };
        this.xLen = function() {
            return plot.x.max.id-plot.x.min.id;
        };
        this.yLen = function() {
            return plot.y.max.id-plot.y.min.id;
        };
        this.data = data
        this.widthX = function (i) {
            if(data)
                if(data.lowEdgeX)
                    return this.lowEdgeX(i+1) - this.lowEdgeX(i); 
                else
                    return this.defWidthX;
        };
        this.lowEdgeX = function (i) {
            if(data)
                if(data.lowEdgeX)
                    if(i == this.xLen())
                        return this.plot.x.max.value;
                    else
                        return data.lowEdgeX[this.plot.x.min.id+i];
                else
                    return plot.x.min.value + this.defWidthX*i;
        };
        if(this.plot.isTH2()) {
            this.binsArray = function() {
                var slicedContent = [];
                for(var i=0; i!=this.yLen(); ++i) {
                    slicedContent.push(this.data.content[plot.y.min.id + i].slice(plot.x.min.id, plot.x.max.id))
                }
                return slicedContent;
            }
            this.lowEdgeY = function (i) {
                if(data && this.plot.isTH2())
                    if(data.lowEdgeY)
                        if(i == this.yLen())
                            return this.plot.y.max.value;
                        else
                            return data.lowEdgeY[this.plot.y.min.id+i];
                    else
                        return plot.y.min.value + this.defWidthY*i;
            };
            this.widthY = function (i) {
                if(data && this.plot.isTH2())
                    if(data.lowEdgeY)
                        return this.lowEdgeY(i+1) - this.lowEdgeY(i); 
                    else
                        return this.defWidthY;
            };
        }
        if(this.plot.isTH1()) {
            this.binsArray = function() {
                return this.data.content.slice(plot.x.min.id, plot.x.max.id);
            }
            this.getFirstValueBiggerThan = function(value) {
                var min = this.plot.y.max;
                for(var i=0; i!= this.xLen(); ++i) {
                    if(this.bins(i) > value && this.bins(i) < min)
                        min = this.bins(i);
                }
                return min;
            }
            this.error = function(i) {
                return this.data.error[i+this.plot.x.min.id]
            }
        }
    }
    this.bins = new Bins(this, hist.bins)
    this.x.bins = this.bins;
    this.y.bins = this.bins;
    var defWidthX,defWidthY;
    if(!hist.bins.lowEdgeX)
        this.bins.defWidthX = (hist.xaxis.last.value - hist.xaxis.first.value) / this.bins.xLen();   
    if(this.isTH2() && !hist.bins.lowEdgeY)
        this.bins.defWidthY = (hist.yaxis.last.value - hist.yaxis.first.value) / this.bins.yLen();
}
function drawMini(content, svg, width, height, start) {
        svg = svg.append("g").attr("transform","scale(0.5)");
        drawHist(content, svg, width*2, height*2, start);
}
//function drawHist(content, div, width, height, start) {
function drawHist(content, svg, width, height, start, url) {

	//height -= 20; width -= 20;
//    var main = div.append("svg").style("display","relative")
    try {
        var json = content.list[0];
        if(!(json.hist.type == "TH2" 
            || json.hist.type == "TProfile2D" 
            || json.hist.type == "TH1"
            || json.hist.type == "TProfile"))
            return;
        var vertMargin = Math.min(Math.max(height/10, 40), 60),
            horiMargin = Math.min(Math.max(width/10, 35), 60);
        var plot = new Plot(json.hist);
        plot.area.margin = {
                top: Math.min(Math.max(height/10, 20), 60),
                right: (plot.isTH2() ? horiMargin: 0), 
                bottom: vertMargin, 
                left: horiMargin
            };
        svg
        .attr("width", width)
        .attr("height", height)
     
        drawTitleArea(svg, json.hist, plot, width)
        
        main = svg
               .append("g")
               .attr("transform", "translate(" + plot.area.margin.left + "," + plot.area.margin.top + ")");
        
        plot.area.width = width - plot.area.margin.left - plot.area.margin.right;
        plot.area.height = height - plot.area.margin.top - plot.area.margin.bottom;
        plot.dqm = content.dqmInfo;
        prepareAxis(main,svg,plot,json.hist);
        drawAxis(main,svg,plot,json.hist);
        if(plot.isTH2()) {
            drawTH2bins(main,json.hist, plot);
        } else {
            var stepBins = drawTH1bins(main, json.hist, plot);
            if(content.list.length > 1) {
                for(var i=1; i < content.list.length; ++i) {
                    var overlayHist = content.list[i].hist;
                    var overlayPlot = new Plot(content.list[i].hist)
                    overlayPlot.x.scale = plot.x.scale;
                    overlayPlot.x.setScaleLimit(plot.x.min.id, plot.x.max.id)
                    if(overlayHist.values.min != 0 || overlayHist.values.max != 0)
                        drawOverlay(main, plot, overlayPlot, stepBins, i)
                }
            }
        }
        drawStats(svg, content, plot, width);

        main.attr("transform", "translate(" + plot.area.margin.left + "," + (plot.area.margin.top) + ")");

        var svgBox =  document.getElementsByTagName("svg")[0].getBBox();
        svg.select("content")
         .style("width", svgBox.width+"px")
         .style("height", svgBox.height+"px")

         svg.append("text")
          .attr("id","generatedIn")
          .text("generated in "+(new Date() - start)+" ms")
          .attr("x",0)
          .attr("y",height - 3)
          .attr("font-size","7px")
          .attr("font-family","Arial")
          .attr("dominant-baseline","auto")//hanging
          if(url != null)
             svg.append("a")
              .attr("id","edit")
              .attr("xlink:href",url)
              .attr("target","_black")
              .attr("transform","translate("+width+","+(height - 3)+")")
             .append("text")
              .text("edit")
              .attr("font-size","12px")
              .attr("font-family","Arial")
              .attr("dominant-baseline","auto")//hanging
              .attr("text-anchor","end")
    } catch (error) {
        drawErrorBox(svg, error, width, height, true)
    }
}
function drawErrorBox(main, error, width, height, hide) {
    errorBox = main.append("g").attr("id","errorBox");
    errorBox.append("rect")
     .attr("x",width*0.05)
     .attr("y",height*0.05)
     .attr("width",width*0.9)
     .attr("height",height*0.9)
     .attr("fill","white")
     .attr("stroke","black")
     
    errorBox.append("text")
     .attr("x",width/2)
     .attr("y",height/4)
     .attr("text-anchor","middle")//begin
     .attr("dominant-baseline","middle")//hanging
     .attr("font-size","14px")
     .attr("font-family","Arial")
     .attr("fill","red")
     .text("ERROR")
     
   var errorField = errorBox.append("text")
     .attr("transform","translate("+width/4+","+height/3+")")
     .attr("font-size","7px")
     .attr("font-family","Arial")
     
   errorField
     .append("tspan")
     .attr("dy", 15)
     .attr("x", 0)
     .text(error.name)
   errorField
     .append("tspan")
     .attr("dy", 15)
     .attr("x", 0)
     .text(error.message)
   if(hide)
       errorBox.append("text")
         .text("X ")
         .attr("dominant-baseline","hanging")
         .attr("text-anchor","end")
         .attr("font-weight", "bold")
         .attr("font-size",15)
         .attr("transform","translate("+(width*0.9-2)+","+(height*0.1+2)+")")
         .attr("class","link")
         .on("click",function() {main.select("#errorBox").attr("display","none")});
} 

function drawTitleArea(main, hist, plot, width) {
    var titleHeight = 0;
    if(hist.title) {
        var title = main.append("g")
            .attr("transform","translate(" + 0 + "," + 0 + ")")
            .attr("id","title")
         .append("text")
            .attr("class","title")
            .attr("dominant-baseline","hanging")
            .attr("text-anchor","begin")            
            .text(hist.title);
        for(var size=2; size>=0; size-=0.1) {
            var titleBox = main.select("#title").node().getBBox();
            if(titleBox.width < (width - 100) && titleBox.height < plot.area.margin.top) {
                plot.area.margin.bottom += plot.area.margin.top - (titleBox.height + 5);
                plot.area.margin.top = titleBox.height + 5;
                break;
            }
            title.style("font-size", size+"em");            
        }
        titleHeight = main.select("#title").node().getBBox().height
        titleWidth = main.select("#title").node().getBBox().width
    } else {
        plot.area.margin.bottom += plot.area.margin.top - 10;
        plot.area.margin.top = 10;
    }
    var statsBoxWidth = 100;
    var statsLabel = 
            main.append("g")
             .attr("id", "stats")
             .attr("transform","translate(" + (width - statsBoxWidth) + "," + 0 + ")")
             .on("click",function() {
                 if(main.select("#statsBox").attr("display") == "none")
                     main.select("#statsBox").attr("display","block");
                 else
                     main.select("#statsBox").attr("display","none");
             }) //
    statsLabel.append("rect")
        .attr("width",statsBoxWidth)
        .attr("height",titleHeight)
        .attr("fill","white")
        .attr("class","link")
//      .attr("stroke","black")
//      .attr("stroke-width", 2);
        
    statsLabel.append("text")
        .text("Stats")
        .attr("class","link")
        .attr("dominant-baseline","middle")
        .attr("text-anchor","middle")
        .attr("transform","translate("+statsBoxWidth/2+","+titleHeight/2+")");
}
function drawXLabels(main, hist, plot) {
    var gLabels = main.select("#xaxisscale")
    var oldLabel = main.select("#xaxisscale").select("text");
    main.select("#xaxisscale")
         .selectAll("text").remove();
    var xAxisBox = main.select("#xaxis").node().getBBox();
    var labelsPosition =  (xAxisBox.y+ xAxisBox.height);
    xLabels = gLabels.selectAll(".label")
    .data(hist.xaxis.labels)
    .enter().append("text")
        .text(function(d) {return d.value;})
        .attr("class","label")
        .attr("text-anchor","middle")//begin
        .attr("dominant-baseline","hanging")//middle
        .attr("x", function(d){return plot.x.scale(d.center);})
        .attr("y", labelsPosition+5)
        .attr("fill",function(d){if(!plot.x.isInDomain(d.center)) {return "white"} else {return "black"}})
    for(var i = 1; i != gLabels.selectAll(".label")[0].length; ++i) {
        var firstLabel = gLabels.selectAll(".label")[0][i-1].getBBox();
        var secondLabel = gLabels.selectAll(".label")[0][i].getBBox();
        if(Math.abs(firstLabel.x + firstLabel.width) > Math.abs(secondLabel.x)) {
             gLabels.selectAll(".label").each(function() {
                 d3.select(this)
                  .attr("text-anchor","begin")
                  .attr("dominant-baseline","middle")                      
                  .attr("transform","translate("+0+","+0+") " +
                                "rotate("+90+" "+d3.select(this).attr("x")+",3"+")")
                  .attr("y", 3);
             })
             var xAxisBox = main.select("#xaxis").node().getBBox();
             if(xAxisBox.height > plot.area.margin.bottom) {
                 var yDelta = (xAxisBox.height - plot.area.margin.bottom) + 10;
                 plot.area.height -= yDelta;
                 plot.y.scale.range([plot.area.height, 0])
                 if(plot.isTH2())
                     plot.z.scale.range([plot.area.height, 0])
                 plot.area.margin.bottom += yDelta;
                 main.select("#xaxis")
                 .attr("transform", "translate(0," + (plot.area.height +1)+ ")")
             }
            break;
        }
    }
}
function  organiseXLabels(main, min) {
    organiseLabels(main, main.select("#xaxis"), min);
    for(var i = 1; i != main.select("#xaxis").selectAll("text")[0].length-1; ++i) {
       x1 = main.select("#xaxis").selectAll("text")[0][i-1];
       x2 = main.select("#xaxis").selectAll("text")[0][i];
       if(!x1 || !x2)
           break;
       var matrix1 = x1.getTransformToElement(x1.nearestViewportElement);
       var matrix2 = x2.getTransformToElement(x2.nearestViewportElement);
        //p defaults to 0,0
        var p = x1.nearestViewportElement.createSVGPoint()
        var sp1 = p.matrixTransform(matrix1);
        var sp2 = p.matrixTransform(matrix2);
        if(sp1.x+ x1.getBBox().width>sp2.x) {
            main.select("#xaxis").selectAll("text")
            .attr("text-anchor","start")
            .attr("dominant-baseline","middle")
            .attr("transform",function(d,i) {
                var width = main.select("#xaxis").selectAll("text")[0][i].getBBox().height;
                return "translate("+width/2+",0) rotate(90 0,"+width/2+")";
            })
            break;
        }
    }
}
function  organiseLabels(main, axisG, min) {
    var s = d3.format("s");
    var minValPrecition = 4;
    if(min < 1)
        minValPrecition = min.toString().length - min.toString().indexOf(".") - 1;
    axisG.selectAll("text")
    .each(
        function(d,i) {
            if(d && d3.select(this).text().indexOf(".") != -1 && d3.select(this).text().length - d3.select(this).text().indexOf(".") > 3) {
                d3.select(this)
                .text( 
                    s(d.toFixed(minValPrecition))
                )
            } 
        }
    )
}
function  organiseYLabels(main, min) {
    organiseLabels(main, main.select("#yaxis"), min);
}
function  organiseZLabels(main, min) {
    organiseLabels(main, main.select("#zaxis"), min);
}
function checkDqmLimits(plot) {
    if(plot.dqm.xAxis.min != null || plot.dqm.xAxis.max != null) {
        if(plot.dqm.xAxis.min < json.hist.xaxis.first.value)
            plot.dqm.xAxis.min = null;
        if(plot.dqm.xAxis.min > json.hist.xaxis.last.value)
            plot.dqm.xAxis.min = null;
        if(plot.dqm.xAxis.max > json.hist.xaxis.last.value)
            plot.dqm.xAxis.max = null;
        if(plot.dqm.xAxis.max < json.hist.xaxis.first.value)
            plot.dqm.xAxis.max = null;
        if(plot.dqm.xAxis.min > plot.dqm.xAxis.max) {
            plot.dqm.xAxis.min = null;
            plot.dqm.xAxis.max = null;
        }
        if(plot.dqm.xAxis.type=="log" && plot.dqm.xAxis.max < 0)
            plot.dqm.xAxis.max = null;
    }
    if(plot.dqm.yAxis.min != null || plot.dqm.yAxis.max != null) {
        if(plot.dqm.yAxis.min > plot.dqm.yAxis.max) {
            plot.dqm.yAxis.min = null;
            plot.dqm.yAxis.max = null;
        }
        if(plot.isTH2()) {
            if(plot.dqm.yAxis.min < json.hist.yaxis.first.value)
                plot.dqm.yAxis.min = null;
            if(plot.dqm.yAxis.min > json.hist.yaxis.last.value)
                plot.dqm.yAxis.min = null;
            if(plot.dqm.yAxis.max > json.hist.yaxis.last.value)
                plot.dqm.yAxis.max = null;
            if(plot.dqm.yAxis.max < json.hist.yaxis.first.value)
                plot.dqm.yAxis.max = null;
            if(plot.dqm.yAxis.type=="log" && plot.dqm.yAxis.max < 0)
                plot.dqm.yAxis.max = null;
        }
    }
    if(plot.dqm.xAxis.max != null || plot.dqm.xAxis.min != null) {
        plot.x.customised = true;
    }
    if(plot.dqm.yAxis.max != null || plot.dqm.yAxis.min != null) {
        plot.y.customised = true;
    }
    if(plot.dqm.zAxis.max != null || plot.dqm.zAxis.min != null) {
        plot.z.customised = true;
    }   
}
function prepareAxis(main, svg, plot, hist) {
    checkDqmLimits(plot); 
    if(plot.dqm.xAxis.type=="log") {
        plot.x.scale = d3.scale.log();
        plot.x.log = true;
    } else { 
        plot.x.scale = d3.scale.linear();
        plot.x.scale.domain([hist.xaxis.first.value, hist.xaxis.last.value]);
    }
    plot.x.scale.range([0, plot.area.width]);
    
    if(plot.isTH1()) {
        if(plot.dqm.yAxis.type=="log") {
            plot.y.scale = d3.scale.log();
            plot.y.log = true;
        } else {
            plot.y.scale = d3.scale.linear();
            plot.y.scale.domain([hist.values.min, hist.values.max]);
        }
        plot.y.scale.range([plot.area.height, 0])
         .clamp(true);
    }
    if(plot.isTH2()) {
        if(plot.dqm.zAxis.type=="log") {
            plot.z.scale = d3.scale.log();
            plot.z.log = true;
        } else {
            plot.z.scale = d3.scale.linear();
            plot.z.scale.domain([hist.values.min, hist.values.max]);
        }
        
        plot.z.scale.range([plot.area.height, 0])
         .clamp(true);
    }
    if(plot.dqm.xAxis.type=="log" && plot.x.min.value <= 0 && !(plot.dqm.xAxis.min > plot.bins.getXValueBiggerOf(0))) {
        var minId = plot.bins.getXBinBiggerOf(0)
        plot.x.setScaleLimit(minId,null);
        plot.x.customised = true;
    } else {
        if(plot.dqm.xAxis.min != null) {
            var minId = plot.bins.getXBinBiggerOf(plot.dqm.xAxis.min)
            plot.x.setScaleLimit(minId-1,null);
            plot.x.customised = true;
        } else {
            plot.x.setScaleLimit(plot.x.min.id,plot.x.max.id);
        }
    }
    if(plot.dqm.xAxis.max != null) {
        var maxId = plot.bins.getXBinBiggerOf(plot.dqm.xAxis.max)
        plot.x.setScaleLimit(null,maxId);
        plot.x.customised = true;
    }
    if(plot.isTH2()) {
        if(plot.dqm.yAxis.type=="log") {
            plot.y.scale = d3.scale.log();
            plot.y.log = true;
        } else {
            plot.y.scale = d3.scale.linear();
            plot.y.scale.domain([hist.yaxis.first.value, hist.yaxis.last.value]);
        }
        plot.y.scale.range([plot.area.height, 0]);
        
        if(plot.dqm.yAxis.type=="log" && plot.y.min.value <= 0 && !(plot.dqm.yAxis.min > plot.bins.getXValueBiggerOf(0))) {
            var minId = plot.bins.getYBinBiggerOf(0)
            plot.y.setScaleLimit(minId,null);
        } else {
            if(plot.dqm.yAxis.min != null) { 
                var minId = plot.bins.getYBinBiggerOf(plot.dqm.yAxis.min)
                plot.y.setScaleLimit(minId-1,null);
            }
        }
        if(plot.dqm.yAxis.max != null) {
            var maxId = plot.bins.getYBinBiggerOf(plot.dqm.yAxis.max)
            plot.y.setScaleLimit(null,maxId);
        }
    }
    if(plot.isTH1()) {
        if(plot.dqm.yAxis.type=="log" && plot.y.min <= 0) {
            var min = plot.bins.getFirstValueBiggerThan(0);
            plot.y.setScaleDomain(min, null);
        }
        if(plot.isTProfile()) {
            var maxArray = [], minArray = [];
            for(var i=0; i!=plot.bins.xLen();++i) {
                if(plot.dqm.yAxis.type!="log" || plot.bins.bins(i)-plot.bins.error(i) > 0)
                    minArray.push(plot.bins.bins(i)-plot.bins.error(i));
                if(plot.dqm.yAxis.type!="log" || plot.bins.bins(i)+plot.bins.error(i) > 0)
                    maxArray.push(plot.bins.bins(i)+plot.bins.error(i));
            }
            plot.y.setScaleDomain(d3.min(minArray),d3.max(maxArray))
        }
       if(plot.dqm.yAxis.type=="log")
           if(plot.dqm.yAxis.min > 0) {
               plot.y.setScaleDomain(plot.dqm.yAxis.min, plot.dqm.yAxis.max)
           } else {
               plot.y.setScaleDomain(null, plot.dqm.yAxis.max)
           }
       else
           plot.y.setScaleDomain(plot.dqm.yAxis.min, plot.dqm.yAxis.max)
    }
    if(plot.isTH1() && (!plot.y.customised)) {
        plot.y.scale.nice()
    }
    if(plot.isTH2()) {
        if(plot.dqm.zAxis.type=="log" && plot.z.min <= 0) {
            if(plot.dqm.zAxis.min > 0) {
                plot.z.setScaleDomain(plot.dqm.zAxis.min, plot.dqm.zAxis.max)
            } else {
                plot.z.setScaleDomain(null, plot.dqm.zAxis.max)
            }
        } else {
            plot.z.setScaleDomain(plot.dqm.zAxis.min, plot.dqm.zAxis.max);
        }
    }
}
function drawAxis(main, svg, plot, hist) {
    var xAxis = d3.svg.axis()
    .scale(plot.x.scale)
    .orient("bottom");
    if(plot.isTH2() && plot.x.max.id - plot.x.min.id < 10) 
        xAxis.ticks(plot.x.max.id - plot.x.min.id +1)
    if(hist.xaxis.labels) {
        xAxis.ticks(plot.x.max.id - plot.x.min.id);
    } else {
        xAxis.tickFormat(d3.format("s"));
    }
   
    var xAxisArea =
        main.append("g")
        .attr("class", "x axis")
        .attr("id","xaxis");
    xAxisArea
     .append("g")
     .attr("id","xaxisscale")
     .call(xAxis);
    xAxisArea.attr("transform", "translate(0," + (plot.area.height +1)+ ")")
    if(hist.xaxis.labels) 
        drawXLabels(svg,hist,plot);  
    else 
        organiseXLabels(svg, plot.x.min.value)
        
    if(hist.xaxis.title) {
        var xAxisBox = svg.select("#xaxis").node().getBBox();
        var xtitle = svg.select("#xaxis").append("text")
            .text(hist.xaxis.title)
            .attr("transform","translate("+(plot.area.width /*+ plot.area.margin.right*/)+", "+(xAxisBox.y+ xAxisBox.height)+")")
            .attr("text-anchor","end")
            .attr("dominant-baseline","hanging")            
            .attr("id","xtitle")
            .attr("class","axistitle")
            .attr("x",0)
            .attr("y",0);
/*      for(var size=1.5; size>=0; size-=0.1) {
            ;
            if(svg.select("#xtitle").node().getBBox().width < plot.area.width 
                    && svg.select("#xaxis").node().getBBox().height < plot.area.margin.bottom)
                break;
            xtitle.style("font-size", size+"em");           
        }*/
    }

    
    var yAxis = d3.svg.axis()
        .scale(plot.y.scale)
        .orient("left");
    if(plot.isTH2()  && plot.y.max.id - plot.y.min.id < 10)
        yAxis.ticks(plot.y.max.id - plot.y.min.id + 1)
    if(plot.isTH2() && hist.yaxis.labels) {
        yAxis.ticks(plot.y.max.id - plot.y.min.id); 
    } else {
        yAxis.tickFormat(d3.format("s"));
    }
    var s = d3.format("s");
    var yAxisArea = main.append("g")
        .attr("class", "y axis")
        .attr("id","yaxis")
    yAxisArea
    .append("g")
    .attr("id","yaxisscale")
    .call(yAxis);
    
    organiseYLabels(svg, plot.y.min.value || plot.y.min)
    
    yAxisArea.attr("transform", "translate(-1,-1)")
/*    if(isTH1(hist.type) && content.dqmInfo.yAxis.type == "log") {
        var logLabel = svg.select("#yaxisscale")
                .selectAll("text")[0];
        for(var i = 1; i < logLabel.length; ++i) {
            var firstLabel = logLabel[i-1].getBBox();
            var secondLabel = logLabel[i].getBBox();
            if(firstLabel.y + firstLabel.height > secondLabel.y) {
                svg.select("#yaxisscale")
                .selectAll("text").each(function(){
                    if(d3.select(this).text().toString()[0] != "1")
                        d3.select(this).style("display","none")
                })
                break;
            }
        }
    }*/
    if(plot.isTH2() &&  hist.yaxis.labels) {
        var gLabels = svg.select("#yaxisscale")
        svg.select("#yaxisscale")
             .selectAll("text").remove();
        var labelsPosition =  (svg.select("#yaxisscale").node().getBBox().x);
        yLabels = gLabels.selectAll(".label")
        .data(hist.yaxis.labels)
        .enter().append("text")
            .text(function(d) {if(plot.y.isInDomain(d.center)) return d.value;})
            .attr("class","label")
            .attr("text-anchor","end")
            .attr("dominant-baseline","middle")         
            .attr("transform",function(d) {return "translate("+(labelsPosition-5)+","+plot.y.scale(d.center)+")"+" rotate(0)"});
    }
    
    if(hist.yaxis && hist.yaxis.title) {
        var ytitle = svg.select("#yaxis").append("g").attr("id","gytitle").append("text")
            .text(hist.yaxis.title)
            .attr("transform","translate("+
                    (svg.select("#yaxis").node().getBBox().x-5)+", 0)  rotate(-90)")
            .attr("text-anchor","end")
            .attr("dominant-baseline","auto")//hanging
            .attr("class","axistitle")
            .attr("id","ytitle")
            .attr("x",-10)
            .attr("y",0);
    }
    var yTitleSize = 1.5;
    var yLabelSize = 10;
    
    var titleBox = svg.select("#title").node().getBBox();
    //try only 5 times to corect it if not possible leave it
    for (var i = 0; i<5 &&(svg.select("#yaxis").node().getBBox().height > plot.area.height
            || svg.select("#yaxis").node().getBBox().width > plot.area.margin.left); ++i) {
        if(hist.yaxis && hist.yaxis.title && svg.select("#ytitle").node().getBBox().width > plot.area.height) {
            yTitleSize =yTitleSize*plot.area.height/svg.select("#ytitle").node().getBBox().width - 0.05;
            ytitle.style("font-size", yTitleSize+"em");
        }
        if(svg.select("#yaxis").node().getBBox().width > plot.area.margin.left) {
            yLabelSize =Math.round(yLabelSize*(plot.area.margin.left/svg.select("#yaxis").node().getBBox().width - 0.05));
//          yLabels.style("font-size", yLabelSize+"px");
            svg.select("#yaxisscale").selectAll("text").style("font-size", yLabelSize+"px");
            if(ytitle) {
                yTitleSize -= 0.2; 
                ytitle.attr("transform","translate("+(svg.select("#yaxisscale").node().getBBox().x)+", "+0+")  rotate(-90)")
                .style("font-size", yTitleSize+"em");
            }
        }
    }
    plot.area.margin.left = svg.select("#yaxis").node().getBBox().width;
    
    var xTitleSize = 1.5;
    var xLabelSize = 10;
    var xTickSize = 6;
    var i;
    for (i = 0; i<5 &&(svg.select("#xaxis").node().getBBox().width > plot.area.width
            || svg.select("#xaxis").node().getBBox().height > plot.area.margin.bottom); ++i) {
        if(hist.xaxis.title && svg.select("#xtitle").node().getBBox().width > plot.area.width) {
            xTitleSize =xTitleSize*plot.area.width/svg.select("#xtitle").node().getBBox().width - 0.05;
            xtitle.style("font-size", xTitleSize+"em");
        }
        if(svg.select("#xaxis").node().getBBox().height > plot.area.margin.bottom) {
            xLabelSize -= 1;
            xTickSize -= 0.5;
            svg.select("#xaxis").selectAll(".tick")
              .attr("y2",xTickSize);
            if(hist.xaxis.labels) {
                var labelsPosition =  (xAxisBox.y);
                xLabels
                 .style("font-size", xLabelSize+"px")
            } else
                svg.select("#xaxisscale").selectAll("text")
                 .style("font-size", xLabelSize+"px")
                 .attr("y",xTickSize+3);
            var xLabelBox = svg.select("#xaxisscale").node().getBBox();
            if(xtitle) {
                xTitleSize -= 0.2; 
                xtitle
                 .attr("transform","translate("+(plot.area.width/*+ plot.area.margin.right*/)+", "+(xLabelBox.y+xLabelBox.height)+")")
                 .style("font-size", xTitleSize+"em");
            }
        }
    }
}
function colorOfNo(no) {
    switch(no) {
    case 0: return "black"
    case 1: return "blue"
    case 2: return "orange"
    case 3: return "red"
    case 4: return "violet"
    }
}

function drawTH1bins(main, hist, plot) {
    stepBins = [];
    for(var i = 0; i!=plot.bins.xLen(); ++i) {
        var tmp = {};
        var value = plot.bins.bins(i);
        tmp.x = plot.bins.lowEdgeX(i);
        tmp.width = plot.bins.widthX(i);
        tmp.error = plot.bins.error(i);
        if(plot.isTProfile() && value == 0 && plot.bins.error(i) == 0) {
             
        }
        else {
            if(plot.y.log && value <= 0) {
                tmp.y = plot.y.min;                
            } else {
                tmp.y = value;
            }
            stepBins.push(tmp);   
        }
    }
    var binsTable;
    var cells = main 
        .append("g")
         .style("width", plot.area.width)
         .style("heigth", plot.area.heigth)
         
    cells.selectAll(".bin").data(stepBins).enter().append("line")
     .attr("class","bin")
     .attr("x1",function(d) {return plot.x.scale(d.x)})
     .attr("x2",function(d) {return plot.x.scale(d.x + d.width)})
     .attr("y1",function(d) {return plot.y.scale(d.y)})
     .attr("y2",function(d) {return plot.y.scale(d.y)})
     .attr("stroke",function(d) {
         if(d.y < plot.y.min || d.y > plot.y.max ) {
             return "none"
         } else {
             return "black"}
     })
     
    if(plot.isTProfile())
        cells.selectAll(".error").data(stepBins).enter().append("line")
         .attr("class","error")
         .attr("x1",function(d) {return plot.x.scale(d.x + d.width/2)})
         .attr("x2",function(d) {return plot.x.scale(d.x + d.width/2)})
         .attr("y1",function(d) {return plot.y.scale(d.y-d.error)})
         .attr("y2",function(d) {return plot.y.scale(d.y+d.error)})
         .attr("stroke","black")
     else {
         var lastVal = plot.y.scale.domain()[0];
         for(var i=0; i!=stepBins.length; ++i) {
             var d = stepBins[i];
             cells.append("line")
             .attr("class","vertical")
             .attr("x1",plot.x.scale(d.x))
             .attr("x2",plot.x.scale(d.x))
             .attr("y1",plot.y.scale(lastVal))
             .attr("y2",plot.y.scale(d.y))
             .attr("stroke","black")
             lastVal = d.y;
         }
         var d = stepBins[stepBins.length-1];
         cells.append("line")
         .attr("class","vertical")
         .attr("x1",plot.x.scale(d.x+d.width))
         .attr("x2",plot.x.scale(d.x+d.width))
         .attr("y1",plot.y.scale(lastVal))
         .attr("y2",plot.y.scale(lastVal))
         .attr("stroke","black")

     }
    return stepBins;
}

function drawTH2bins(main, hist, plot) {
    var range = plot.area.height;
    var zColor = 
        d3.scale.linear()
        .domain([range, range*4/5, range*3/5, range*2/5, range*1/5,  0])
        .range(["rgb(255,0,255)","rgb(0,0,255)","rgb(0,255,255)", "rgb(0,255,0)","rgb(255,255,0)", "rgb(255,0,0)"])
       .interpolate(d3.interpolateRgb);    

    var rects = 
          main
          .append("g")
           .attr("id","cells")
           .attr("width", plot.area.width)
           .attr("heigth", plot.area.heigth)
           .selectAll(".row")
           .data(plot.bins.binsArray())
           .enter()
          .append("g")
           .attr("id","row")
           .selectAll(".point")
           .data(function(d){return d;})
           .enter()
          .append("rect")
           .attr("id",function(d,i,j) {return "point"+i+j;})
           .style("fill",function(d,i,j) {if(d<plot.z.min || d==0) {return "rgb(255,255,255)";} else {if(d>plot.z.max) {return zColor(plot.z.scale(plot.z.max))} else {return zColor(plot.z.scale(d));}}});

    rects
    .attr("x",function(d,i,j) {return plot.x.scale(plot.bins.lowEdgeX(i));})
    .attr("width",function(d,i,j) {return plot.x.scale(plot.bins.lowEdgeX(i+1)) - plot.x.scale(plot.bins.lowEdgeX(i));})
    .attr("y",function(d,i,j) {return plot.y.scale(plot.bins.lowEdgeY(j+1));})
    .attr("height",function(d,i,j) {return plot.y.scale(plot.bins.lowEdgeY(j)) - plot.y.scale(plot.bins.lowEdgeY(j+1));})
                     
    var cells = main.select("#cells").node().getBBox();
    if(plot.z.max != plot.z.min) {
        var values = d3.range(0,400,2)
        var xx = d3.scale.ordinal()
           .domain(values)
           .rangeBands([0,cells.height],0);
        
        main.append("g")
         .attr("id","zaxis")
         .selectAll("rect")
           .data(values)
         .enter().append("rect")
            .attr("x", cells.x + cells.width + 1)
            .attr("y", xx)
            .attr("height", xx.rangeBand())
            .attr("width", plot.area.margin.right * 0.3)
            .style("fill", zColor)
        
        var zAxis = d3.svg.axis()
            .scale(plot.z.scale)
            .tickSize(10)
            .orient("right")
            .tickFormat(d3.format("s"));
        var zAxisBox = main.select("#zaxis").node().getBBox();
        
        main.select("#zaxis").append("g")
            .attr("class", "z axis")
            .attr("transform", "translate("+(zAxisBox.x+zAxisBox.width)+",0)")
            .attr("id","zaxis")
            .call(zAxis);
        organiseZLabels(main, plot.z.min)
        var zTitleSize = 1.5;
        var zLabelSize = 10;
        var zTicksLane = 10;
        var i;
        for (i = 0; i<5 && main.select("#zaxis").node().getBBox().width > plot.area.margin.right; ++i) {
            zLabelSize--;
            main.select("#zaxis").selectAll("text")
              .style("font-size", zLabelSize+"px")
              .attr("x",zLabelSize+3);
            main.select("#zaxis").selectAll(".tick")
              .attr("x2",zLabelSize)
        }
    }
}
function drawStats(svg,content,plot, width) {
    var hist = content.list[0].hist;
    var titleBox = svg.select("#title").node().getBBox();  
    var statsBoxWidth = 150, statsBoxHeight = 220; 
    var statsBox = 
        svg.append("g")
         .attr("id","statsBox")
         .attr("transform","translate(" + (width - statsBoxWidth) + "," + plot.area.margin.top + ")");
    var mainHistStats =  
        statsBox.append("g")
            .attr("id", "mainHistStats")
            .attr("transform","translate(0,18)");
    drawStatsBox(svg, plot, hist, mainHistStats,statsBoxWidth,0)
    if(plot.isTH1()) {
        var statsBoxSize = svg.select("#mainHistStats").node().getBBox();
        var deltaY = 18;
        for(var i=1; i < content.list.length; ++i) {
            deltaY += statsBoxSize.height;
            var overlayHist = content.list[i].hist;
            var mainHistStats =  
                statsBox.append("g")
                    .attr("id", "histStats"+i)
                    .attr("transform","translate(0,"+(deltaY)+")");
            drawStatsBox(svg, new Plot(overlayHist), overlayHist, mainHistStats,statsBoxWidth,i)
            statsBoxSize = svg.select("#histStats"+i).node().getBBox();
        }    
    }
    statsBox.attr("display","none");
    statsBox.append("text")
        .text("X ")
        .attr("dominant-baseline","hanging")
        .attr("text-anchor","end")
        .attr("font-weight", "bold")
        .attr("font-size",15)
        .attr("transform","translate("+(statsBoxWidth-2)+","+2+")")
        .attr("class","link")
        .on("click",function() {d3.select("#statsBox").attr("display","none")});
}
function drawStatsBox(svg, plot, hist, statsBox, statsBoxWidth, no) {
    var statsRect = statsBox.append("rect")
        .attr("width",statsBoxWidth)
        .attr("height",10)
        .attr("fill","white")
        .attr("stroke","black")
        .attr("stroke-width", 2);

    var statsNames = statsBox.append("text")
        .attr("id", "statsNames")
        .attr("font-weight", "bold")
        .attr("transform","translate("+(2)+","+0+")")
        .attr("fill",colorOfNo(no));
    var statsVals = statsBox.append("text")
        .attr("id", "statsVals")
        .attr("fill",colorOfNo(no))
        .attr("transform","translate("+(2)+","+0+")")
    for(var k in hist.stats) {
        statsNames
        .append("tspan")
         .attr("dy",15).attr("x",0)
         .text(k);        
        if( typeof hist.stats[k] != "object") {
           statsVals
            .append("tspan")
    //        .attr("transform","translate("+(statsBoxWidth-2)+","+20+")")
            .attr("text-anchor","end")
            .attr("dy",15).attr("x",statsBoxWidth-5)
            .text(hist.stats[k]);
        } else {
            var wasObj = true;
            for(var axis in hist.stats[k]) {
                if(!wasObj) {
                    statsNames
                    .append("tspan")
                     .attr("dy",15).attr("x",0)
                     .text(" ");
                }
                wasObj = false;
                statsNames
                .append("tspan")
                 .attr("x",32)
                 .text(axis);
                var wasSubObj = true;                
                for(var vals in hist.stats[k][axis]) {
                    if(!wasSubObj) {
                        statsNames
                        .append("tspan")
                         .attr("dy",15).attr("x",0)
                         .text(" ");
                    }
                    wasSubObj = false;
                    statsNames
                    .append("tspan")
                     .attr("x",45)
                     .text(vals);
                    
                    statsVals
                    .append("tspan")
        //            .attr("transform","translate("+(statsBoxWidth-2)+","+20+")")
                    .attr("text-anchor","end")
                    .attr("dy",15).attr("x",statsBoxWidth-5)
                    .text(hist.stats[k][axis][vals]);
                }
            }
        }
        statsNamesBox = statsBox.node().getBBox();
        statsBoxHeight= statsNamesBox.y+statsNamesBox.height;
       
        var xDelta = 0; 
        var statsValsBox = svg.select("#statsVals").node().getBBox();
          
        if(statsNamesBox.x > statsValsBox.x) {
            xDelta = statsValsBox.x - 30;
        } else {
            if(statsNamesBox.x + 30 > statsValsBox.x) {
                xDelta = (statsNamesBox.x - 30);
            }
        }
        if (xDelta != 0) {
            statsRect.attr("x", xDelta )
                     .attr("width", statsBoxWidth + Math.abs(xDelta));
            statsNames.attr("transform", "translate("+(xDelta + 2)+ ",18)");
        }
        statsRect.attr("height", statsBoxHeight+2);
    }
    
    var statsNamesBox = svg.select("#statsNames").node().getBBox();
    var statsBoxHeight= statsNamesBox.y + statsNamesBox.height+18;
    if(plot.isTH2()) {
        statsNames
        .append("tspan")
         .attr("dy",15).attr("x",0)
         .text("integral");
        statsNamesBox = svg.select("#statsNames").node().getBBox();
        statsBoxHeight= statsNamesBox.y + statsNamesBox.height+18;
        var pos = 0;
    
        var integral = 
                statsBox
                 .append("g")
                 .attr("id","integral");
        var integralRow1 =
                integral
                 .append("text")
                 .attr("text-anchor","end")
                 .attr("transform","translate("+(statsBoxWidth-5)+","+statsBoxHeight+")");
        
        var integralRow2 =             
                integral
                 .append("text")
                 .attr("text-anchor","end")
                 .attr("transform","translate("+(statsBoxWidth-5)+","+(statsBoxHeight+15)+")");
        
        var integralRow3 = 
                integral
                 .append("text")
                 .attr("text-anchor","end")
                 .attr("transform","translate("+(statsBoxWidth-5)+","+(statsBoxHeight+30)+")");
        var pos = 0;
        for(var i = 2 ; i!= -1 ; --i) {
            integralRow1 
            .append("tspan")
            .attr("x", pos)
            .text(hist.bins.integral[0][i]);
            
            integralRow2 
             .append("tspan")
            .attr("x", pos)
             .text(hist.bins.integral[1][i]);
             
            integralRow3 
            .append("tspan")
            .attr("x", pos)
            .text(hist.bins.integral[2][i]);
            
            pos = -(svg.select("#integral").node().getBBox().width+10);
        }
        statsRect.attr("height", statsBoxHeight+2+svg.select("#integral").node().getBBox().height);
    }
}
function drawOverlay(svg, plot, overlayPlot, stepBins, no) {
    var plotArea = svg.append("g").attr("id","overlay "+ no);
    plotArea.selectAll(".point").data(stepBins).enter().append("circle")
    .attr("class","point")
    .attr("cx",function(d) {return plot.x.scale(d.x+d.width/2)})
    .attr("cy",function(d,i) {return plot.y.scale(overlayPlot.bins.bins(i)) })
    .attr("r",3)
    .attr("id",function(d,i) {return i})
    .attr("value",function(d,i){return overlayPlot.bins.bins(i)})
    .attr("fill",function(d,i) {
        if( overlayPlot.bins.error(i) == 0
            || overlayPlot.bins.bins(i) > plot.y.scale.domain()[1] 
            || overlayPlot.bins.bins(i) < plot.y.scale.domain()[0]) {
            return "none"
        } else {
            return colorOfNo(no)
        };
    });

    plotArea.selectAll(".error").data(stepBins).enter().append("line")
    .attr("class","error")
    .attr("x1",function(d) {return plot.x.scale(d.x+d.width/2)})
    .attr("x2",function(d) {return plot.x.scale(d.x+d.width/2)})
    .attr("y1",function(d,i) {return plot.y.scale(((overlayPlot.bins.bins(i) -overlayPlot.bins.error(i)))) })
    .attr("y2",function(d,i) {return plot.y.scale(((overlayPlot.bins.bins(i) +overlayPlot.bins.error(i)))) })
    .attr("stroke",colorOfNo(no));
    
    plotArea.selectAll(".errorTop").data(stepBins).enter().append("line")
    .attr("class","errorTop")
    .attr("x1",function(d) {return plot.x.scale(d.x+d.width/2)-2})
    .attr("x2",function(d) {return plot.x.scale(d.x+d.width/2)+2})
    .attr("y1",function(d,i) {return plot.y.scale(((overlayPlot.bins.bins(i) +overlayPlot.bins.error(i)))) })
    .attr("y2",function(d,i) {return plot.y.scale(((overlayPlot.bins.bins(i) +overlayPlot.bins.error(i)))) })
    .attr("stroke",function(d,i) {
        if(overlayPlot.bins.error(i) == 0 
                || (overlayPlot.bins.bins(i) +overlayPlot.bins.error(i)) > plot.y.scale.domain()[1] 
                || (overlayPlot.bins.bins(i) +overlayPlot.bins.error(i)) < plot.y.scale.domain()[0]) {
            return "none";
        } else {
            return colorOfNo(no)
        }
     });
    
    plotArea.selectAll(".errorBottom").data(stepBins).enter().append("line")
    .attr("class","errorBottom")
    .attr("x1",function(d) {return plot.x.scale(d.x+d.width/2)-2})
    .attr("x2",function(d) {return plot.x.scale(d.x+d.width/2)+2})
    .attr("y1",function(d,i) {return plot.y.scale(((overlayPlot.bins.bins(i) -overlayPlot.bins.error(i)))) })
    .attr("y2",function(d,i) {return plot.y.scale(((overlayPlot.bins.bins(i) -overlayPlot.bins.error(i)))) })       
    .attr("id",function(d,i) {return i})
    .attr("stroke",function(d,i) {
        if(overlayPlot.bins.error(i) == 0 
                || (overlayPlot.bins.bins(i) -overlayPlot.bins.error(i)) > plot.y.scale.domain()[1]
                || (overlayPlot.bins.bins(i) -overlayPlot.bins.error(i)) < plot.y.scale.domain()[0]) {
            return "none"
        } else {
            return colorOfNo(no)
        }
     });
}
function redrawCheck(prevZoomData, w, h) {
    if(prevZoomData.json == null || (Math.abs(h - prevZoomData.h) < 5 && Math.abs(w - prevZoomData.w) < 5))
        return;
    prevZoomData.w = w-2;
    prevZoomData.h = h-1;
    redraw(prevZoomData, w-2, h-1, prevZoomData.url.replace("jsonfairy","editfairy"));
}
function redraw(prevZoomData, w, h, url) {
    var start = new Date();
    var extJsonWinDiv = d3.select("#jsonPlotDiv");
    extJsonWinDiv.select("svg").remove()
    var svg = extJsonWinDiv.append("svg").attr("width",w).attr("height",h)
    drawHist(prevZoomData.json, svg, w, h, start, url);
}
function draw(json) {
    var start = new Date();
    if(parent && parent.GUI) {
        var extWin = parent.GUI.Plugin.DQMCanvas.getJsonWin();
        svgWidth = extWin.getInnerWidth()-20/**0.95*/; svgHeight  =  extWin.getInnerHeight()-20/**0.95*/;
    } else {
        svgWidth = svgHeight = 500;
    }
    var div= d3.select("body")
               .append("div")
               .attr("id","content")
    
    drawHist(json, div, svgWidth, svgHeight, start);
    if(!(parent && parent.GUI)) {
        var div2= d3.select("body")
            .append("div")
            .attr("id","content2")
            
        drawHist(json, div2, svgWidth/2, svgHeight/2, start);
    }
}
function drawZoomPlot(dataUrl, prevZoomData, w, h) {
    if(dataUrl == null || dataUrl == "" || w < 10 || h < 10)
        return;
	if(w == null && h == null) {
		w = prevZoomData.w;
		h = prevZoomData.h;
	}
	if(prevZoomData.url == dataUrl && Math.abs(h - prevZoomData.h) < 5 && Math.abs(w - prevZoomData.w) < 5)
		return;

    prevZoomData.w = w;
    prevZoomData.h = h;
	var extJsonWinDiv = d3.select("#jsonPlotDiv");
	if(dataUrl != prevZoomData.url) {
		d3.json(dataUrl, function(data) {
		    extJsonWinDiv.selectAll("*").remove();
            var svg = 
                extJsonWinDiv.append("svg")
                .attr("height",h)
                .attr("width",w);
			prevZoomData.url = dataUrl;
			prevZoomData.json = data;
			drawHist(data, svg , w, h, new Date(), dataUrl.replace("jsonfairy","editfairy"));
		})
	} else {
		redraw(prevZoomData,w,h, dataUrl.replace("jsonfairy","editfairy"));
	}
}
