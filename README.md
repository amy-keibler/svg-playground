# svg-playground

[![Build Status](https://travis-ci.org/matt-keibler/svg-playground.svg?branch=master)](https://travis-ci.org/matt-keibler/svg-playground)

## Purpose
This tool was created for generating SVG animations for game assets. SVG was chosen because it provides an easy to manipulate interface and allows assets to be scaled up to accommodate higher resolutions without a loss of clarity. 

## Using SVG-Playground
To run the program with stack, execute `stack exec svg-playground -- <path to JSON file>`. This will create SVG files based on the configuration found in the JSON file. SVG files can be converted to PNG files using Inkscape via the command `inkscape -z -e <filename>.png -w <width> -h <height> <filename>.svg`.

### Configuration Options
* filename: the path of the files to be created, or `/tmp/animation` if not configured
** this creates files `animation0.svg` through `animation<numFrames -1>.svg`
* frameWidth, frameHeight: the width and height of the svg in pixels, or 64 if not configured
* numFrames: the number of animation frames to create, or 10 if not configured
* staticShapes: a list of shapes that are not interpolated (described further below)
* animatedShapes: a list of shapes that are interpolated (described further below)

### Shape JSON Configuration
Static shapes require a single shape and a style. Styles are in the form:
`{"stroke": "black", "strokeWidth": "2", "fill": "white", "fillOpacity": "1"}`
The numbers are strings because they are passed directly to the SVG as string values.

The following shapes are supported:
* Line -> `{"line": {"start": {"x": 0, "y": 0}, "end": {"x": 1, "y":1}}, "style": {...}}`
* Rectangle -> `{"rect": {"topLeft": {"x": 0, "y": 0}, "bottomRight": {"x": 1, "y":1}}, "style": {...}}`
* Circle -> `{"circle": {"center": {"x": 0, "y": 0}, "radius": 4}, "style": {...}}`
* Ellipse -> `{"ellipse": {"ellipseCenter": {"x": 0, "y": 0}, "rX": 4, "rY": 2, "style": {...}}`

Animated shapes work the same way, except there is a start and end shape that are the start and end points for the interpolation.
* Line -> `{"startLine": {...}, "endLine": {...}, "style": {...}}`
* Rectangle -> `{"startRect": {...}, "endRect": {...}, "style": {...}}`
* Circle -> `{"startCircle": {...}, "endCircle": {...}, "style": {...}}`
* Ellipse -> `{"startEllipse": {...}, "endEllipse": {...}, "style": {...}}`

Examples can be found under `data/`
