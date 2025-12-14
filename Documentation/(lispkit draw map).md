# LispKit Draw Map

Library `(lispkit draw map)` provides an API for creating map snapshots. A map snapshot encapsulates a map image and provides a procedure for mapping locations to points on the image. This makes it possible to draw on top of the image based on locations (lat/longs). Here is a typical use case for this library:

```scheme
(import (lispkit base)
        (lispkit draw)
        (lispkit location)
        (lispkit draw map)
        (lispkit thread future))

;; Draws a map around location future `center` covering `area`
;; (a size object in meters) and return and image of size
;; `dims` (in points).
(define (map-drawing center area dims)
  (let*
    (; Create a map snapshot for the `area` around `center` of `size`
     ; displaying satellite footage
     (snapsh (make-map-snapshot (future-get center) area dims 'satellite))
     ; Create an image from the snapshot
     (image  (map-snapshot-image (future-get snapsh)))
     ; Determine the points on the map image for the center
     (pt     (map-snapshot-point (future-get snapsh) (future-get center))))
    ; Create a drawing of the map with highlighted center
    (drawing
      ; Draw the map at the origin of the drawing
      (draw-image image (point 0 0))
      ; Highlight the center with a red circle
      (set-fill-color red)
      (fill-ellipse
        (rect (point (- (point-x pt) 5) (- (point-y pt) 5))
              (size 10 10))))))

;; Save a drawing for a map snapshot of size 800x800 (in points)
;; for a square map segment of 500m x 500m around the current location.
(save-drawing
  "~/Downloads/my-map.pdf"
  (map-drawing (current-location) (size 500 500) (size 800 800))
  (size 800 800))
```

The body of the `let*` form first draws the image and then layers a red ellipse on top. This is done in the context of a drawing, which can then be turned into PDF format and saved into a file.


**(map-snapshot? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a map snapshot object; otherwise `#f` is returned.

**(make-map-snapshot _center dist size_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-map-snapshot _center dist size type_)**  
**(make-map-snapshot _center dist size type poi_)**  
**(make-map-snapshot _center dist size type poi bldng_)**  

Returns a future for a new map snapshot object which represents a rectangular area of a map whose center is the location _center_. Locations are created and managed via library `(lispkit location)`. _dist_ describes the width and height of the map region. If is either a `lat-long-span` object or a size object, as defined by library `(lispkit draw)`. `lat-long-span` objects describe a width and height in terms of a north-to-south and east-to-west distance measured in degrees. size objects are interpreted as width and height measured in meters. _size_ is a size object describing the dimensions of the image in points. _type_ is a symbold that indicates the map type. Supported are:

- `standard`: Street map that shows the position of all roads and some road names.
- `satellite`: Satellite imagery of the area.
- `satellite-flyover`: Satellite image of the area with flyover data where available.
- `hybrid`: Satellite image of the area with road and road name information layered on top.
- `hybrid-flyover`: Hybrid satellite image with flyover data where available.
- `standard-muted`: Street map where the underlying map details are less emphasized to make custom data on top stand out more.

_poi_ is a list of symbols indicating the categories for which point of interests are highlighted on the map. The following categories are supported:

`airport`, `amusement-park`, `aquarium`, `atm`, `bakery`, `bank`, `beach`, `brewery`, `cafe`, `campground`, `car-rental`, `ev-charger`, `fire-station`, `fitness-center`, `supermarket`, `gas-station`, `hospital`, `hotel`, `laundry`, `library`, `marina`
`movie-theater`, `museum`, `national-park`, `nightlife`, `park`, `parking`, `pharmacy`, `police`, `post-office`, `public-transport`, `restaurant`, `restroom`, `school`, `stadium`, `store`, `theater`, `university`, `winery`, `zoo`.

_bldng_ is a boolean parameter (default is `#f`) indicating whether to show buildings or not.

**(map-snapshot-image _msh_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Given a map snapshot object _msh_, procedure `map-snapshot-image` returns an image of the map encapsulated by _msh_.

**(map-snapshot-point _msh loc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(map-snapshot-point _msh lat long_)**  

Given a map snapshot object _msh_, procedure `map-snapshot-point` returns a point on the image of the map that matches the given location _loc_, or the location derived from the given latitude _lat_ and longitude _long_.

**(lat-long-span _latspan longspan_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new `lat-long-span` object from the given latitudal (north-to-south) and longitudal (east-to-west) distances _latspan_ and _longspan_ measured in degrees.
