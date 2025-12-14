# LispKit Location

Library `(lispkit location)` implements procedures for geocoding and reverse geocoding and provides representations of _locations_ (latitude, longitude, altitude) and _places_ (structured representation of addresses). Since geocoding operations take some time, procedures typically return Futures as implemented by library `(lispkit thread future)`.


## Locations

A _location_ consists of a latitude, a longitude, and an optional altitude. Locations are represented as lists of two or three flonum values.

**(location? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the given expression _obj_ is a valid location; returns `#f` otherwise.

**(location _latitude longitude_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(location _latitude longitude altitude_)**  

Creates a location for the given _latitude_, _longitude_, and _altitude_. This procedure fails with an error if any of the provided arguments are not flonum values.

**(current-location)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a future which eventually refers to the current device location. If a device location can't be determined, the returned future will refer to `#f`. Similarly, the returned future will refer to `#f` if the user did not authorize the device to reveal the location.

**(location-latitude _loc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the latitude of location _loc_.

**(location-longitude _loc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the longitude of location _loc_.

**(location-altitude _loc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the altitude of location _loc_. Since altitudes are optional, procedure `location-altitude` returns `#f` if the altitude is undefined.

**(location-distance _loc1 loc2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the distance between location _loc1_ and location _loc2_ in meters.

**(location-\>timezone _loc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a timezone identifier (string) for location _loc_.


## Places

A _place_ is a structured representation describing a place on Earth. Its main components are address components, but a place might also provide meta-information such as the timezone of the place or the ISO country code. Library `(lispkit date-time)` provides more functionality to deal with such meta-data. Places are represented as lists of one to ten strings in the following order:

   1. ISO country code
   2. Country
   3. Region (a part of the country; e.g. State, Bundesland, Kanton, etc.)
   4. Administrational region (a part of the region; e.g. County, Landkreis, etc.)
   5. Postal code
   6. City
   7. Locality (a part of the city; e.g. District, Stadtteil, etc.)
   8. Street
   9. Street number
 10. Time zone

Note that all components are optional. An optional component is represented as `#f`.

**(place? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the given expression _obj_ is a valid place; returns `#f` otherwise.

**(place _code_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(place _code country_)**  
**(place _code country region_)**  
**(place _code country region admin_)**  
**(place _code country region admin zip_)**  
**(place _code country region admin zip city_)**  
**(place _code country region admin zip city locality_)**  
**(place _code country region admin zip city locality street_)**  
**(place _code country region admin zip city locality street nr_)**  
**(place _code country region admin zip city locality street nr tz_)**  

Returns a location for the given components of a place. Each component is either `#f` (= undefined) or a string.

**(place-country-code _pl_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the country code for place _pl_ as a string or `#f` if the country code is undefined.

**(place-country _pl_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the country for place _pl_ as a string or `#f` if the country is undefined.

**(place-region _pl_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the region for place _pl_ as a string or `#f` if the region is undefined.

**(place-admin _pl_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the administrational region for place _pl_ as a string or `#f` if the administrational region is undefined.

**(place-postal-code _pl_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the postal code for place _pl_ as a string or `#f` if the postal code is undefined.

**(place-city _pl_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the city for place _pl_ as a string or `#f` if the city is undefined.

**(place-locality _pl_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the locality for place _pl_ as a string or `#f` if the locality is undefined.

**(place-street _pl_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the street for place _pl_ as a string or `#f` if the street is undefined.

**(place-street-number _pl_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the street number for place _pl_ as a string or `#f` if the street number is undefined.

**(place-timezone _pl_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the timezone for place _pl_ as a string or `#f` if the timezone is undefined.


## Geocoding

**(geocode _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(geocode _obj locale_)**  

Returns a future for a list of locations at the given place or address. _obj_ is either a valid place representation or it is an address string. _locale_ is a symbol representing a locale, which is used to interpret the given place or address. The future returned by `geocode` signals an error if the geocoding operation fails, e.g. if there is no network access.

```scheme
(future-get (geocode "Brandschenkestrasse 110, Zürich" 'de_CH))
⇒ ((47.365535 8.524876))
```

**(reverse-geocode _loc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(reverse-geocode _loc locale_)**  
**(reverse-geocode _lat long_)**  
**(reverse-geocode _lat long locale_)**  

Returns a future for a list of places at the given location. _loc_ is a valid location. _lat_ and _long_ describe latitude and longitude as flonums directly. _locale_ is a symbol representing a locale. It is used for the place representations returned by `reverse-geocode`.

```scheme
(future-get (reverse-geocode (location 47.36541 8.5247) 'en_US))
⇒ (("CH" "Switzerland" "ZH" "Zürich"
    "8002" "Zürich" "Enge"
    "Brandschenkestrasse" "110" "Europe/Zurich"))
```

**(place-\>address _pl_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Formats a place as an address. For this operation to succeed, it is important that the country code of the place _pl_ is set as it is used to determine the address format.

```scheme
(define pl (car (future-get (reverse-geocode (location 47.36541 8.5247) 'de_CH))))
pl ⇒ ("CH" "Schweiz" "ZH" "Zürich"
       "8002" "Zürich" "Enge"
       "Brandschenkestrasse" "110" "Europe/Zurich")
(display (place->address pl))
⇒
Brandschenkestrasse 110
8002 Zürich
Schweiz
```

**(address-\>place _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(address-\>place _str locale_)**  

Parses the given address string _str_ into a place (or potentially multiple possible places) and returns a future referring to this as a list of places. _locale_ is a symbol representing a locale. It is used for the place representations returned by `address->place`.

```scheme
(future-get (address->place "Brandschenkestrasse 110, Zürich"))
⇒ (("CH" "Switzerland" "Zürich" "Zürich"
    "8002" "Zürich" "Enge"
    "Brandschenkestrasse" "110" "Europe/Zurich"))
```
