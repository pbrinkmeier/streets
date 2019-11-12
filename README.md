# streets

Download an extract at extract.bbbike.org.

```
$ rm -rf Mnesia*
$ erl
> streets:fill_db("osmdata/city.osm").
> % fill_db takes a while
> % selects nodes to put into SVG
> IsBuilding = fun(Tags) -> proplists:is_defined("building", Tags) end.
> % tags to SVG attributes
> FillBlack = fun(_Tags) -> [ {"fill", "black"} ] end.
> streets:ways_to_file("city.svg", 10000, IsBuilding, FillBlack).
> % exit
$ convert city.svg city.png
```

## Analyze a tag's values

```
> streets:analyze("building").  
[
{"yes",126332},                 
{"garage",807},                 
{"service",283},                
{"house",258},                  
{"garages",251},                
{"apartments",185},             
{"church",76},                  
{"school",70},                  
{"roof",54},                    
{"retail",52},                  
{"public",51},                  
{"residential",48},             
{"dormitory",41},               
{"detached",39},                
{"semidetached_house",36},      
{"chapel",32},                  
{"hospital",30},                
{"hangar",17},                  
...
]
```
