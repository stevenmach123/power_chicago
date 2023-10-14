left_join(values, by = "GEOID") 







mymap <- mapview(nc_pop, 
                 zcol = "estimate", 
                 homebutton = FALSE) + 
  mapview(starbucksNC, 
          zcol = "Name", 
          legend = FALSE, 
          alpha = 0.5, cex = 3, 
          col.regions = "orange",
          homebutton = FALSE) 
