eucl_dist::[Float] -> [Float] -> Float
eucl_dist xVector yVector = sqrt(sum (zipWith (\x y -> (x-y) * (x-y)) xVector yVector))

point_grid:: Float -> [[Float]]
point_grid granularity = [[x,y]|x <- [0, (1/granularity)..1], y <- [0, (1/granularity)..1]]

area_of_circle_approx:: Float -> Float
area_of_circle_approx granularity = sum [1| point <- (point_grid granularity), (eucl_dist point [0,0]) < 1 ]