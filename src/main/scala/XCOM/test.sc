val vector = Vector(5,6,7,20,11)
var map = Map[Int, Boolean]()

for(e <- vector){
  map += (
    e -> false
    )

}

print(vector)


map += 5 -> true

print(map)

map -= 5

print(map)