-- JumperTest

local colourMap = readImage("Documents:Map1_Colour")
local tileMap = readImage("Documents:Map1_Elevation_Tile")

-- Use this function to perform your initial setup
function setup()
    
    local map = {}
    for y=tileMap.height, 1, -1 do
        map[y] = {}
        for x=1, tileMap.width do
            -- Tilemap is greyscale, so just take r value as elevation
            map[y][x] = tileMap:get(x,y)
        end
    end
    local walkable = function(value) return value < 255 end
    local grid = Jumper.Grid(map, false)

    local myFinder = Jumper.Pathfinder(grid, 'ASTAR', walkable)

    local h = function(nodeA, nodeB)
 
        local dx = math.abs(nodeA._x - nodeB._x)
        local dy = math.abs(nodeA._y - nodeB._y)
        local value = map[nodeB._y][nodeB._x]
        if (value == 0) then value = 1 end
            
        return (dx + dy) * (value/10)
    end

    myFinder:setHeuristic(h)
    
    local p = myFinder:getPath(5, 90, 70, 40)

    for node, count in p:nodes() do
        for x=1,4 do
            for y=1,4 do
                colourMap:set(((node:getX()-1)*8)+x, ((node:getY()-1)*8)+y, color(255,0,0))
            end
        end
    end
    print(('Path length: %.2f'):format(p:getLength()))
   
end

-- This function gets called once every frame
function draw()
    -- This sets a dark background color 
    background(40, 40, 50)

    -- This sets the line thickness
    strokeWidth(5)

    -- Do your drawing here
    spriteMode(CORNER)
    sprite(colourMap, 0, 0)     
end
