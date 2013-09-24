--[[
    Ported 17th September 2013 by Simon Brooke

    Original code: https://github.com/Yonaba/Jumper

    *** Copyright notice below must not be removed ***

    Porting notes:
        - All modules have been scrunched into the one file under
          a Jumper namespace, see example at end
        - The assert module has been removed and most body comments 
          to keep file size reasonable
          
    Usage notes:
        local grid = Jumper.Grid(map, false)
        Setting the second arg to false computes the grid on the fly.
        This is use-case-based but worked faster for me at runtime...
        See original documentation for more information

    Many thanks to Roland Yonaba for an outstanding piece of coding and
    permission to publish his work in this form for Codea users

--]]

--[[
This work is under MIT-LICENSE
Copyright (c) 2012-2013 Roland Yonaba.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
--]]
Jumper = {}
function node()

    local assert = assert
    local Node = {}
    Node.__index = Node

    function Node:new(x, y)
        return setmetatable({ _x = x, _y = y, _clearance = {} }, Node)
    end

    function Node.__lt(A, B) return (A._f < B._f) end

    function Node:getX() return self._x end

    function Node:getY() return self._y end

    function Node:getPos() return self._x, self._y end

    function Node:getClearance(walkable)
        return self._clearance[walkable]
    end

    function Node:removeClearance(walkable)
        self._clearance[walkable] = nil
        return self
    end

    function Node:reset()
        self._g, self._h, self._f = nil, nil, nil
        self._opened, self._closed, self._parent = nil, nil, nil
        return self
    end

    return setmetatable(Node,
        {
            __call = function(self, ...)
                return Node:new(...)
            end
        })
end

local Node = node()

function path()

    local abs, max = math.abs, math.max
    local t_insert, t_remove = table.insert, table.remove

    local Path = {}
    Path.__index = Path

    function Path:new()
        return setmetatable({ _nodes = {} }, Path)
    end

    function Path:iter()
        local i, pathLen = 1, #self._nodes
        return function()
            if self._nodes[i] then
                i = i + 1
                return self._nodes[i - 1], i - 1
            end
        end
    end

    Path.nodes = Path.iter

    function Path:getLength()
        local len = 0
        for i = 2, #self._nodes do
            len = len + Heuristic.EUCLIDIAN(self._nodes[i], self._nodes[i - 1])
        end
        return len
    end

    function Path:addNode(node, index)
        index = index or #self._nodes + 1
        t_insert(self._nodes, index, node)
        return self
    end

    function Path:fill()
        local i = 2
        local xi, yi, dx, dy
        local N = #self._nodes
        local incrX, incrY
        while true do
            xi, yi = self._nodes[i]._x, self._nodes[i]._y
            dx, dy = xi - self._nodes[i - 1]._x, yi - self._nodes[i - 1]._y
            if (abs(dx) > 1 or abs(dy) > 1) then
                incrX = dx / max(abs(dx), 1)
                incrY = dy / max(abs(dy), 1)
                t_insert(self._nodes, i, self._grid:getNodeAt(self._nodes[i - 1]._x + incrX, self._nodes[i - 1]._y + incrY))
                N = N + 1
            else i = i + 1
            end
            if i > N then break end
        end
        return self
    end

    function Path:filter()
        local i = 2
        local xi, yi, dx, dy, olddx, olddy
        xi, yi = self._nodes[i]._x, self._nodes[i]._y
        dx, dy = xi - self._nodes[i - 1]._x, yi - self._nodes[i - 1]._y
        while true do
            olddx, olddy = dx, dy
            if self._nodes[i + 1] then
                i = i + 1
                xi, yi = self._nodes[i]._x, self._nodes[i]._y
                dx, dy = xi - self._nodes[i - 1]._x, yi - self._nodes[i - 1]._y
                if olddx == dx and olddy == dy then
                    t_remove(self._nodes, i - 1)
                    i = i - 1
                end
            else break
            end
        end
        return self
    end

    function Path:clone()
        local p = Path:new()
        for node in self:nodes() do p:addNode(node) end
        return p
    end

    function Path:isEqualTo(p2)
        local p1 = self:clone():filter()
        local p2 = p2:clone():filter()
        for node, count in p1:nodes() do
            if not p2._nodes[count] then return false end
            local n = p2._nodes[count]
            if n._x ~= node._x or n._y ~= node._y then return false end
        end
        return true
    end

    function Path:reverse()
        local _nodes = {}
        for i = #self._nodes, 1, -1 do
            _nodes[#_nodes + 1] = self._nodes[i]
        end
        self._nodes = _nodes
        return self
    end

    function Path:append(p)
        for node in p:nodes() do self:addNode(node) end
        return self
    end

    return setmetatable(Path,
        {
            __call = function(self, ...)
                return Path:new(...)
            end
        })
end

local Path = path()

function utils()

    local pairs = pairs
    local type = type
    local t_insert = table.insert
    local assert = assert
    local coroutine = coroutine

    local function arraySize(t)
        local count = 0
        for k, v in pairs(t) do
            count = count + 1
        end
        return count
    end

    local function stringMapToArray(str)
        local map = {}
        local w, h
        for line in str:gmatch('[^\n\r]+') do
            if line then
                w = not w and #line or w

                h = (h or 0) + 1
                map[h] = {}
                for char in line:gmatch('.') do
                    map[h][#map[h] + 1] = char
                end
            end
        end
        return map
    end

    local function getKeys(t)
        local keys = {}
        for k, v in pairs(t) do keys[#keys + 1] = k end
        return keys
    end

    local function getArrayBounds(map)
        local min_x, max_x
        local min_y, max_y
        for y in pairs(map) do
            min_y = not min_y and y or (y < min_y and y or min_y)
            max_y = not max_y and y or (y > max_y and y or max_y)
            for x in pairs(map[y]) do
                min_x = not min_x and x or (x < min_x and x or min_x)
                max_x = not max_x and x or (x > max_x and x or max_x)
            end
        end
        return min_x, max_x, min_y, max_y
    end

    local function arrayToNodes(map)
        local min_x, max_x
        local min_y, max_y
        local nodes = {}
        for y in pairs(map) do
            min_y = not min_y and y or (y < min_y and y or min_y)
            max_y = not max_y and y or (y > max_y and y or max_y)
            nodes[y] = {}
            for x in pairs(map[y]) do
                min_x = not min_x and x or (x < min_x and x or min_x)
                max_x = not max_x and x or (x > max_x and x or max_x)
                nodes[y][x] = Node:new(x, y)
            end
        end
        return nodes,
        (min_x or 0), (max_x or 0),
        (min_y or 0), (max_y or 0)
    end

    local function around()
        local iterf = function(x0, y0, s)
            local x, y = x0 - s, y0 - s
            coroutine.yield(x, y)
            repeat
                x = x + 1
                coroutine.yield(x, y)
                until x == x0 + s
            repeat
                y = y + 1
                coroutine.yield(x, y)
                until y == y0 + s
            repeat
                x = x - 1
                coroutine.yield(x, y)
                until x == x0 - s
            repeat
                y = y - 1
                coroutine.yield(x, y)
                until y == y0 - s + 1
        end
        return coroutine.create(iterf)
    end

    local function traceBackPath(finder, node, startNode)
        local path = Path:new()
        path._grid = finder._grid
        while true do
            if node._parent then
                t_insert(path._nodes, 1, node)
                node = node._parent
            else
                t_insert(path._nodes, 1, startNode)
                return path
            end
        end
    end

    local indexOf = function(t, v)
        for i = 1, #t do
            if t[i] == v then return i end
        end
        return nil
    end

    local function outOfRange(i, low, up)
        return (i < low or i > up)
    end

    return {
        arraySize = arraySize,
        getKeys = getKeys,
        indexOf = indexOf,
        outOfRange = outOfRange,
        getArrayBounds = getArrayBounds,
        arrayToNodes = arrayToNodes,
        strToMap = stringMapToArray,
        around = around,
        drAround = drAround,
        traceBackPath = traceBackPath
    }
end

local Utils = utils()

function grid()

    local pairs = pairs
    local assert = assert
    local next = next
    local setmetatable = setmetatable
    local floor = math.floor
    local coroutine = coroutine

    local straightOffsets = {
      {x = 1, y = 0} --[[W]], {x = -1, y =  0}, --[[E]]
      {x = 0, y = 1} --[[S]], {x =  0, y = -1}, --[[N]]
    }

    local diagonalOffsets = {
      {x = -1, y = -1} --[[NW]], {x = 1, y = -1}, --[[NE]]
      {x = -1, y =  1} --[[SW]], {x = 1, y =  1}, --[[SE]]
    }

    local Grid = {}
    Grid.__index = Grid

    local PreProcessGrid = setmetatable({}, Grid)
    local PostProcessGrid = setmetatable({}, Grid)
    PreProcessGrid.__index = PreProcessGrid
    PostProcessGrid.__index = PostProcessGrid
    PreProcessGrid.__call = function(self, x, y)
        return self:getNodeAt(x, y)
    end
    PostProcessGrid.__call = function(self, x, y, create)
        if create then return self:getNodeAt(x, y) end
        return self._nodes[y] and self._nodes[y][x]
    end

    function Grid:new(map, cacheNodeAtRuntime)
        if type(map) == 'string' then

            map = Utils.strToMap(map)
        end

        if cacheNodeAtRuntime then
            return PostProcessGrid:new(map, walkable)
        end
        return PreProcessGrid:new(map, walkable)
    end

    function Grid:isWalkableAt(x, y, walkable, clearance)
        local nodeValue = self._map[y] and self._map[y][x]
        if nodeValue then
            if not walkable then return true end
        else
            return false
        end
        local hasEnoughClearance = not clearance and true or false
        if not hasEnoughClearance then
            if not self._isAnnotated[walkable] then return false end
            local node = self:getNodeAt(x, y)
            local nodeClearance = node:getClearance(walkable)
            hasEnoughClearance = (nodeClearance >= clearance)
        end
        if self._eval then
            return walkable(nodeValue) and hasEnoughClearance
        end
        return ((nodeValue == walkable) and hasEnoughClearance)
    end

    function Grid:getWidth()
        return self._width
    end

    function Grid:getHeight()
        return self._height
    end

    function Grid:getMap()
        return self._map
    end

    function Grid:getNodes()
        return self._nodes
    end

    function Grid:getBounds()
        return self._min_x, self._min_y, self._max_x, self._max_y
    end

    function Grid:getNeighbours(node, walkable, allowDiagonal, tunnel, clearance)
        local neighbours = {}
        for i = 1, #straightOffsets do
            local n = self:getNodeAt(node._x + straightOffsets[i].x,
                node._y + straightOffsets[i].y)
            if n and self:isWalkableAt(n._x, n._y, walkable, clearance) then
                neighbours[#neighbours + 1] = n
            end
        end

        if not allowDiagonal then return neighbours end

        tunnel = not not tunnel
        for i = 1, #diagonalOffsets do
            local n = self:getNodeAt(node._x + diagonalOffsets[i].x,
                node._y + diagonalOffsets[i].y)
            if n and self:isWalkableAt(n._x, n._y, walkable, clearance) then
                if tunnel then
                    neighbours[#neighbours + 1] = n
                else
                    local skipThisNode = false
                    local n1 = self:getNodeAt(node._x + diagonalOffsets[i].x, node._y)
                    local n2 = self:getNodeAt(node._x, node._y + diagonalOffsets[i].y)
                    if ((n1 and n2) and not self:isWalkableAt(n1._x, n1._y, walkable, clearance) and not self:isWalkableAt(n2._x, n2._y, walkable, clearance)) then
                        skipThisNode = true
                    end
                    if not skipThisNode then neighbours[#neighbours + 1] = n end
                end
            end
        end

        return neighbours
    end

    function Grid:iter(lx, ly, ex, ey)
        local min_x = lx or self._min_x
        local min_y = ly or self._min_y
        local max_x = ex or self._max_x
        local max_y = ey or self._max_y

        local x, y
        y = min_y
        return function()
            x = not x and min_x or x + 1
            if x > max_x then
                x = min_x
                y = y + 1
            end
            if y > max_y then
                y = nil
            end
            return self._nodes[y] and self._nodes[y][x] or self:getNodeAt(x, y)
        end
    end

    function Grid:around(node, radius)
        local x, y = node._x, node._y
        radius = radius or 1
        local _around = Utils.around()
        local _nodes = {}
        repeat
            local state, x, y = coroutine.resume(_around, x, y, radius)
            local nodeAt = state and self:getNodeAt(x, y)
            if nodeAt then _nodes[#_nodes + 1] = nodeAt end
            until (not state)
        local _i = 0
        return function()
            _i = _i + 1
            return _nodes[_i]
        end
    end

    function Grid:each(f, ...)
        for node in self:iter() do f(node, ...) end
        return self
    end

    function Grid:eachRange(lx, ly, ex, ey, f, ...)
        for node in self:iter(lx, ly, ex, ey) do f(node, ...) end
        return self
    end

    function Grid:imap(f, ...)
        for node in self:iter() do
            node = f(node, ...)
        end
        return self
    end

    function Grid:imapRange(lx, ly, ex, ey, f, ...)
        for node in self:iter(lx, ly, ex, ey) do
            node = f(node, ...)
        end
        return self
    end

    function PreProcessGrid:new(map)
        local newGrid = {}
        newGrid._map = map
        newGrid._nodes, newGrid._min_x, newGrid._max_x, newGrid._min_y, newGrid._max_y = Utils.arrayToNodes(newGrid._map)
        newGrid._width = (newGrid._max_x - newGrid._min_x) + 1
        newGrid._height = (newGrid._max_y - newGrid._min_y) + 1
        newGrid._isAnnotated = {}
        return setmetatable(newGrid, PreProcessGrid)
    end

    function PostProcessGrid:new(map)
        local newGrid = {}
        newGrid._map = map
        newGrid._nodes = {}
        newGrid._min_x, newGrid._max_x, newGrid._min_y, newGrid._max_y = Utils.getArrayBounds(newGrid._map)
        newGrid._width = (newGrid._max_x - newGrid._min_x) + 1
        newGrid._height = (newGrid._max_y - newGrid._min_y) + 1
        newGrid._isAnnotated = {}
        return setmetatable(newGrid, PostProcessGrid)
    end

    function PreProcessGrid:getNodeAt(x, y)
        return self._nodes[y] and self._nodes[y][x] or nil
    end

    function PostProcessGrid:getNodeAt(x, y)
        if not x or not y then return end
        if Utils.outOfRange(x, self._min_x, self._max_x) then return end
        if Utils.outOfRange(y, self._min_y, self._max_y) then return end
        if not self._nodes[y] then self._nodes[y] = {} end
        if not self._nodes[y][x] then self._nodes[y][x] = Node:new(x, y) end
        return self._nodes[y][x]
    end

    return setmetatable(Grid, {
        __call = function(self, ...)
            return self:new(...)
        end
    })
end

Jumper.Grid = grid()

local abs = math.abs
local sqrt = math.sqrt
local sqrt2 = sqrt(2)
local max, min = math.max, math.min

function heuristics()
    local Heuristics = {}

    function Heuristics.MANHATTAN(nodeA, nodeB)
        local dx = abs(nodeA._x - nodeB._x)
        local dy = abs(nodeA._y - nodeB._y)
        return (dx + dy)
    end

    function Heuristics.EUCLIDIAN(nodeA, nodeB)
        local dx = nodeA._x - nodeB._x
        local dy = nodeA._y - nodeB._y
        return sqrt(dx * dx + dy * dy)
    end

    function Heuristics.DIAGONAL(nodeA, nodeB)
        local dx = abs(nodeA._x - nodeB._x)
        local dy = abs(nodeA._y - nodeB._y)
        return max(dx, dy)
    end

    function Heuristics.CARDINTCARD(nodeA, nodeB)
        local dx = abs(nodeA._x - nodeB._x)
        local dy = abs(nodeA._y - nodeB._y)
        return min(dx, dy) * sqrt2 + max(dx, dy) - min(dx, dy)
    end

    return Heuristics
end

Heuristic = heuristics()

function heap()

    local floor = math.floor

    local function f_min(a, b) return a < b end

    local function percolate_up(heap, index)
        if index == 1 then return end
        local pIndex
        if index <= 1 then return end
        if index % 2 == 0 then
            pIndex = index / 2
        else pIndex = (index - 1) / 2
        end
        if not heap._sort(heap._heap[pIndex], heap._heap[index]) then
            heap._heap[pIndex], heap._heap[index] =
            heap._heap[index], heap._heap[pIndex]
            percolate_up(heap, pIndex)
        end
    end

    local function percolate_down(heap, index)
        local lfIndex, rtIndex, minIndex
        lfIndex = 2 * index
        rtIndex = lfIndex + 1
        if rtIndex > heap._size then
            if lfIndex > heap._size then return
            else minIndex = lfIndex
            end
        else
            if heap._sort(heap._heap[lfIndex], heap._heap[rtIndex]) then
                minIndex = lfIndex
            else
                minIndex = rtIndex
            end
        end
        if not heap._sort(heap._heap[index], heap._heap[minIndex]) then
            heap._heap[index], heap._heap[minIndex] = heap._heap[minIndex], heap._heap[index]
            percolate_down(heap, minIndex)
        end
    end

    local function newHeap(template, comp)
        return setmetatable({
            _heap = {},
            _sort = comp or f_min,
            _size = 0
        },
            template)
    end

    local heap = setmetatable({},
        {
            __call = function(self, ...)
                return newHeap(self, ...)
            end
        })
    heap.__index = heap

    function heap:empty()
        return (self._size == 0)
    end

    function heap:clear()
        self._heap = {}
        self._size = 0
        self._sort = self._sort or f_min
        return self
    end

    function heap:push(item)
        if item then
            self._size = self._size + 1
            self._heap[self._size] = item
            percolate_up(self, self._size)
        end
        return self
    end

    function heap:pop()
        local root
        if self._size > 0 then
            root = self._heap[1]
            self._heap[1] = self._heap[self._size]
            self._heap[self._size] = nil
            self._size = self._size - 1
            if self._size > 1 then
                percolate_down(self, 1)
            end
        end
        return root
    end

    function heap:heapify(item)
        if self._size == 0 then return end
        if item then
            local i = Utils.indexOf(self._heap, item)
            if i then
                percolate_down(self, i)
                percolate_up(self, i)
            end
            return
        end
        for i = floor(self._size / 2), 1, -1 do
            percolate_down(self, i)
        end
        return self
    end

    return heap
end

local Heap = heap()

function search_astar()

    local ipairs = ipairs
    local huge = math.huge

    local function computeCost(node, neighbour, finder, clearance)
        local mCost = Heuristic.EUCLIDIAN(neighbour, node)
        if node._g + mCost < neighbour._g then
            neighbour._parent = node
            neighbour._g = node._g + mCost
        end
    end

    local function updateVertex(finder, openList, node, neighbour, endNode, clearance, heuristic, overrideCostEval)
        local oldG = neighbour._g
        local cmpCost = overrideCostEval or computeCost
        cmpCost(node, neighbour, finder, clearance)
        if neighbour._g < oldG then
            local nClearance = neighbour._clearance[finder._walkable]
            local pushThisNode = clearance and nClearance and (nClearance >= clearance)
            if (clearance and pushThisNode) or (not clearance) then
                if neighbour._opened then neighbour._opened = false end
                neighbour._h = heuristic(endNode, neighbour)
                neighbour._f = neighbour._g + neighbour._h
                openList:push(neighbour)
                neighbour._opened = true
            end
        end
    end

    return function(finder, startNode, endNode, clearance, toClear, overrideHeuristic, overrideCostEval)

        local heuristic = overrideHeuristic or finder._heuristic
        local openList = Heap()
        startNode._g = 0
        startNode._h = heuristic(endNode, startNode)
        startNode._f = startNode._g + startNode._h
        openList:push(startNode)
        toClear[startNode] = true
        startNode._opened = true

        while not openList:empty() do
            local node = openList:pop()
            node._closed = true
            if node == endNode then return node end
            local neighbours = finder._grid:getNeighbours(node, finder._walkable, finder._allowDiagonal, finder._tunnel)
            for i = 1, #neighbours do
                local neighbour = neighbours[i]
                if not neighbour._closed then
                    toClear[neighbour] = true
                    if not neighbour._opened then
                        neighbour._g = huge
                        neighbour._parent = nil
                    end
                    updateVertex(finder, openList, node, neighbour, endNode, clearance, heuristic, overrideCostEval)
                end
            end
        end

        return nil
    end
end

function search_dijkstra()
    local astar_search = search_astar()
    -- Dijkstra is similar to aStar, with no heuristic
    local dijkstraHeuristic = function() return 0 end

    -- Calculates a path.
    -- Returns the path from location `<startX, startY>` to location `<endX, endY>`.
    return function(finder, startNode, endNode, clearance, toClear)
        return astar_search(finder, startNode, endNode, clearance, toClear, dijkstraHeuristic)
    end
end

function search_bfs()
    -- Internalization
    local t_remove = table.remove

    local function breadth_first_search(finder, openList, node, endNode, clearance, toClear)
        local neighbours = finder._grid:getNeighbours(node, finder._walkable, finder._allowDiagonal, finder._tunnel)
        for i = 1, #neighbours do
            local neighbour = neighbours[i]
            if not neighbour._closed and not neighbour._opened then
                local nClearance = neighbour._clearance[finder._walkable]
                local pushThisNode = clearance and nClearance and (nClearance >= clearance)
                if (clearance and pushThisNode) or (not clearance) then
                    openList[#openList + 1] = neighbour
                    neighbour._opened = true
                    neighbour._parent = node
                    toClear[neighbour] = true
                end
            end
        end
    end

    -- Calculates a path.
    -- Returns the path from location `<startX, startY>` to location `<endX, endY>`.
    return function(finder, startNode, endNode, clearance, toClear)

        local openList = {} -- We'll use a FIFO queue (simple array)
        openList[1] = startNode
        startNode._opened = true
        toClear[startNode] = true

        local node
        while (#openList > 0) do
            node = openList[1]
            t_remove(openList, 1)
            node._closed = true
            if node == endNode then return node end
            breadth_first_search(finder, openList, node, endNode, clearance, toClear)
        end

        return nil
    end
end

function search_dfs()
    -- Internalization
    local t_remove = table.remove

    local function depth_first_search(finder, openList, node, endNode, clearance, toClear)
        local neighbours = finder._grid:getNeighbours(node, finder._walkable, finder._allowDiagonal, finder._tunnel)
        for i = 1, #neighbours do
            local neighbour = neighbours[i]
            if (not neighbour._closed and not neighbour._opened) then
                local nClearance = neighbour._clearance[finder._walkable]
                local pushThisNode = clearance and nClearance and (nClearance >= clearance)
                if (clearance and pushThisNode) or (not clearance) then
                    openList[#openList + 1] = neighbour
                    neighbour._opened = true
                    neighbour._parent = node
                    toClear[neighbour] = true
                end
            end
        end
    end

    -- Calculates a path.
    -- Returns the path from location `<startX, startY>` to location `<endX, endY>`.
    return function(finder, startNode, endNode, clearance, toClear)

        local openList = {} -- We'll use a LIFO queue (simple array)
        openList[1] = startNode
        startNode._opened = true
        toClear[startNode] = true

        local node
        while (#openList > 0) do
            node = openList[#openList]
            t_remove(openList)
            node._closed = true
            if node == endNode then return node end
            depth_first_search(finder, openList, node, endNode, clearance, toClear)
        end

        return nil
    end
end

function search_jps()

    -- Internalization
    local max, abs = math.max, math.abs

    -- Local helpers, these routines will stay private
    -- As they are internally used by the public interface

    -- Resets properties of nodes expanded during a search
    -- This is a lot faster than resetting all nodes
    -- between consecutive pathfinding requests

    --[[
      Looks for the neighbours of a given node.
      Returns its natural neighbours plus forced neighbours when the given
      node has no parent (generally occurs with the starting node).
      Otherwise, based on the direction of move from the parent, returns
      neighbours while pruning directions which will lead to symmetric paths.

      In case diagonal moves are forbidden, when the given node has no
      parent, we return straight neighbours (up, down, left and right).
      Otherwise, we add left and right node (perpendicular to the direction
      of move) in the neighbours list.
    --]]
    local function findNeighbours(finder, node, clearance)

        if node._parent then
            local neighbours = {}
            local x, y = node._x, node._y
            -- Node have a parent, we will prune some neighbours
            -- Gets the direction of move
            local dx = (x - node._parent._x) / max(abs(x - node._parent._x), 1)
            local dy = (y - node._parent._y) / max(abs(y - node._parent._y), 1)

            -- Diagonal move case
            if dx ~= 0 and dy ~= 0 then
                local walkY, walkX

                -- Natural neighbours
                if finder._grid:isWalkableAt(x, y + dy, finder._walkable, clearance) then
                    neighbours[#neighbours + 1] = finder._grid:getNodeAt(x, y + dy)
                    walkY = true
                end
                if finder._grid:isWalkableAt(x + dx, y, finder._walkable, clearance) then
                    neighbours[#neighbours + 1] = finder._grid:getNodeAt(x + dx, y)
                    walkX = true
                end
                if walkX or walkY then
                    neighbours[#neighbours + 1] = finder._grid:getNodeAt(x + dx, y + dy)
                end

                -- Forced neighbours
                if (not finder._grid:isWalkableAt(x - dx, y, finder._walkable, clearance)) and walkY then
                    neighbours[#neighbours + 1] = finder._grid:getNodeAt(x - dx, y + dy)
                end
                if (not finder._grid:isWalkableAt(x, y - dy, finder._walkable, clearance)) and walkX then
                    neighbours[#neighbours + 1] = finder._grid:getNodeAt(x + dx, y - dy)
                end

            else
                -- Move along Y-axis case
                if dx == 0 then
                    local walkY
                    if finder._grid:isWalkableAt(x, y + dy, finder._walkable, clearance) then
                        neighbours[#neighbours + 1] = finder._grid:getNodeAt(x, y + dy)

                        -- Forced neighbours are left and right ahead along Y
                        if (not finder._grid:isWalkableAt(x + 1, y, finder._walkable, clearance)) then
                            neighbours[#neighbours + 1] = finder._grid:getNodeAt(x + 1, y + dy)
                        end
                        if (not finder._grid:isWalkableAt(x - 1, y, finder._walkable, clearance)) then
                            neighbours[#neighbours + 1] = finder._grid:getNodeAt(x - 1, y + dy)
                        end
                    end
                    -- In case diagonal moves are forbidden : Needs to be optimized
                    if not finder._allowDiagonal then
                        if finder._grid:isWalkableAt(x + 1, y, finder._walkable, clearance) then
                            neighbours[#neighbours + 1] = finder._grid:getNodeAt(x + 1, y)
                        end
                        if finder._grid:isWalkableAt(x - 1, y, finder._walkable, clearance)
                        then neighbours[#neighbours + 1] = finder._grid:getNodeAt(x - 1, y)
                        end
                    end
                else
                    -- Move along X-axis case
                    if finder._grid:isWalkableAt(x + dx, y, finder._walkable, clearance) then
                        neighbours[#neighbours + 1] = finder._grid:getNodeAt(x + dx, y)

                        -- Forced neighbours are up and down ahead along X
                        if (not finder._grid:isWalkableAt(x, y + 1, finder._walkable, clearance)) then
                            neighbours[#neighbours + 1] = finder._grid:getNodeAt(x + dx, y + 1)
                        end
                        if (not finder._grid:isWalkableAt(x, y - 1, finder._walkable, clearance)) then
                            neighbours[#neighbours + 1] = finder._grid:getNodeAt(x + dx, y - 1)
                        end
                    end
                    -- : In case diagonal moves are forbidden
                    if not finder._allowDiagonal then
                        if finder._grid:isWalkableAt(x, y + 1, finder._walkable, clearance) then
                            neighbours[#neighbours + 1] = finder._grid:getNodeAt(x, y + 1)
                        end
                        if finder._grid:isWalkableAt(x, y - 1, finder._walkable, clearance) then
                            neighbours[#neighbours + 1] = finder._grid:getNodeAt(x, y - 1)
                        end
                    end
                end
            end
            return neighbours
        end

        -- Node do not have parent, we return all neighbouring nodes
        return finder._grid:getNeighbours(node, finder._walkable, finder._allowDiagonal, finder._tunnel, clearance)
    end

    --[[
      Searches for a jump point (or a turning point) in a specific direction.
      This is a generic translation of the algorithm 2 in the paper:
        http://users.cecs.anu.edu.au/~dharabor/data/papers/harabor-grastien-aaai11.pdf
      The current expanded node is a jump point if near a forced node

      In case diagonal moves are forbidden, when lateral nodes (perpendicular to
      the direction of moves are walkable, we force them to be turning points in other
      to perform a straight move.
    --]]
    local function jump(finder, node, parent, endNode, clearance)
        if not node then return end

        local x, y = node._x, node._y
        local dx, dy = x - parent._x, y - parent._y

        -- If the node to be examined is unwalkable, return nil
        if not finder._grid:isWalkableAt(x, y, finder._walkable, clearance) then return end

        -- If the node to be examined is the endNode, return this node
        if node == endNode then return node end
        -- Diagonal search case
        if dx ~= 0 and dy ~= 0 then
            -- Current node is a jump point if one of his leftside/rightside neighbours ahead is forced
            if (finder._grid:isWalkableAt(x - dx, y + dy, finder._walkable, clearance) and (not finder._grid:isWalkableAt(x - dx, y, finder._walkable, clearance))) or
                    (finder._grid:isWalkableAt(x + dx, y - dy, finder._walkable, clearance) and (not finder._grid:isWalkableAt(x, y - dy, finder._walkable, clearance))) then
                return node
            end
        else
            -- Search along X-axis case
            if dx ~= 0 then
                if finder._allowDiagonal then
                    -- Current node is a jump point if one of his upside/downside neighbours is forced
                    if (finder._grid:isWalkableAt(x + dx, y + 1, finder._walkable, clearance) and (not finder._grid:isWalkableAt(x, y + 1, finder._walkable, clearance))) or
                            (finder._grid:isWalkableAt(x + dx, y - 1, finder._walkable, clearance) and (not finder._grid:isWalkableAt(x, y - 1, finder._walkable, clearance))) then
                        return node
                    end
                else
                    -- : in case diagonal moves are forbidden
                    if finder._grid:isWalkableAt(x + 1, y, finder._walkable, clearance) or finder._grid:isWalkableAt(x - 1, y, finder._walkable, clearance) then return node end
                end
            else
                -- Search along Y-axis case
                -- Current node is a jump point if one of his leftside/rightside neighbours is forced
                if finder._allowDiagonal then
                    if (finder._grid:isWalkableAt(x + 1, y + dy, finder._walkable, clearance) and (not finder._grid:isWalkableAt(x + 1, y, finder._walkable, clearance))) or
                            (finder._grid:isWalkableAt(x - 1, y + dy, finder._walkable, clearance) and (not finder._grid:isWalkableAt(x - 1, y, finder._walkable, clearance))) then
                        return node
                    end
                else
                    -- : in case diagonal moves are forbidden
                    if finder._grid:isWalkableAt(x, y + 1, finder._walkable, clearance) or finder._grid:isWalkableAt(x, y - 1, finder._walkable, clearance) then return node end
                end
            end
        end

        -- Recursive horizontal/vertical search
        if dx ~= 0 and dy ~= 0 then
            if jump(finder, finder._grid:getNodeAt(x + dx, y), node, endNode, clearance) then return node end
            if jump(finder, finder._grid:getNodeAt(x, y + dy), node, endNode, clearance) then return node end
        end

        -- Recursive diagonal search
        if finder._allowDiagonal then
            if finder._grid:isWalkableAt(x + dx, y, finder._walkable, clearance) or finder._grid:isWalkableAt(x, y + dy, finder._walkable, clearance) then
                return jump(finder, finder._grid:getNodeAt(x + dx, y + dy), node, endNode, clearance)
            end
        end
    end

    --[[
      Searches for successors of a given node in the direction of each of its neighbours.
      This is a generic translation of the algorithm 1 in the paper:
        http://users.cecs.anu.edu.au/~dharabor/data/papers/harabor-grastien-aaai11.pdf

      Also, we notice that processing neighbours in a reverse order producing a natural
      looking path, as the pathfinder tends to keep heading in the same direction.
      In case a jump point was found, and this node happened to be diagonal to the
      node currently expanded in a straight mode search, we skip this jump point.
    --]]
    local function identifySuccessors(finder, openList, node, endNode, clearance, toClear)

        -- Gets the valid neighbours of the given node
        -- Looks for a jump point in the direction of each neighbour
        local neighbours = findNeighbours(finder, node, clearance)
        for i = #neighbours, 1, -1 do

            local skip = false
            local neighbour = neighbours[i]
            local jumpNode = jump(finder, neighbour, node, endNode, clearance)

            -- : in case a diagonal jump point was found in straight mode, skip it.
            if jumpNode and not finder._allowDiagonal then
                if ((jumpNode._x ~= node._x) and (jumpNode._y ~= node._y)) then skip = true end
            end

            -- Performs regular A-star on a set of jump points
            if jumpNode and not skip then
                -- Update the jump node and move it in the closed list if it wasn't there
                if not jumpNode._closed then
                    local extraG = Heuristic.EUCLIDIAN(jumpNode, node)
                    local newG = node._g + extraG
                    if not jumpNode._opened or newG < jumpNode._g then
                        toClear[jumpNode] = true -- Records this node to reset its properties later.
                        jumpNode._g = newG
                        jumpNode._h = jumpNode._h or
                                (finder._heuristic(jumpNode, endNode))
                        jumpNode._f = jumpNode._g + jumpNode._h
                        jumpNode._parent = node
                        if not jumpNode._opened then
                            openList:push(jumpNode)
                            jumpNode._opened = true
                        else
                            openList:heapify(jumpNode)
                        end
                    end
                end
            end
        end
    end

    -- Calculates a path.
    -- Returns the path from location `<startX, startY>` to location `<endX, endY>`.
    return function(finder, startNode, endNode, clearance, toClear)

        startNode._g, startNode._f, startNode._h = 0, 0, 0
        local openList = Heap()
        openList:push(startNode)
        startNode._opened = true
        toClear[startNode] = true

        local node
        while not openList:empty() do
            -- Pops the lowest F-cost node, moves it in the closed list
            node = openList:pop()
            node._closed = true
            -- If the popped node is the endNode, return it
            if node == endNode then
                return node
            end
            -- otherwise, identify successors of the popped node
            identifySuccessors(finder, openList, node, endNode, clearance, toClear)
        end

        -- No path found, return nil
        return nil
    end
end

function search_thetastar()
    -- Internalization
    local ipairs = ipairs
    local huge, abs = math._huge, math.abs

    -- Line Of Sight (Bresenham's line marching algorithm)
    -- http://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm
    local lineOfSight = function(node, neighbour, finder, clearance)
        local x0, y0 = node._x, node._y
        local x1, y1 = neighbour._x, neighbour._y
        local dx = abs(x1 - x0)
        local dy = abs(y1 - y0)
        local err = dx - dy
        local sx = (x0 < x1) and 1 or -1
        local sy = (y0 < y1) and 1 or -1

        while true do
            if not finder._grid:isWalkableAt(x0, y0, finder._walkable, finder._tunnel, clearance) then
                return false
            end
            if x0 == x1 and y0 == y1 then
                break
            end
            local e2 = 2 * err
            if e2 > -dy then
                err = err - dy
                x0 = x0 + sx
            end
            if e2 < dx then
                err = err + dx
                y0 = y0 + sy
            end
        end
        return true
    end

    -- Theta star cost evaluation
    local function computeCost(node, neighbour, finder, clearance)
        local parent = node._parent or node
        local mpCost = Heuristic.EUCLIDIAN(neighbour, parent)
        if lineOfSight(parent, neighbour, finder, clearance) then
            if parent._g + mpCost < neighbour._g then
                neighbour._parent = parent
                neighbour._g = parent._g + mpCost
            end
        else
            local mCost = Heuristic.EUCLIDIAN(neighbour, node)
            if node._g + mCost < neighbour._g then
                neighbour._parent = node
                neighbour._g = node._g + mCost
            end
        end
    end

    -- Calculates a path.
    -- Returns the path from location `<startX, startY>` to location `<endX, endY>`.
    return function(finder, startNode, endNode, clearance, toClear, overrideHeuristic)
        local astar_search = search_astar()
        return astar_search(finder, startNode, endNode, clearance, toClear, overrideHeuristic, computeCost)
    end
end

function pathfinder()

    local t_insert, t_remove = table.insert, table.remove
    local floor = math.floor
    local pairs = pairs
    local assert = assert
    local type = type
    local setmetatable, getmetatable = setmetatable, getmetatable

    local Finders = {
        ['ASTAR'] = search_astar(),
        ['DIJKSTRA'] = search_dijkstra(),
        ['THETASTAR'] = search_thetastar(),
        ['BFS'] = search_bfs(),
        ['DFS'] = search_dfs(),
        ['JPS'] = search_jps(),
    }

    local toClear = {}

    local searchModes = { ['DIAGONAL'] = true, ['ORTHOGONAL'] = true }

    local Pathfinder = {}
    Pathfinder.__index = Pathfinder

    function Pathfinder:new(grid, finderName, walkable)
        local newPathfinder = {}
        setmetatable(newPathfinder, Pathfinder)
        newPathfinder:setGrid(grid)
        newPathfinder:setFinder(finderName)
        newPathfinder:setWalkable(walkable)
        newPathfinder:setMode('DIAGONAL')
        newPathfinder:setHeuristic('MANHATTAN')
        newPathfinder:setTunnelling(false)
        return newPathfinder
    end

    function Pathfinder:annotateGrid()

        for x = self._grid._max_x, self._grid._min_x, -1 do
            for y = self._grid._max_y, self._grid._min_y, -1 do
                local node = self._grid:getNodeAt(x, y)
                if self._grid:isWalkableAt(x, y, self._walkable) then
                    local nr = self._grid:getNodeAt(node._x + 1, node._y)
                    local nrd = self._grid:getNodeAt(node._x + 1, node._y + 1)
                    local nd = self._grid:getNodeAt(node._x, node._y + 1)
                    if nr and nrd and nd then
                        local m = nrd._clearance[self._walkable] or 0
                        m = (nd._clearance[self._walkable] or 0) < m and (nd._clearance[self._walkable] or 0) or m
                        m = (nr._clearance[self._walkable] or 0) < m and (nr._clearance[self._walkable] or 0) or m
                        node._clearance[self._walkable] = m + 1
                    else
                        node._clearance[self._walkable] = 1
                    end
                else node._clearance[self._walkable] = 0
                end
            end
        end
        self._grid._isAnnotated[self._walkable] = true
        return self
    end

    function Pathfinder:clearAnnotations()

        for node in self._grid:iter() do
            node:removeClearance(self._walkable)
        end
        self._grid._isAnnotated[self._walkable] = false
        return self
    end

    function Pathfinder:setGrid(grid)

        self._grid = grid
        self._grid._eval = self._walkable and type(self._walkable) == 'function'
        return self
    end

    function Pathfinder:getGrid()
        return self._grid
    end

    function Pathfinder:setWalkable(walkable)

        self._walkable = walkable
        self._grid._eval = type(self._walkable) == 'function'
        return self
    end

    function Pathfinder:getWalkable()
        return self._walkable
    end

    function Pathfinder:setFinder(finderName)
        if not finderName then
            if not self._finder then
                finderName = 'ASTAR'
            else return
            end
        end

        self._finder = finderName
        return self
    end

    function Pathfinder:getFinder()
        return self._finder
    end

    function Pathfinder:getFinders()
        return Utils.getKeys(Finders)
    end

    function Pathfinder:setHeuristic(heuristic)

        self._heuristic = Heuristic[heuristic] or heuristic
        return self
    end

    function Pathfinder:getHeuristic()
        return self._heuristic
    end

    function Pathfinder:getHeuristics()
        return Utils.getKeys(Heuristic)
    end

    function Pathfinder:setMode(mode)

        self._allowDiagonal = (mode == 'DIAGONAL')
        return self
    end

    function Pathfinder:getMode()
        return (self._allowDiagonal and 'DIAGONAL' or 'ORTHOGONAL')
    end

    function Pathfinder:getModes()
        return Utils.getKeys(searchModes)
    end

    function Pathfinder:setTunnelling(bool)

        self._tunnel = bool
        return self
    end

    function Pathfinder:getTunnelling()
        return self._tunnel
    end

    function Pathfinder:getPath(startX, startY, endX, endY, clearance)
        self:reset()
        local startNode = self._grid:getNodeAt(startX, startY)
        local endNode = self._grid:getNodeAt(endX, endY)

        local _endNode = Finders[self._finder](self, startNode, endNode, clearance, toClear)
        if _endNode then
            return Utils.traceBackPath(self, _endNode, startNode)
        end
        return nil
    end

    function Pathfinder:reset()
        for node in pairs(toClear) do node:reset() end
        toClear = {}
        return self
    end

    return setmetatable(Pathfinder, {
        __call = function(self, ...)
            return self:new(...)
        end
    })
end


Jumper.Pathfinder = pathfinder()
--[[
local map = {
    { 0, 0, 0, 0, 8, 0, 0, 0, 0, 0 },
    { 0, 0, 0, 0, 7, 0, 0, 0, 0, 0 },
    { 0, 0, 0, 0, 8, 0, 0, 0, 0, 0 },
    { 0, 0, 0, 0, 8, 0, 0, 0, 0, 0 },
    { 0, 0, 0, 0, 8, 0, 0, 0, 0, 0 },
    { 0, 0, 0, 0, 8, 0, 0, 0, 0, 0 },
    { 0, 0, 0, 0, 5, 0, 0, 0, 0, 0 },
    { 0, 0, 0, 0, 4, 0, 0, 0, 0, 0 },
}

local walkable = function(value) return value < 9 end
local grid = Jumper.Grid(map)

local myFinder = Jumper.Pathfinder(grid, 'ASTAR', walkable)

myFinder:setHeuristic('EUCLIDIAN')

local h = function(nodeA, nodeB)

    local dx = math.abs(nodeA._x - nodeB._x)
    local dy = math.abs(nodeA._y - nodeB._y)
    local value = map[nodeB._y][nodeB._x]
    if (value == 0) then value = 1 end
    return (dx + dy) * value
end

myFinder:setHeuristic(h)

local p = myFinder:getPath(1, 5, 10, 8)
--p:fill()
for node, count in p:nodes() do
    print(('%d. Node(%d,%d)'):format(count, node:getX(), node:getY()))
end
print(('Path length: %.2f'):format(p:getLength()))
--]]
