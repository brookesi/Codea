-- -------------------------------------------------------------------------------------------------------
-- Continuous Gesture Recognizer Library
--
-- Original copyright notice - DO NOT MODIFY:
--
-- ** START **
-- If you use this code for your research then please remember to cite our paper:
--
-- Kristensson, P.O. and Denby, L.C. 2011. Continuous recognition and visualization
-- of pen strokes and touch-screen gestures. In Procceedings of the 8th Eurographics
-- Symposium on Sketch-Based Interfaces and Modeling (SBIM 2011). ACM Press: 95-102.
--
-- Copyright (C) 2011 by Per Ola Kristensson, University of St Andrews, UK.
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.
-- ** END **
-- ----------------------------------------------------------------------------

-- ----------------------------------------------------------------------------
-- Porting Notes:
--
-- @ Ported by Simon Brooke, August 2013
-- @ Pretty extensively tested on a variety of gestures
-- @ Actual probability values do vary from Java original due to Lua underlying
--   data type of float, but not by much. Attempts to do integer rounding
--   resulted in less accuracy
-- @ I have very little idea how this actually works its magic, refer to original
--   paper as detailed in copyright notice above for specifics
-- @ Moving from zero to one-based indexes in arrays etc. is the Seventh Circle
--   of Debugging Hell!
--
-- Usage Notes:
--
-- Gesture templates are defined as a series of lines in a virtual coordinate
-- space where values to the 'North' and 'West' decrease, e.g.:
--
--       |
--  x<0  |  x>0
--  y<0  |  y<0
--       |
-- -------------
--       |
--  x<0  |  x>0
--  y>0  |  y>0
--       |
--
-- It may be easier to think in terms of a square where the top left corner is
-- coordinate 0,0 with increasing x and y values as you go right and down...
--
-- I only mention the above because the templates packaged with the original
-- source code (which I have included) work on a coordinate system with (0,0)
-- in the *middle* of the square (as diagrammed above), see the 'North' and
-- 'West' templates below. The NorthWest template is therefore from origin
-- (0,0) to (-1,-1)
--
-- The easiest way of defining your gesture set is to use graph paper!!
--
-- Note that gestures are evaluated in the *order the points are defined*,
-- that is, if you define a 'clockwise' square, gesturing an anti-clockwise
-- square will *not* match with high probability, this is *gesture* not *shape*
-- recognition
--
-- Open or closed gestures are supported from simple swipes to e.g. figure-of-
-- eights (and beyond...)
--
-- Issues, Warnings and Caveats:
--
-- This algorithm works amazingly well, *but* the more gestures you add,
-- especially any similar ones, the more the probability results will fall...
-- E.g. with using just the eight 'compass point' templates, you will reliably
-- get 0.7+ on the probability (out of 1.0). The more similar gestures you add
-- though caused the probability to drop to e.g. 0.25. The algorithm *is* still
-- accurate, but at this point you may run into difficulties deciding whether
-- the user gesture is valid or not, because the CGR will *always* return
-- results for all gestures sorted by probability...
--
-- Personally I am considering using >0.7 validity and the first two results
-- being less than 90% similar (see CGR_cmdLineTest example below) to determine
-- a 'good' user gesture...
-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
-- Local fields
-- ----------------------------------------------------------------------------
-- Constants
local DEFAULT_E_SIGMA = 200.0
local DEFAULT_BETA = 400.0
local DEFAULT_LAMBDA = 0.4
local DEFAULT_KAPPA = 1.0

-- Original value: 1000
local MAX_RESAMPLING_PTS = 1000

-- This is the distance between sampling points in the normalized gesture space
-- (1000 x 1000 units). A lower value improves precision at the cost of
-- increased memory and processing time. The default in the original is: 5
local samplePointDistance = 10

-- Global pattern set
local patterns = {}

-- Performance optimizations: http://www.lua.org/gems/sample.pdf
-- Local references to 'in-loop' functions for math.*
local math_floor = math.floor
local math_sqrt = math.sqrt
local math_min = math.min
local math_abs = math.abs
local math_acos = math.acos
local math_exp = math.exp

-- Table optimizations
--     @ Small table usage, rehashing etc., not sure...

-- String optimizations
--     @ Nothing really string-based here except some IDs

-- Reduce, reuse, recycle
--     @ Change Pt function to use vec2() ** DONE **
--     @ Have Pt lists as two arrays, x and y, of numbers
--     @ Check table creation in loops ** DONE **
--
--     Possible candidates/solutions:
--         +1 CGR_recognize to take a table to populate as an argument, can be
--            primed with #patterns entries and then reused ** DONE **
--         +2 Create a table pool and never use {} except from pool

-- ----------------------------------------------------------------------------
-- Create Functions
-- ----------------------------------------------------------------------------
-- Function to create a CGR_IncrementalResult object
local function CGR_create_IncrementalResult(pattern, prob, indexOfMostLikelySegment)
    local obj = {}
    obj.pattern = pattern
    obj.prob = prob
    obj.indexOfMostLikelySegment = indexOfMostLikelySegment
    return obj
end

-- Function to create a CGR_Centroid object
local function CGR_create_Centroid(x, y)
    local obj = {}
    obj.x = x
    obj.y = y
    return obj
end

-- Function to create a CGR_Rect object
local function CGR_create_Rect(x, y, width, height)
    local obj = {}
    obj.x = x
    obj.y = y
    obj.width = width
    obj.height = height
    return obj
end

-- Local member
local normalizedSpace = CGR_create_Rect(0, 0, 1000, 1000)
-- Function to create a CGR_Pattern object
local function CGR_create_Pattern(template, segments)
    local obj = {}
    obj.template = template
    obj.segments = segments
    return obj
end

-- Function to create a CGR_Result object
local function CGR_create_Result(template, prob, pts)
    local obj = {}
    obj.template = template
    obj.prob = prob
    obj.pts = pts
    return obj
end

local function CGR_compareResult(p1, p2)
    if p1.prob == p2.prob then
        return 0
    elseif p1.prob < p2.prob then
        return 1
    else
        return -1
    end

end

-- Function to create a CGR_Pt object
function CGR_create_Pt(x, y)
    --local obj = {}
    --obj.x = x
    --obj.y = y

    return vec2(x,y)
end

-- Function to create a CGR_Template object
local function CGR_create_Template(id, pts)
    local obj = {}
    obj.id = id
    obj.pts = pts
    return obj
end

-- ----------------------------------------------------------------------------
-- Main Functions
-- ----------------------------------------------------------------------------
-- Main function: toArray
local function CGR_toArray(points)
    local out = {}
    local count = 1
    for i = 1, #points * 2, 2 do
        out[i] = points[count].x
        out[i + 1] = points[count].y
        count = count + 1
    end

    return out
end

-- Main function: deepCopyPts
local function CGR_deepCopyPts(p1)
    local out = {}
    for k, pt in ipairs(p1) do
        table.insert(out, CGR_create_Pt(pt.x, pt.y))
    end

    return out
end

-- Main function: getBoundingBox
local function CGR_getBoundingBox(pts)
    local minX = 1000000
    local minY = 1000000
    local maxX = -1000000
    local maxY = -1000000
    for k, pt in ipairs(pts) do
        local x = pt.x
        local y = pt.y
        if x < minX then
            minX = x
        end

        if x > maxX then
            maxX = x
        end

        if y < minY then
            minY = y
        end

        if y > maxY then
            maxY = y
        end

    end

    return CGR_create_Rect(minX, minY, (maxX - minX), (maxY - minY))
end

-- Main function: getCentroid
local function CGR_getCentroid(pts)
    local totalMass = #pts
    local xIntegral = 0.0
    local yIntegral = 0.0
    for k, pt in ipairs(pts) do
        xIntegral = pt.x + xIntegral
        yIntegral = pt.y + yIntegral
    end

    return CGR_create_Centroid(xIntegral / totalMass, yIntegral / totalMass)
end

-- Main function: translate
local function CGR_translate(pts, dx, dy)
    for k, v in ipairs(pts) do
        v.x = math_floor(dx) + v.x
        v.y = math_floor(dy) + v.y
    end

end

-- Main function: scale
local function CGR_scale(pts, p2, p3, p4, p5)
    -- Implementation Here
    if p4 == nil then
        local sx = p2
        local sy = p3
        for k, pt in ipairs(pts) do
            pt.x = pt.x * sx
            pt.y = pt.y * sy
        end

    else
        local sx = p2
        local sy = p3
        local originX = p4
        local originY = p5
        CGR_translate(pts, -originX, -originY)
        CGR_scale(pts, sx, sy)
        CGR_translate(pts, originX, originY)
    end

end

-- Main function: distance
local function CGR_distance(x1, y1, x2, y2)
    if x2 == nil then
        return CGR_distance(x1.x, x1.y, y1.x, y1.y)
    else
        -- Re-written, could be performance issues
        x2 = x2 - x1
        if (x2 < 0) then
            x2 = -x2
        end

        y2 = y2 - y1
        if (y2 < 0) then
            y2 = -y2
        end

        local fac
        if (x2 > y2) then
            fac = y2
        else
            fac = x2
        end

        -- Original Java used a bitshift >>1 here on fac, which is a fast
        -- division operator, e.g.:
        -- return (x2 + y2 - (((x2 > y2) ? y2 : x2) >> 1));
        return (x2 + y2 - (fac/2))
    end

end

-- Main function: getSpatialLength
local function CGR_getSpatialLength(p1, p2)
    if p2 == nil then
        local pts = p1
        local len = 0.0
        local prev
        for k, nxt in ipairs(pts) do
            if prev == nil then
                prev = nxt
            else
                len = len + CGR_distance(prev, nxt)
                prev = nxt
            end

        end

        return math_floor(len)
    else
        local pat = p1
        local n = p2
        local l
        local i, m
        local x1, y1, x2, y2
        l = 0
        m = 2 * n
        if m > 2 then
            x1 = pat[1]
            y1 = pat[2]
            for i = 3, m, 2 do
                x2 = pat[i]
                y2 = pat[i + 1]
                l = CGR_distance(x1, y1, x2, y2) + l
                x1 = x2
                y1 = y2
            end

            return math_floor(l)
        else
            return 0
        end

    end

end

-- Main function: getResamplingPointCount
local function CGR_getResamplingPointCount(pts, samplePointDistance)
    local len = CGR_getSpatialLength(pts)
    --System.out.println("len: " .. len .. ", samp: " .. samplePointDistance .. " res: " .. ((len / samplePointDistance) + 1))
    return math_floor((len / samplePointDistance) + 1)
end

-- Main function: getSegmentPoints
local function CGR_getSegmentPoints(pts, n, length, buffer)
    local i, m
    local x1, y1, x2, y2, ps
    local rest, currentLen
    m = n * 2
    rest = 0.0
    x1 = pts[1]
    y1 = pts[2]
    for i = 3, m, 2 do
        --doPrint("i:", i)
        x2 = pts[i]
        y2 = pts[i + 1]
        currentLen = CGR_distance(x1, y1, x2, y2)
        currentLen = currentLen + rest
        rest = 0.0
        ps = currentLen / length
        if (ps == 0) then
            rest = rest + currentLen
        else
            rest = rest + currentLen - (ps * length)
        end

        if (i == 3 and ps == 0) then
            ps = 1
        end

        buffer[(i / 2) - 1] = ps
        x1 = x2
        y1 = y2
    end

    return rest
end

-- Main function: resample
local function CGR_resample(p1, p2, p3, p4)
    -- Two parameter version
    if p3 == nil then
        local points = p1
        local numTargetPoints = p2
        local r = {}
        local inArray = CGR_toArray(points)
        local outArray = {}
        CGR_resample(inArray, outArray, #points, numTargetPoints)
        for i = 1, #outArray - 1, 2 do
            table.insert(r, CGR_create_Pt(outArray[i], outArray[i + 1]))
        end

        return r
    else
        -- Four parameter version
        local template = p1
        local buffer = p2
        local n = p3
        local numTargetPoints = p4
        local segment_buf = {}
        local l, segmentLen, horizRest, verticRest, dx, dy
        local x1, y1, x2, y2
        local i, m, a, segmentPoints, j, maxOutputs, theEnd
        m = n * 2
        l = CGR_getSpatialLength(template, n)
        segmentLen = l / (numTargetPoints - 1)
        CGR_getSegmentPoints(template, n, segmentLen, segment_buf)
        horizRest = 0.0
        verticRest = 0.0
        x1 = template[1]
        y1 = template[2]
        a = 1
        local cnt = 1
        maxOutputs = numTargetPoints * 2
        --doPrint("m:", m)
        for i = 3, m, 2 do
            x2 = template[i]
            y2 = template[i + 1]
            segmentPoints = segment_buf[(i / 2) - 1]
            dx = -1.0
            dy = -1.0
            if (segmentPoints - 1 <= 0) then
                dx = 0.0
                dy = 0.0
            else
                dx = (x2 - x1) / (segmentPoints)
                dy = (y2 - y1) / (segmentPoints)
            end

            --doPrint(i, ": ", " segmentPoints:", segmentPoints)
            if (segmentPoints > 0) then
                --doPrint(i, ": ", " segmentPoints:", segmentPoints)
                for j = 1, segmentPoints do
                    if (j == 1) then
                        if (a < maxOutputs) then
                            buffer[a] = (x1 + horizRest)
                            buffer[a + 1] = (y1 + verticRest)
                            horizRest = 0.0
                            verticRest = 0.0
                            a = a + 2
                        end

                    else
                        if (a < maxOutputs) then
                            buffer[a] = (x1 + j * dx)
                            buffer[a + 1] = (y1 + j * dy)
                            a = a + 2
                        end

                    end

                end

            end

            x1 = x2
            y1 = y2
        end

        theEnd = (numTargetPoints * 2) - 2
        if (a < theEnd) then
            for i = a, theEnd, 2 do
                buffer[i] = (buffer[i - 2] + template[m - 2]) / 2
                buffer[i + 1] = (buffer[i - 1] + template[m - 1]) / 2
            end

        end

        buffer[maxOutputs - 1] = template[m - 1]
        buffer[maxOutputs - 0] = template[m - 0]
    end

end

-- Main function: generateEquiDistantProgressiveSubSequences
local function CGR_generateEquiDistantProgressiveSubSequences(pts, ptSpacing)
    local sequences = {}
    local nSamplePoints = CGR_getResamplingPointCount(pts, ptSpacing)
    local resampledPts = CGR_resample(pts, nSamplePoints)
    for i = 1, #resampledPts do
        local subList = {}
        for j = 1, i do
            table.insert(subList, resampledPts[j])
        end

        local seq = CGR_deepCopyPts(subList)
        table.insert(sequences, seq)
    end

    return sequences
end

-- Main function: scaleTo
local function CGR_scaleTo(pts, targetBounds)
    local bounds = CGR_getBoundingBox(pts)
    local a1 = targetBounds.width
    local a2 = targetBounds.height
    local b1 = bounds.width
    local b2 = bounds.height
    local scale = math_sqrt(a1 * a1 + a2 * a2) / math_sqrt(b1 * b1 + b2 * b2)
    CGR_scale(pts, scale, scale, bounds.x, bounds.y)
end

-- Main function: normalize
local function CGR_normalize(pts, x, y, width, height)
    local out
    if x ~= nil then
        out = CGR_deepCopyPts(pts)
        CGR_scaleTo(out, CGR_create_Rect(0, 0, width - x, height - y))
        local c = CGR_getCentroid(out)
        CGR_translate(out, -c.x, -c.y)
        CGR_translate(out, width - x, height - y)
    else
        CGR_scaleTo(pts, normalizedSpace)
        local c = CGR_getCentroid(pts)
        CGR_translate(pts, -c.x, -c.y)
    end

    return out
end

-- Main function: setTemplateSet
function CGR_setTemplateSet(templates)
    for k, v in pairs(patterns) do
        patterns[k] = nil
    end

    for k, t in ipairs(templates) do
        CGR_normalize(t.pts)
        table.insert(patterns, CGR_create_Pattern(t, CGR_generateEquiDistantProgressiveSubSequences(t.pts, 200)))
    end

    for k, pattern in ipairs(patterns) do
        local segments = {}
        for l, pts in ipairs(pattern.segments) do
            local newPts = CGR_deepCopyPts(pts)
            CGR_normalize(newPts)
            table.insert(segments, CGR_resample(newPts, CGR_getResamplingPointCount(newPts, samplePointDistance)))
        end

        pattern.segments = segments
    end

end

-- Main function: marginalizeIncrementalResults
local function CGR_marginalizeIncrementalResults(p1)
    local totalMass = 0.0
    for k, r in ipairs(p1) do
        totalMass = r.prob + totalMass
    end

    for k, r in ipairs(p1) do
        r.prob = r.prob / totalMass
    end

end

-- Main function: getSquaredEuclidenDistance
local function CGR_getSquaredEuclidenDistance(pt1, pt2)
    return (pt1.x - pt2.x) * (pt1.x - pt2.x) + (pt1.y - pt2.y) * (pt1.y - pt2.y)
end

-- Main function: getEuclideanDistance
local function CGR_getEuclideanDistance(pt1, pt2)
    return math_sqrt(CGR_getSquaredEuclidenDistance(pt1, pt2))
end

-- Main function: getEuclidianDistance
local function CGR_getEuclidianDistanceByList(pts1, pts2)
    -- HACK: Mismatched lists for some reason
    local n = #pts1
    if (#pts1 ~= #pts2) then
        error("lists must be of equal lengths, cf. " .. #pts1 .. " with " .. #pts2)
        n = math_min(#pts1, #pts2)
    end

    local td = 0
    for i = 1, n do
        td = td + CGR_getEuclideanDistance(pts1[i], pts2[i])
    end

    return td / n
end

-- Main function: getTurningAngleDistance
local function CGR_getTurningAngleDistance(ptA1, ptA2, ptB1, ptB2)
    if ptB1 == nil then
        local pts1 = ptA1
        local pts2 = ptA2
        -- HACK: Mismatched lists for some reason
        local n = #pts1
        if (#pts1 ~= #pts2) then
            error("lists must be of equal lengths, cf. " .. #pts1 .. " with " .. #pts2)
            n = math_min(#pts1, #pts2)
        end

        local td = 0
        for i = 1, n - 1 do
            td = td + math_abs(CGR_getTurningAngleDistance(pts1[i], pts1[i + 1], pts2[i], pts2[i + 1]))
        end

        if td ~= td then
            return 0.0
        end

        return td / (n - 1)
    else
        local len_a = CGR_getEuclideanDistance(ptA1, ptA2)
        local len_b = CGR_getEuclideanDistance(ptB1, ptB2)
        if (len_a == 0 or len_b == 0) then
            return 0.0
        else
            local cos = (((ptA1.x - ptA2.x) * (ptB1.x - ptB2.x) + (ptA1.y - ptA2.y) * (ptB1.y - ptB2.y)) / (len_a * len_b))
            if (math_abs(cos) > 1.0) then
                return 0.0
            else
                return math_acos(cos)
            end

        end

    end

end

-- Main function: getLikelihoodOfMatch
local function CGR_getLikelihoodOfMatch(pts1, pts2, eSigma, aSigma, lambda)
    if (eSigma == 0 or eSigma < 0) then
        error("eSigma must be positive")
    end

    if (aSigma == 0 or eSigma < 0) then
        error("aSigma must be positive")
    end

    if (lambda < 0 or lambda > 1) then
        error("lambda must be in the range between zero and one")
    end

    local x_e = CGR_getEuclidianDistanceByList(pts1, pts2)
    local x_a = CGR_getTurningAngleDistance(pts1, pts2)
    return math_exp(-(x_e * x_e / (eSigma * eSigma) * lambda + x_a * x_a / (aSigma * aSigma) * (1 - lambda)))
end

-- Main function: getIncrementalResult
local function CGR_getIncrementalResult(unkPts, pattern, beta, lambda, e_sigma)
    -- List<List<Pt>> segments = pattern.segments
    local segments = pattern.segments
    local maxProb = 0.0
    local maxIndex = -1
    for i = 1, #segments do
        local pts = segments[i]
        local samplingPtCount = #pts
        local unkResampledPts = CGR_resample(unkPts, samplingPtCount)
        local prob = CGR_getLikelihoodOfMatch(unkResampledPts, pts, e_sigma, e_sigma / beta, lambda)
        if prob > maxProb then
            maxProb = prob
            maxIndex = i
        end

    end

    return CGR_create_IncrementalResult(pattern, maxProb, maxIndex)
end

-- Main function: getResults
function CGR_getResults(incrResults, inResults)
    local results = inResults or {}
    local index = 1
    for k, ir in ipairs(incrResults) do
        local r = CGR_create_Result(ir.pattern.template, ir.prob, ir.pattern.segments[ir.indexOfMostLikelySegment])
        results[index] = r
        index = index +1
    end

    return results
end

-- Main function: getIncrementalResults
local function CGR_getIncrementalResults(input, beta, lambda, kappa, e_sigma)
    local incrResults = {}
    local unkPts = CGR_deepCopyPts(input)
    CGR_normalize(unkPts)
    for k, pattern in ipairs(patterns) do
        local result = CGR_getIncrementalResult(unkPts, pattern, beta, lambda, e_sigma)
        local lastSegmentPts = pattern.segments[#pattern.segments]
        local completeProb = CGR_getLikelihoodOfMatch(CGR_resample(unkPts, #lastSegmentPts), lastSegmentPts, e_sigma, e_sigma / beta, lambda)
        local x = 1 - completeProb
        result.prob = (1 + kappa * math_exp(-x * x)) * result.prob
        table.insert(incrResults, result)
    end

    CGR_marginalizeIncrementalResults(incrResults)
    return incrResults
end

-- Main function: recognize
function CGR_recognize(p1, p2, p3, p4, p5, p6)
    if p2 == nil then
        return CGR_recognize(p1, DEFAULT_BETA, DEFAULT_LAMBDA, DEFAULT_KAPPA, DEFAULT_E_SIGMA)
    elseif p3 == nil then
        -- Performance optimization, p2 is an incoming table
        CGR_recognize(p1, DEFAULT_BETA, DEFAULT_LAMBDA, DEFAULT_KAPPA, DEFAULT_E_SIGMA, p2)
    else
        if (#p1 < 2) then
            error("CGR_recognize: Input must consist of at least two points")
        end

        local incResults = CGR_getIncrementalResults(p1, p2, p3, p4, p5)
        local results = CGR_getResults(incResults, p6)
        table.sort(results, function(a, b) return a.prob > b.prob
        end)
        return results
    end

end

-- Main function: scaleTo
local function CGR_scaleTo(pts, targetBounds)
    local bounds = CGR_getBoundingBox(pts)
    local a1 = targetBounds.width
    local a2 = targetBounds.height
    local b1 = bounds.width
    local b2 = bounds.height
    local scale = math_sqrt(a1 * a1 + a2 * a2) / math_sqrt(b1 * b1 + b2 * b2)
    CGR_scale(pts, scale, scale, bounds.x, bounds.y)
end

-- Test function: CGR_createExampleGestureTemplates
function CGR_createDirectionalTemplates()
    -- Template list
    local directionalTemplates = {}
    -- Individual templates, each  atable with a number of points defining each gesture
    local nPoints = {}
    table.insert(nPoints, CGR_create_Pt(0, 0))
    table.insert(nPoints, CGR_create_Pt(0, -1))
    table.insert(directionalTemplates, CGR_create_Template("North", nPoints))
    local sPoints = {}
    table.insert(sPoints, CGR_create_Pt(0, 0))
    table.insert(sPoints, CGR_create_Pt(0, 1))
    table.insert(directionalTemplates, CGR_create_Template("South", sPoints))
    local wPoints = {}
    table.insert(wPoints, CGR_create_Pt(0, 0))
    table.insert(wPoints, CGR_create_Pt(-1, 0))
    table.insert(directionalTemplates, CGR_create_Template("West", wPoints))
    local ePoints = {}
    table.insert(ePoints, CGR_create_Pt(0, 0))
    table.insert(ePoints, CGR_create_Pt(1, 0))
    table.insert(directionalTemplates, CGR_create_Template("East", ePoints))
    local nwPoints = {}
    table.insert(nwPoints, CGR_create_Pt(0, 0))
    table.insert(nwPoints, CGR_create_Pt(-1, -1))
    table.insert(directionalTemplates, CGR_create_Template("NorthWest", nwPoints))
    local nePoints = {}
    table.insert(nePoints, CGR_create_Pt(0, 0))
    table.insert(nePoints, CGR_create_Pt(1, -1))
    table.insert(directionalTemplates, CGR_create_Template("NorthEast", nePoints))
    local swPoints = {}
    table.insert(swPoints, CGR_create_Pt(0, 0))
    table.insert(swPoints, CGR_create_Pt(-1, 1))
    table.insert(directionalTemplates, CGR_create_Template("SouthWest", swPoints))
    local sePoints = {}
    table.insert(sePoints, CGR_create_Pt(0, 0))
    table.insert(sePoints, CGR_create_Pt(1, 1))
    table.insert(directionalTemplates, CGR_create_Template("SouthEast", sePoints))
    return directionalTemplates
end

function CGR_createOtherTemplates()
    -- Template list
    local directionalTemplates = {}
    local t1Points = {}
    table.insert(t1Points, CGR_create_Pt(0, 0))
    table.insert(t1Points, CGR_create_Pt(0, 1))
    table.insert(directionalTemplates, CGR_create_Template("Template 1", t1Points))
    local t2Points = {}
    table.insert(t2Points, CGR_create_Pt(0, 0))
    table.insert(t2Points, CGR_create_Pt(0, 1))
    table.insert(t2Points, CGR_create_Pt(1, 1))
    table.insert(directionalTemplates, CGR_create_Template("Template 2", t2Points))
    local t3Points = {}
    table.insert(t3Points, CGR_create_Pt(0, 0))
    table.insert(t3Points, CGR_create_Pt(0, 1))
    table.insert(t3Points, CGR_create_Pt(-1, 1))
    table.insert(directionalTemplates, CGR_create_Template("Template 3", t3Points))
    local t4Points = {}
    table.insert(t4Points, CGR_create_Pt(0, 0))
    table.insert(t4Points, CGR_create_Pt(0, 1))
    table.insert(t4Points, CGR_create_Pt(1, 1))
    table.insert(t4Points, CGR_create_Pt(1, 2))
    table.insert(directionalTemplates, CGR_create_Template("Template 4", t4Points))
    local t5Points = {}
    table.insert(t5Points, CGR_create_Pt(-396, 102))
    table.insert(t5Points, CGR_create_Pt(93, -326))
    table.insert(t5Points, CGR_create_Pt(-266, 281))
    table.insert(t5Points, CGR_create_Pt(-108, -362))
    table.insert(directionalTemplates, CGR_create_Template("Template Zag", t5Points))
    return directionalTemplates
end

-- Test function: CGR_createExampleGestureTemplates
local function CGR_getExampleGestureSamplingData()
    -- Points sampled from a simple Java GUI storing points from mouse
    -- movements, this is a vertical-ish line dragged North to South
    local gesturePointsList = {}
    table.insert(gesturePointsList, CGR_create_Pt(258, 38))
    table.insert(gesturePointsList, CGR_create_Pt(258, 39))
    table.insert(gesturePointsList, CGR_create_Pt(258, 41))
    table.insert(gesturePointsList, CGR_create_Pt(258, 46))
    table.insert(gesturePointsList, CGR_create_Pt(258, 48))
    table.insert(gesturePointsList, CGR_create_Pt(258, 51))
    table.insert(gesturePointsList, CGR_create_Pt(258, 53))
    table.insert(gesturePointsList, CGR_create_Pt(258, 56))
    table.insert(gesturePointsList, CGR_create_Pt(258, 58))
    table.insert(gesturePointsList, CGR_create_Pt(258, 59))
    table.insert(gesturePointsList, CGR_create_Pt(258, 61))
    table.insert(gesturePointsList, CGR_create_Pt(258, 62))
    table.insert(gesturePointsList, CGR_create_Pt(258, 65))
    table.insert(gesturePointsList, CGR_create_Pt(258, 66))
    table.insert(gesturePointsList, CGR_create_Pt(258, 68))
    table.insert(gesturePointsList, CGR_create_Pt(258, 73))
    table.insert(gesturePointsList, CGR_create_Pt(259, 77))
    table.insert(gesturePointsList, CGR_create_Pt(259, 78))
    table.insert(gesturePointsList, CGR_create_Pt(259, 80))
    table.insert(gesturePointsList, CGR_create_Pt(259, 82))
    table.insert(gesturePointsList, CGR_create_Pt(259, 83))
    table.insert(gesturePointsList, CGR_create_Pt(259, 85))
    table.insert(gesturePointsList, CGR_create_Pt(259, 88))
    table.insert(gesturePointsList, CGR_create_Pt(259, 89))
    table.insert(gesturePointsList, CGR_create_Pt(259, 92))
    table.insert(gesturePointsList, CGR_create_Pt(259, 96))
    table.insert(gesturePointsList, CGR_create_Pt(259, 99))
    table.insert(gesturePointsList, CGR_create_Pt(259, 102))
    table.insert(gesturePointsList, CGR_create_Pt(259, 105))
    table.insert(gesturePointsList, CGR_create_Pt(259, 106))
    table.insert(gesturePointsList, CGR_create_Pt(259, 108))
    table.insert(gesturePointsList, CGR_create_Pt(259, 110))
    table.insert(gesturePointsList, CGR_create_Pt(259, 111))
    table.insert(gesturePointsList, CGR_create_Pt(259, 112))
    table.insert(gesturePointsList, CGR_create_Pt(259, 113))
    table.insert(gesturePointsList, CGR_create_Pt(259, 114))
    table.insert(gesturePointsList, CGR_create_Pt(259, 115))
    table.insert(gesturePointsList, CGR_create_Pt(259, 116))
    table.insert(gesturePointsList, CGR_create_Pt(259, 117))
    table.insert(gesturePointsList, CGR_create_Pt(259, 118))
    table.insert(gesturePointsList, CGR_create_Pt(259, 119))
    table.insert(gesturePointsList, CGR_create_Pt(259, 120))
    table.insert(gesturePointsList, CGR_create_Pt(259, 121))
    table.insert(gesturePointsList, CGR_create_Pt(259, 122))
    table.insert(gesturePointsList, CGR_create_Pt(259, 123))
    table.insert(gesturePointsList, CGR_create_Pt(259, 124))
    table.insert(gesturePointsList, CGR_create_Pt(259, 125))
    table.insert(gesturePointsList, CGR_create_Pt(259, 126))
    table.insert(gesturePointsList, CGR_create_Pt(259, 127))
    table.insert(gesturePointsList, CGR_create_Pt(259, 128))
    table.insert(gesturePointsList, CGR_create_Pt(259, 129))
    table.insert(gesturePointsList, CGR_create_Pt(259, 130))
    table.insert(gesturePointsList, CGR_create_Pt(259, 131))
    table.insert(gesturePointsList, CGR_create_Pt(259, 132))
    table.insert(gesturePointsList, CGR_create_Pt(259, 133))
    table.insert(gesturePointsList, CGR_create_Pt(259, 134))
    table.insert(gesturePointsList, CGR_create_Pt(259, 135))
    table.insert(gesturePointsList, CGR_create_Pt(259, 136))
    table.insert(gesturePointsList, CGR_create_Pt(259, 137))
    table.insert(gesturePointsList, CGR_create_Pt(259, 138))
    table.insert(gesturePointsList, CGR_create_Pt(259, 139))
    table.insert(gesturePointsList, CGR_create_Pt(259, 140))
    table.insert(gesturePointsList, CGR_create_Pt(259, 141))
    table.insert(gesturePointsList, CGR_create_Pt(259, 142))
    table.insert(gesturePointsList, CGR_create_Pt(259, 143))
    table.insert(gesturePointsList, CGR_create_Pt(259, 144))
    table.insert(gesturePointsList, CGR_create_Pt(259, 145))
    table.insert(gesturePointsList, CGR_create_Pt(259, 146))
    table.insert(gesturePointsList, CGR_create_Pt(259, 147))
    table.insert(gesturePointsList, CGR_create_Pt(259, 148))
    table.insert(gesturePointsList, CGR_create_Pt(259, 149))
    table.insert(gesturePointsList, CGR_create_Pt(259, 150))
    table.insert(gesturePointsList, CGR_create_Pt(259, 151))
    table.insert(gesturePointsList, CGR_create_Pt(259, 152))
    table.insert(gesturePointsList, CGR_create_Pt(259, 152))
    return gesturePointsList
end

function CGR_checkResults(results)
    local r1 = results[1]
    local r2 = results[2]
    local similarity = (r2.prob / r1.prob) * r2.prob
    if (r1.prob > 0.7) then
        if similarity < 95 then
            print("CHECK: Using: " .. r1.template.id .. " : " .. r1.prob .. " : ")
        else
            print"CHECK: First two probabilities too close to call"
        end

    else
        print("CHECK: Probability not high enough (<0.7), discarding user input")
    end

    print()
end

function CGR_printResults(results)
    for k, result in ipairs(results) do
        print("Result: " .. result.template.id .. " : " .. result.prob .. " : ")
    end

    print()
end

local function CGR_cmdLineTest()
    CGR_setTemplateSet(CGR_createDirectionalTemplates())
    print(#patterns)
    local results = {nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,mil}
    CGR_recognize(CGR_getExampleGestureSamplingData(), results)
    -- Too many similar templates, Template 1 and South are identical, has
    -- 'watered down' probability
    CGR_printResults(results)
    CGR_checkResults(results);
    -- Create new templates
    local templates = CGR_createExampleGestureTemplates()
    -- Delete the 'Template [1-4]' templates, we happen to know they start at index
    -- 9, and will 'shuffle up' as we delete them...
    table.remove(templates, 9)
    table.remove(templates, 9)
    table.remove(templates, 9)
    table.remove(templates, 9)
    -- Set the new template set
    CGR_setTemplateSet(templates)
    --
    CGR_recognize(CGR_getExampleGestureSamplingData(), results)
    CGR_printResults(results)
    CGR_checkResults(results);
end

--CGR_cmdLineTest()
