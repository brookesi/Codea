-- This demo shows you how to use the CGR library with the CurrentTouch object for simple interaction
-- CurrentTouch is a global updated with a single touch position

function setup()
    -- Table of user input points
    gesturePointsList = {}
    CGR_setTemplateSet(CGR_createDirectionalTemplates())
    --CGR_setTemplateSet(CGR_createOtherTemplates())

    -- Pre-allocated results table
    results = {nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil}
end

function clearPoints(t)
    for k,v in pairs(t) do t[k]=nil end
end

function draw()
    background(0,0,0)

    if CurrentTouch.state == BEGAN then
        fill(16, 178, 197, 255)
        clearPoints(gesturePointsList)
        table.insert(gesturePointsList, CGR_create_Pt(CurrentTouch.x, HEIGHT-CurrentTouch.y))
        newTouch = true
    elseif CurrentTouch.state == MOVING then
        fill(255, 0, 0, 255)
        table.insert(gesturePointsList, CGR_create_Pt(CurrentTouch.x, HEIGHT-CurrentTouch.y))
    elseif CurrentTouch.state == ENDED then
        fill(210, 218, 16, 255)
        table.insert(gesturePointsList, CGR_create_Pt(CurrentTouch.x, HEIGHT-CurrentTouch.y))
        if newTouch then
            newTouch=false
            if CurrentTouch.tapCount == 0 then
                print("Gesture complete")
                CGR_recognize(gesturePointsList, results)
                CGR_printResults(results)
            else
                print("Tapped! " .. CurrentTouch.tapCount)
            end
       end
    end
    if results[1] then
        text(results[1].template.id .. " " .. math.floor(results[1].prob*100) .. "%", WIDTH/2, HEIGHT - 50)
    end
    for k, pt in ipairs(gesturePointsList) do
        ellipse(pt.x,HEIGHT-pt.y, 10,10)
    end
end
