
walkable_road = {}

-- TODO: recognize too high cliffs
-- TODO: handle strange mapgen heightmap inconsistencies at borders
-- TODO: handle the last/first road node in each mapchunk
-- TODO: allow for roads in x direction instead of z direction as well
-- paths are not built below min_path_height; bridges (2 nodes above that
--        level) are used in such a case
-- bridge pillars and tunnel lamps are limited to the current mapchunk
--        in height (a pillar in midair does not look more strange than
--        a pillar stretching too far up); if necessary, additional
--        parallel lines are added below the bridge_side_a and bridge_side_b
--        lines
-- the material for all nodes (normal path, slabs, stairs, bridge, tunnel)
--        is contained in the table "material"
-- trees: the entire trunk standing on the path is removed, but
--        sidewise branches remain standing
-- stairs and slabs are used where needed (mostly slabs if possible);
--        additional air is added above said stairs and slabs so that
--        the player can climb the stiars
-- the maximum length of bridges and tunnels can be configured

-- builds a nice wooden bridge
-- Note: this covers only bridges extending in z direction
walkable_road.build_bridge = function(start_x, path_wide, height, start_z, end_z, heightmap, minp, chunksize, material, min_height )
	-- no bridges in or below water level
	if(height < min_height) then
		height = min_height
	end
	-- do not build a bridge if its floor does not fit into the current mapchunk
	if(height < minp.y or height > minp.y + chunksize) then
		return
	end
	for j=start_z, end_z do
		-- note: the bridge floor is set when the path as such is placed
		-- trees at the side that hold the fences
		if(material.bridge_side_a) then
			minetest.set_node({x=start_x-1,        y=height, z=j}, material.bridge_side_a)
		end
		if(material.bridge_side_b) then
			minetest.set_node({x=start_x+path_wide,y=height, z=j}, material.bridge_side_b)
		end
		-- fences at the sides as a handrail
		if(material.bridge_fence_a) then
			minetest.set_node({x=start_x-1,        y=height+1,z=j}, material.bridge_fence_a)
		end
		if(material.bridge_fence_b) then
			minetest.set_node({x=start_x+path_wide,y=height+1,z=j}, material.bridge_fence_b)
		end
		-- time for a support-pillar at each side
		-- TODO: use cobble when in water?
		if(material.bridge_pillar
		   and (j-start_z)%(2*(path_wide+2)) == (path_wide+2)) then
			local h_base_a = heightmap[ ((j-minp.z)*chunksize) + (start_x-1-minp.x) ]
			if(not(h_base_a)) then
				h_base_a = minp.y-1
			end
			for l=math.max(minp.y, h_base_a), height-1 do

				minetest.set_node({x=start_x-1,y=l,z=j}, material.bridge_pillar)
				-- horizontal connections between the pillars
				if(material.bridge_pillar_cross and  l>0 and l%3==0 and l>minp.y) then
					for m=start_x, start_x+path_wide do
						minetest.set_node({x=m,y=l,z=j}, material.bridge_pillar_cross)
					end
				end
			end
			-- if we are at the mapchunk border, add additional horizontal connections in
			-- order to make the bridge look less cut off
			if(h_base_a < minp.y) then
				for o = -4 * path_wide, 4 * path_wide do
					minetest.set_node({x=start_x-1,y=minp.y,z=j+o}, material.bridge_side_a)
				end
			end
			-- second pillar
			local h_base_b = heightmap[ ((j-minp.z)*chunksize) + (start_x+path_wide-minp.x) ]
			if(not(h_base_b)) then
				h_base_b = minp.y-1
			end
			for l=math.max(minp.y, h_base_b), height-1 do
				minetest.set_node({x=start_x+path_wide,y=l,z=j}, material.bridge_pillar)
			end
			if(h_base_b < minp.y) then
				for o = -4 * path_wide, 4 * path_wide do
					minetest.set_node({x=start_x+path_wide,y=minp.y,z=j+o}, material.bridge_side_a)
				end
			end
		end
	end
end


-- a tunnel is (for now) a bit more simple;
-- the tunnel itself will be carved out with air later on in the process when the road is made walkable
walkable_road.build_tunnel = function(start_x, path_wide, height, start_z, end_z, heightmap, minp, chunksize, material )
	-- only place the lamps if we are still (roughly) inside the current mapchunk
	if(height+4 < minp.y or height+4 > minp.y + chunksize) then
		return
	end
	for j=start_z, end_z do
		-- add some lamps so that the tunnel is not too dark
		-- (but only if we are still below the surface)
		if(((j-start_z)%10) == 4
		  and heightmap[ ((j-minp.z)*chunksize) + (start_x+1-minp.x) ] > height+4) then
			minetest.set_node({x=start_x+1,y=height+4,z=j}, material.tunnel_lamp)
		end
	end
end


-- build a path extending in z direction
walkable_road.build_road = function(start_x, start_z, heightmap, minp, chunksize, material, path_wide, interval_size, max_bridge_length, max_tunnel_length, min_path_height, bridge_height)
	local get_hsum = function(start_x, z, heightmap, minp, chunksize, path_wide)
		local hsum = 0
		for x=start_x, start_x+path_wide-1 do
			local h = heightmap[ ((z-minp.z)*chunksize) + (x-minp.x) ]
			-- deal with areas where no height is available:
			--      might require a bridge or tunnel
			if(not(h)) then
				return nil
			end
			hsum = hsum + h
		end
		return hsum
	end

	-- sets all nodes over the entire width of the path to the desired node
	local set_road_nodes = function(start_x, path_wide, height, z, node)
		for x=start_x, start_x+path_wide-1 do
			minetest.set_node({x=x, y=height, z=z}, node)
		end
	end

	-- store the observed height averages (or rather, the sum of
	-- all observed heights accross the entire width of the path
	-- at a certain point of its length)
	local averages = {}
	-- the path goes into z direction (x: width)
	local z = start_z
	-- get the height sums at the beginning of the path
	local hsum = get_hsum(start_x, start_z, heightmap, minp, chunksize, path_wide)
	-- TODO
	if(hsum == nil) then
		hsum = minp.y * path_wide
	end

	-- store the last height used in order to be able to detect places that get too steep
	local last_height = math.floor((hsum/path_wide)+0.5)

	-- at the beginning, all height sums are the same
	for i=1, interval_size do
		averages[i] = hsum
	end
	local avg_height_sum = hsum * interval_size

--[[
	-- has the neighbouring mapchunk already been generated? if so, determine its
	-- road level
	local pos = minetest.find_nodes_in_area(
			{x=start_x, y=minp.y, z=start_z-math.min(16, interval_size-1)},
			{x=start_x, y=minp.y+chunksize, z=start_z},
			material.path_air)
	for i,p in ipairs(pos) do
		local i2 = start_z-p.z+1
		avg_height_sum = avg_height_sum - averages[i2]
		averages[i2] = (p.y - 2) * path_wide
		avg_height_sum = avg_height_sum + averages[i2]
		minetest.set_node(p, {name="wool:pink"})
	end
	pos = minetest.find_nodes_in_area(
			{x=start_x, y=minp.y, z=start_z+chunksize+1+math.min(16, interval_size-1)},
			{x=start_x, y=minp.y+chunksize, z=start_z+chunksize+1},
			material.path_air)
	for i,p in ipairs(pos) do
		local i2 = start_z-p.z+1
--		avg_height_sum = avg_height_sum - averages[i2]
--		averages[i2] = (p.y - 2) * path_wide
--		avg_height_sum = avg_height_sum + averages[i2]
		minetest.set_node(p, {name="wool:red"})
	end
	--minetest.set_node({x=start_x,y=minp.y+chunksize-1,z=start_z}, {name="wool:blue"})
--]]


	-- store at which height we placed the road with this algorithm
	local used_height = {}

	-- we want to take heights before and after the current position into account;
	-- therefore, we need to deal with an offset
	local offset=math.floor((interval_size/2)+0.5)
	local i = 1
	-- now walk along the length of the path
	for z=start_z, start_z + chunksize do
		local new_height = math.floor((avg_height_sum / (path_wide * interval_size))+0.5)
		-- detect places that are too steep
		if( math.abs(new_height - last_height) > 1.5 and false ) then
			-- create bridges or tunnels...
			new_height = last_height
		end
		last_height = new_height
		-- create the path at the calculated height
		for x=start_x, start_x+path_wide-1 do
			local old_height = heightmap[ ((z-minp.z)*chunksize) + (x-minp.x) ]
			-- store the heights used for the road
			used_height[z] = new_height
		end
		-- forget the oldest average
		avg_height_sum = avg_height_sum - averages[i]
		-- TODO: sum up the heights at the next position
		local hsum_new = get_hsum(start_x, z+offset, heightmap, minp, chunksize, path_wide)
		-- if the height at that point cannot be determined: stick to the old height
		if(hsum_new == nil) then
			hsum_new = averages[i]
		end
		averages[i] = hsum_new
		avg_height_sum = avg_height_sum + averages[i]
		-- move to the next index in the averages array
		i = (i % interval_size) + 1
	end

	--if(true) then return; end -- no bridge building

--	-- TODO: not really used
--	local extrema = {start_z+1}	
--	local last_d = 0
--	for z=start_z+1, start_z + chunksize do
--		local d = used_height[z] - used_height[z-1]
--		if(( (last_d > 0) and (d < 0) ) or ((last_d < 0) and (d > 0))) then
--			table.insert( extrema, z )
--		end
--		if( d ~= 0 ) then
--			last_d = d
--		end
--	end

	-- those two tables will be true for is_bridge[z] or is_tunnel[z] if
	--   there is a bridge or tunnel at position z (needed for placing
	--   diffrent materials there later on)
	local is_bridge = {}
	local is_tunnel = {}
	-- there may not be a ground inside the height boundaries of this mapchunk here
	local is_out_of_bounds = {}
	local z = start_z+1
	while( z <= start_z + chunksize ) do
		local d = used_height[z] - used_height[z-1]
		-- ground is outside of the mapchunk we are currently working on
		if(not(used_height[z-1]) or used_height[z-1] < minp.y or used_height[z-1] > minp.y+chunksize+1) then
			is_out_of_bounds[z-1] = true
			-- no way to tell the gradient
			d = 0
		-- we may need a bridge here
		elseif(used_height[z-1] < min_path_height) then
			used_height[z] = min_path_height
			-- force a bridge
			d = -2
		end
		-- if there is a hole we might want to build a bridge above it
		if(d<-1) then
			local i = z
			while( i<start_z + chunksize) do
				i = i+1
				local bridge_ends = true
				if(not(used_height[i])
				    or used_height[i] < min_path_height
				    or used_height[i] < used_height[z-1]) then
					bridge_ends = false
				elseif(used_height[i] == min_path_height) then
					local n = minetest.get_node({x=start_x,y=used_height[i],z=i})
					if(not(n) or not(n.name) or n.name=="default:water_source" or n.name=="air") then
						bridge_ends = false
					end
				end
				if(i-z > max_bridge_length) then
					bridge_ends = true
				end
				-- let the bridge span as many nodes as possible
				if(bridge_ends or i == start_z + chunksize) then
					walkable_road.build_bridge(start_x, path_wide, used_height[i], z-1, i+1,
						heightmap, minp, chunksize, material,
						-- looks better if there is a bit of room between water level
						-- and bridge floor
						min_path_height + bridge_height + 1)

					-- raise the path up to bridge level
					-- (the bridge has at least a height of min_path_height + 2)
					for k=z-1, i do
						used_height[k] = math.max(min_path_height + bridge_height + 1, used_height[i])
						is_bridge[k] = true
					end
					-- make sure the bridge can be reached (the entrance may be
					-- quite steep right now)
					local k = z
					local last_direction = 0
					local direction = 0
					while(k>minp.z and used_height[k-1]) do
						if(     used_height[k-1] < used_height[k] - 1) then
							-- make sure not to drop below water level with this
							used_height[k-1] = math.max(min_path_height, used_height[k] - 1)
							direction = -1
						elseif( used_height[k-1] > used_height[k] + 1) then
							used_height[k-1] = math.max(min_path_height, used_height[k] + 1)
							direction = 1
						end
						k = k-1
						-- abort searching once the gradient changes
						if(direction ~= last_direction and last_direction ~= 0) then
							k = minp.z
						end
						last_direction = direction
					end
					z = i
					-- end the loop
					i = start_z + chunksize + 1
				end
			end
		-- if there is a mountain we might want to dig a tunnel
		elseif(d>1) then
			local i = z
			while( i<start_z + chunksize) do
				i = i+1
				-- no tunnels below water level
				if(used_height[i] <= used_height[z-1] and used_height[i]>0 and (i-z <= max_tunnel_length)) then
					walkable_road.build_tunnel(start_x, path_wide, used_height[i], z, i, heightmap, minp, chunksize, material )
					-- lower the path to the floor of the tunnel
					for k=z, i do
						used_height[k] = math.max(min_path_height, used_height[i])
						is_tunnel[k] = true
					end
					z = i
					i = start_z + chunksize + 1
				end
			end
		end
		z = z+1
	end

	-- make sure there is no place on the road that has a gradient higher than one
	-- because stairs cannot cover more than that
	for z=start_z, start_z + chunksize do
		if(used_height[z] and used_height[z-1] and not(is_out_of_bounds[z-1]) and not(is_out_of_bounds[z])) then
			local d = used_height[z] - used_height[z-1]
			if(d > 1) then
				used_height[z] = used_height[z-1] + 1
			elseif( d < -1 ) then
				used_height[z] = used_height[z-1] - 1
			end

			-- also smoothen 1 node wide gaps or elevations
			if(used_height[z+1] and used_height[z+1]==used_height[z-1] and used_height[z]~=used_height[z-1]) then
				used_height[z] = used_height[z-1]
			end
		end
	end

	-- actually place the road nodes
	for z=start_z, start_z + chunksize do

		-- do not place anything if ground is not part of this mapchunk
		if(is_out_of_bounds[z] and not(is_bridge[z] or is_tunnel[z])) then
			break
		end
		-- if there is a tree trunk at ground level, clean up the entire tree
		-- (check the entire width of the path)
		for x=start_x, start_x+path_wide-1 do
			local height = heightmap[ ((z-minp.z)*chunksize) + (x-minp.x) ]
			if(height) then
				height = height + 1
				-- trees usually start at ground height
				local node = minetest.get_node({x=x, y=height, z=z})
				-- check upwards until we find a node that is not a tree
				while(node and node.name ~= "air" and node.name ~= "ignore"
				   and minetest.get_item_group(node.name, "tree")>0) do
					minetest.set_node({x=x, y=height, z=z}, {name="air"}) 
					height = height + 1
					node = minetest.get_node({x=start_x, y=height, z=z})
				end
			end
		end

		-- create the path at the calculated height
		if(     is_bridge[z] ) then
			set_road_nodes(start_x, path_wide, used_height[z], z, material.bridge_floor)
		elseif( is_tunnel[z] ) then
			set_road_nodes(start_x, path_wide, used_height[z], z, material.tunnel_floor)
		else
			set_road_nodes(start_x, path_wide, used_height[z], z, material.normal)
		end

		-- clear the area above the path so that there is room for walking
		for h=used_height[z]+1, used_height[z]+3 do
			set_road_nodes(start_x, path_wide, h, z, {name="air"})
		end
		set_road_nodes(start_x, path_wide, used_height[z]+2, z, material.path_air)

--		for x=start_x, start_x+path_wide-1 do
--			local old_height = heightmap[ ((z-minp.z)*chunksize) + (x-minp.x) ]
--			-- illustrate with glass if the path is lower than the original landscape
--			if(old_height and old_height>used_height[z] and old_height-used_height[z]<20 and old_height>=minp.y) then
--				for k=used_height[z]+1, old_height do
--					minetest.set_node({x=x,y=k,z=z},{name="default:glass"})
--				end
--			end
--		end
	end

	-- add slabs and stairs where possible
	for z=start_z, start_z + chunksize do
		-- there needs to be more room for the player in order to be able to use stairs etc
		local clear_headroom = nil
		-- do not place anything if ground is not part of this mapchunk
		if(is_out_of_bounds[z]) then
			-- do nothing
		-- candidate for a slab
		elseif(used_height[z] == used_height[z+1] and used_height[z-1] == used_height[z-2]) then
			if(     used_height[z-1] and used_height[z] == used_height[z-1] + 1) then
				set_road_nodes(start_x, path_wide, used_height[z], z-1, material.slab_up)
				clear_headroom = z-1
			elseif( used_height[z-1] and used_height[z] == used_height[z-1] - 1) then
				set_road_nodes(start_x, path_wide, used_height[z]+1, z, material.slab_down)
				clear_headroom = z
			end
		-- stair leading up
		elseif(used_height[z+1] and used_height[z] == used_height[z+1] -1 ) then
			set_road_nodes(start_x, path_wide, used_height[z]+1, z, material.stair_up)
			clear_headroom = z
		elseif(used_height[z-1] and used_height[z] == used_height[z-1] -1 ) then
			set_road_nodes(start_x, path_wide, used_height[z]+1, z, material.stair_down)
			clear_headroom = z
		end

		-- make sure there is enough room above so that the player can use the stair or slab
		if(clear_headroom) then
			for az = clear_headroom-1, clear_headroom+1 do
				if(used_height[az]) then
					set_road_nodes(start_x, path_wide, used_height[az]+4, az, {name="air"})
				end
			end
		end
	end
end


minetest.register_on_generated(function(minp, maxp, seed)
	if( minp.y < -64 or minp.y > 500) then
		return
	end

	local heightmap = minetest.get_mapgen_object('heightmap')
	-- the heightmap is necessary for the calculations; give up if none exists
	if(not(heightmap)) then
		return
	end

	local chunksize = maxp.x - minp.x + 1
	local dx=minp.x+3
	while(dx < minp.x + chunksize - 4) do
		walkable_road.build_road( dx, minp.z, heightmap, minp, chunksize, {
			-- the materials used
			normal         = {name="default:stonebrick"},
			slab_up        = {name="stairs:slab_stonebrick", param2=0},
			slab_down      = {name="stairs:slab_stonebrick", param2=0},
			stair_up       = {name="stairs:stair_stonebrick", param2=0},
			stair_down     = {name="stairs:stair_stonebrick", param2=2},
			bridge_floor   = {name="default:wood"},
			bridge_side_a  = {name="default:tree", param2=4},
			bridge_side_b  = {name="default:tree", param2=4},
			bridge_fence_a = {name="default:fence_wood"},
			bridge_fence_b = {name="default:fence_wood"},
			bridge_pillar  = {name="default:tree"},
			bridge_pillar_cross = {name="default:tree", param2=12},
			tunnel_floor   = {name="default:stonebrick"},
			tunnel_lamp    = {name="default:meselamp"},
			path_air       = {name="air"}, --flatpath:path_air"},
			}, 2, 5,
			-- max_bridge_length
			135,
			-- max_tunnel_length
			25,
			-- min_path_height (ought to be at water level)
			1,
			-- min_bridge_height (2 nodes between bridge and water - enough room for
			-- players in a boat)
			2)
		dx = dx + 8
	end
--	walkable_road.draw_line( minp.x+math.floor(chunksize/2), minp.z, heightmap, minp, chunksize, "default:meselamp", 2, 5)
end)

--[[
minetest.register_node("flatpath:path_air", {
        description = "Path air",
        drawtype = "airlike",
        paramtype = "light",
        sunlight_propagates = true,
        walkable = false,
        pointable = false,
        diggable = false,
        buildable_to = true,
        drop = "",
})
--]]

