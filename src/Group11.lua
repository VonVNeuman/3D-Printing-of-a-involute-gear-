 -- Case Study Cyber Physical Production Systems using Additive manufacturing
--- Group11: Brainiacs
--- Venkat Ramanan Rajashekar(22308591)
--- Akshay Karayi(22302496)
-- Kowshik Karnam Kesavalu(22304581)

--- Designing a UI box for components to handle the parameters

---Reference
-- [1]: Zhai, Guodong & Liang, Zhihao & Fu, Zihao. (2020). A Mathematical Model for Parametric Tooth Profile of Spur Gears. Mathematical Problems in Engineering. 2020.

---[2]: HANDBOOK OF DUDLEY’S. Practical gear design and manufacture.

---[3]: J I Pedrero, M Artes, C Garcia-Masia (2004). Determination of the effective path of contact of undercut involute gear teeth.Institution of Mechanical Engineers Part C Journal of Mechanical Engineering Science 1989-1996 (vols 203-210) 218(7):751-760 

no_of_teeth=ui_number("Number Of Teeth",10,10,25);				
module_gear=ui_number("Module",3,2,25);			
pressure_angle=ui_scalar("Pressure Angle",20,20,25);			
width=ui_scalar("Face Width",5,2,20);			
rotation = ui_numberBox ("Rotation of gear",0)*2;				 
f_r=ui_number("Fillet radius", 2,0,3)profile_shift=ui_scalar("Profile Shift Factor",0,-0.3,1);	
      --- Function to obtain Input parameters 

gear_parameters = function(z, m_t, alpha_t, c, x_coef, f_r, b)

    local involute_xy = {}

    m_t = m_t		-- transverse module of gear
    alpha_t = alpha_t    -- pressure angle
    z = z			-- number of teeth
    x_coef = x_coef	-- profile shift coefficient
    c = 0.167 * m_t		 -- clearance of tooth
    f_r =  f_r				-- fillet radius 
	
            -----------------------
--- Assigning Gear parameters

  d_p = z * m_t	-- (1) pitch circle diameter 
  r_p = d_p / 2	-- pitch circle radius
	
  d_b = d_p * math.cos(alpha_t)-- (2) base circle diameter 
  r_b = d_b / 2	-- base circle radius	

  d_a = (d_p + (2 * m_t * (1 + x_coef)))--(3) addendum circle diameter
  r_a = d_a / 2	-- addendum circle radius

  h_a = m_t*(1 +x_coef) + c	-- (4) addendum height

	
  d_f = m_t * (z - 2) - 2 * c	-- (5) root circle diameter
  r_f = d_f / 2	-- root circle radius
	
  h_r = m_t + c-- (6) root height

  d_involute = math.sqrt(math.pow(d_p * math.sin(alpha_t) - 2 *(h_a - (m_t * x_coef) - h_r *
(1 - math.sin(alpha_t))), 2) + d_b * d_b)
--- using the equation (1),(2),(4) and (6) we obtain the (7)Involute diameter 
	
r_involute = d_involute / 2-- form radius

---Tooth thickness calculation on the form circle

s = m_t*((math.pi/2))+ 2*x_coef* math.tan(alpha_t)--(8) thickness of the tooth along the pitch circle 

angle_f = math.acos((d_p*math.cos(alpha_t))/d_involute)-- (9) involute angle along the form circle 

angle_Ff = math.tan(angle_f) - angle_f;        -- (10)involute function of the involute angle 

s_f = d_involute*((s/d_p)+inv_a - angle_Ff)+(x_coef*math.tan(angle_f)); --From the above equations (11) the thickness of the gear teeth obtained  

omega = (s_f)/(0.5*d_involute); -- (12) angle swept along the form circle 
            ----------------------
--- Function to find the slope of the given curve
slope = function(shape) 
    return ((shape[2].y - shape[1].y) / (shape[2].x - shape[1].x))		
end
 ----------------------  								
circle = function(x0, y0, r, th)
return (v(x0 + r * math.cos(th), y0 + r * math.sin(th)))
end												
            -----------------------
--- Function to deliver the x and y coordinates of the involute function in relation to the base radius and involute angle
involutecurve = function(base_r, involuteangle)			
return v(base_r *(math.sin(involuteangle) - involuteangle *math.cos(involuteangle)), base_r *
(math.cos(involuteangle) + involuteangle *
math.sin(involuteangle)))
end

            -----------------------
--- Function to mirror the Involute curve w.r.t Y-axis
Mirror = function(xy_value) 
return v(-xy_value.x, xy_value.y) 
end		

--- Function to rotate the individual tooth profile relative to the circle center.
Rotation = function(angle, xy_value)
return v(math.cos(angle) * xy_value.x + math.sin(angle) * xy_value.y, math.cos(angle) * xy_value.y - math.sin(angle) * xy_value.x)
end
Angle = function(r2, r1) 
return (math.sqrt((r1 * r1 - r2 * r2) / (r2 * r2))) 
end	
           ----------------------- 



--- Function for defining the endpoints of an involute between base circle and addendum circle

    tooth_ang = (((math.pi * m_t / 2) + 2 * m_t * x_coef * math.tan(alpha_t)) /r_p + 2 * math.tan(alpha_t) - 2 * alpha_t)

    start_angle = Angle(r_b, r_b)
    stop_angle = Angle(r_b, r_a)
    resl = 100		

-- Intiating of fillet
    	local points = {}
    for i = 1, resl do
        points[i] = involutecurve(r_b, (start_angle + (stop_angle - start_angle) * i / resl))
    end
	
    m_s = slope(points)
    slope_angle = math.atan(m_s)
    parallel = {}-- parallel line point 

    parallel[1] = v(points[1].x + f_r * math.cos(slope_angle + math.pi / 2),points[1].y + f_r * math.sin(slope_angle + math.pi / 2))
 d = (parallel[1].y - m_s * parallel[1].x) / math.sqrt(m_s * m_s + 1)
    angle1 = math.asin(d / (f_r + r_b)) + slope_angle
    center_f = {} --Finding the center of the fillet curve
    center_f[1] = v((f_r + r_f) * math.cos(angle1), (f_r + r_f) * math.sin(angle1))

--- Generating the fillet curve

    slope_c = 2 * math.pi + math.atan(center_f[1].y / center_f[1].x)
    slope_a = 3 * math.pi / 2 + slope_angle

    for m = 1, z do
        th = 2* math.pi

        for i = 1, resl do
            involute_xy[#involute_xy + 1] =Rotation(th*m/z,circle(center_f[1].x,
                                                          center_f[1].y, f_r,
                                                          (slope_c + (slope_a - slope_c) * i / resl)))  -- fillet radius
    end
	
		start_angle = Angle(r_b, r_involute)
		stop_angle = Angle(r_b, r_a)

--- Generating tooth profile
    
    for i = 1, resl do
	     involute_xy[#involute_xy + 1] =Rotation(th*m/z, involutecurve(r_b, (start_angle +(stop_angle -start_angle) *i / resl)))-- actual involute                                              
       end
    
		for i = resl, 1, -1 do
		  involute_xy[#involute_xy + 1] =Rotation(th*m/z,Rotation(tooth_ang, Mirror(involutecurve(r_b,(start_angle +(stop_angle -start_angle) *i / resl)))))	-- mirrored   involute                                         
        
         end 
for i = resl, 1, -1 do
            involute_xy[#involute_xy + 1] =Rotation(th*m/z,Rotation(tooth_ang,Mirror(circle(center_f[1].x,center_f[1].y, f_r,(slope_c + (slope_a- slope_c) *i / resl)))))        -- mirrored fillet                                          
        end
    end


    involute_xy[#involute_xy + 1] = involute_xy[1]
	output = linear_extrude(v(0,0,width),involute_xy) -- actual tooth profile is developed
    return output
end

              -----------------------
function gear(i_p)--Input values from UI box
	return gear_parameters(
	i_p.z or 17,i_p.m_t or 3,i_p.alpha_t or 20,
	i_p.c or 0.2,i_p.x_coef or 0,i_p.f_r or 0,    i_p.width or 15    
    )
end

function circle_t(r)
	local x, y = 0, 0
	local XY={}
	for i = 1, 360 do
		local angle = i * math.pi / 180
		XY[i] = v(x + r * math.cos( angle ), y + r * math.sin( angle ))
  end  
  return XY
end
 
             -----------------------

function extrude(Contour, angle, dirctn_vector, scale_vector, z_steps)
    local no_contour= #Contour -- Number of points in the contour
    local Vertex= {} 

---Generating the contour
   for 
      j= 0,z_steps-1 do
      local phi= angle*j/(z_steps-1)
      local dirctn_vectorh= dirctn_vector*j/(z_steps-1)
      local scale_vectorh= (scale_vector - v(1,1,1))*(j/(z_steps-1)) + v(1,1,1)

      for
          i= 1,no_contour-1 do
          Vertex[i+j*no_contour]= v((dirctn_vectorh.x + scale_vectorh.x * (Contour[i].x*math.cos(phi) - Contour[i].y*math.sin(phi))),
 (dirctn_vectorh.y + scale_vectorh.y * (Contour[i].x*math.sin(phi) + Contour[i].y*math.cos(phi))),
                                (dirctn_vectorh.z * scale_vectorh.z))
      end

table.insert(Vertex,Vertex[1+j*no_contour]) --Points added to vertex table
   end

   local vert_sum_1 = v(0,0,0)
   local vert_sum_m = v(0,0,0)
   for i= 1,no_contour-1 do
      vert_sum_1= vert_sum_1 + Vertex[i]
      vertex_sum_m= vert_sum_m + Vertex[i+no_contour*(z_steps-1)]
   end    
   table.insert(Vertex,vert_sum_1/(no_contour-1)) 
   table.insert(Vertex,vertex_sum_m/(no_contour-1)) 

   index= {}	-- the index in the table with Vertex starts at zero
   local k = 1
   for j=0,z_steps-2 do
      for i= 0,no_contour-2 do
         index[k]=   v(i, i+1, i+no_contour) + v(1,1,1)*no_contour*j
         index[k+1]= v(i+1, i+no_contour+1, i+no_contour) + v(1,1,1)*no_contour*j
         k= k+2
      end
   end
   for i= 0,no_contour-2 do
      index[k]= v(i+1,i,no_contour*z_steps)
      k= k+1
   end
   for i= 0,no_contour-2 do
      index[k]= v(i+no_contour*(z_steps-1),i+1+no_contour*(z_steps-1),no_contour*z_steps+1)
      k= k+1
   end
   return(polyhedron(Vertex,index))
end

           -----------------------
---intializing parameter gear and pinon 
z_2 = no_of_teeth; ---pinion
z_1 = z_2+5;       ---gear
x=profile_shift
alpha_t = 20*math.pi/180  ---transverse pressure angle taken standard value
inv_a = math.tan(alpha_t) - alpha_t;---involute function


 -- involute function of Working pressure angle   ///
    inv_alpha_w= 2*((x+x)/(z_1+z_2))*math.tan(alpha_t) + inv_a;   --  Involute Of working pressure angle
--- calculation of working pressure angle  -----
	alpha_w=(((math.pow(3,(1/3)))*(math.pow(inv_alpha_w,(1/3)))) - (2*inv_alpha_w/5) + (math.pow(9/175*3,(2/3)))*(math.pow(inv_alpha_w,(5/3))) - (math.pow(2/175*3, (1/3)))
						*(math.pow(inv_alpha_w,(7/3))) - ((144/67375)*(math.pow(inv_alpha_w,(9/3))) + (3258/3128215)*(math.pow(3,(2/3)))*(math.pow(inv_alpha_w,(11/3)))
						- (49711/153278125)*(math.pow(3,(1/3)))*(math.pow(inv_alpha_w,(13/3))) - (1130112/9306171875)*(math.pow(inv_alpha_w,(15/3)))
						+ (5169659643/95304506171875)*(math.pow(3,(2/3)))*(math.pow(inv_alpha_w,(17/3)))))	-- approximation formula to obtain qorking pressure angle from its involute function. The formula is reffered from Dudely's Handbook of gears
		--------------------
		
	center_d= (module_gear*(z_1+z_2)/2)*(math.cos(alpha_t)/math.cos(alpha_w))	-- Center distance: Gears using working pressure angle


addOn_distance= z_1/4;


function pinion() -- function to generate pinion
	ext_gr = gear({z=no_of_teeth;m_t=module_gear;alpha_t=pressure_angle*math.pi/180;c=clearance;width=width;x_coef=profile_shift;f_r=f_r})
end

pinion()  -- Calling of the function

function gear1() --Function to generate gear shape

	ext_gr2 = gear({z=no_of_teeth+5;m_t=module_gear;alpha_t=pressure_angle*math.pi/180;c=clearance;width=width;x_coef=profile_shift;f_r=f_r})
end

gear1()      
angle = (((math.pi * module_gear / 2) + 2 * module_gear * profile_shift * math.tan(alpha_t)) / r_p + 2 * math.tan(alpha_t) - 2 * alpha_t)

rotation3 = rotate(0,0,angle*90/math.pi)				        
rotation4 = rotate(0,0,(angle*90/math.pi))
if (z_2%2==0)
then
rotation42 = rotate(0,0, ((angle*90/math.pi)+z_1/z_2)+1-z_2/12)
else 
rotation42 = rotate(0,0, ((angle*90/math.pi))+180/z_1)
end

 --- Formation of gear and pinion
r2 = rotate(0,0,rotation)
r3 = rotate(0,0,-rotation*z_2/z_1)

             -----------------------

bore_pinion=extrude(circle_t(addOn_distance),0,v(0,0,10000), v(1,1,1),20)	-- Bore formation of pinion
bore_diff=bore_pinion
bore_full=difference(ext_gr,bore_diff)

bore_gear=extrude(circle_t(addOn_distance*1.5),0,v(0,0,10000), v(1,1,1),20)	-- Bore formation of external gear
bore_diff2=bore_gear
bore_full2=difference(ext_gr2,bore_diff2)
             -----------------------

emit(translate(0,0,40)*r2*rotation4*difference(bore_full,bore_diff),5)	
	-- creates an pinion
emit(translate(0,center_d,40)*r3*rotation42*difference(bore_full2,bore_diff2),7)	
	-- creates an gear
-- Define the radius and height of the shaft
local radius = addOn_distance
local height = 80

-- Create a cylinder representing the shaft
shaft = cylinder(radius, height)

-- Set the shaft's position (optional, center it along the z-axis)
shaft = translate(0, 0,0) * shaft

-- Render the shaft
emit(shaft) -- Shaft 1
shaft2 = cylinder(radius*1.5, height) --Shaft 2
shaft2 = translate(0, center_d,0) * shaft2
emit(shaft2)

cube=ccube(180,200,1) -- Creation of base plate
cube = translate(4, 20,0) * cube
emit(cube)
            ---------------------------
  
