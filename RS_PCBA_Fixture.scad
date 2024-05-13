// Soldering fixture for Rocket Servo PCBA


PCB_X=61;
PCB_Y=15;
nPCB_X=3;
nPCB_Y=5;
Space_x=5;
Space_y=5;


module RoundRect(X=10,Y=10,Z=5,R=2){
	hull(){
		translate([-X/2+R, -Y/2+R, 0]) cylinder(r=R, h=Z);
		translate([-X/2+R, Y/2-R, 0]) cylinder(r=R, h=Z);
		translate([X/2-R, -Y/2+R, 0]) cylinder(r=R, h=Z);
		translate([X/2-R, Y/2-R, 0]) cylinder(r=R, h=Z);
	} // hull
} // RoundRect

module Fixture(){
	X_Size=(PCB_X+Space_x)*nPCB_X+10;
	Y_Size=(PCB_Y+Space_y)*nPCB_Y+10;
	
	difference(){
		translate([0,0,-5.5]) RoundRect(X=X_Size,Y=Y_Size,Z=5,R=5);
		
		translate([-X_Size/2+(PCB_X+Space_x)/2+5,-Y_Size/2+(PCB_Y+Space_y)/2+5,0])
		for (x=[0:nPCB_X-1])
			for (y=[0:nPCB_Y-1])
				translate([(PCB_X+Space_x)*x,(PCB_Y+Space_y)*y,0]){
					cube([PCB_X-3,PCB_Y,20],center=true);
					cube([PCB_X,PCB_Y,5],center=true);
				}
	} // difference

} // Fixture

Fixture();