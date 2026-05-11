// Soldering fixture for Rocket Servo PCBA

Overlap=0.05;

/*
// Rocket Servo Rev C
PCB_X=61;
Cavity_X=PCB_X-3;
PCB_Y=15;
Cavity_Y=PCB_Y;
PCB_t=1.6;
nPCB_X=3;
nPCB_Y=5;
Space_x=5;
Space_y=5;
Frame_t=5.5;
RemoveMid_X=false;
/**/

//*
// Rocket Servo 2 Rev n/c
PCB_X=53;
Cavity_X=PCB_X;
PCB_Y=30;
Cavity_Y=PCB_Y-3;
PCB_t=1.6;
nPCB_X=3;
nPCB_Y=4;
Space_x=5;
Space_y=5;
Frame_t=11;
RemoveMid_X=true;
/**/

module RoundRect(X=10,Y=10,Z=5,R=2){
	hull(){
		translate([-X/2+R, -Y/2+R, 0]) cylinder(r=R, h=Z);
		translate([-X/2+R, Y/2-R, 0]) cylinder(r=R, h=Z);
		translate([X/2-R, -Y/2+R, 0]) cylinder(r=R, h=Z);
		translate([X/2-R, Y/2-R, 0]) cylinder(r=R, h=Z);
	} // hull
} // RoundRect

module Fixture(RemoveMid_X=RemoveMid_X){
	CornerSup_l=7;
	
	X_Size=(PCB_X+Space_x)*nPCB_X+10;
	Y_Size=(PCB_Y+Space_y)*nPCB_Y+10;
	
	difference(){
		translate([0,0,-Frame_t]) RoundRect(X=X_Size,Y=Y_Size,Z=Frame_t-0.5,R=5);
		
		translate([-X_Size/2+(PCB_X+Space_x)/2+5, -Y_Size/2+(PCB_Y+Space_y)/2+5, 0])
		for (x=[0:nPCB_X-1])
			for (y=[0:nPCB_Y-1]){
				translate([(PCB_X+Space_x)*x-Cavity_X/2, (PCB_Y+Space_y)*y-Cavity_Y/2, -Frame_t-Overlap])
					cube([Cavity_X, Cavity_Y, Frame_t]);
					
				if (RemoveMid_X)
				translate([(PCB_X+Space_x)*x-PCB_X/2+CornerSup_l, (PCB_Y+Space_y)*y-PCB_Y/2, -Frame_t-Overlap])
					cube([PCB_X-CornerSup_l*2, PCB_Y, Frame_t]);
				
				translate([(PCB_X+Space_x)*x-PCB_X/2, (PCB_Y+Space_y)*y-PCB_Y/2, -PCB_t])
					cube([PCB_X, PCB_Y, PCB_t+Overlap]);
			}
	} // difference

} // Fixture

Fixture();