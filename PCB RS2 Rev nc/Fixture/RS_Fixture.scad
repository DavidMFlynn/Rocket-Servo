// ***************************************************
// Project: Rocket Servo 2
// Filename: RS_Fixture.scad
// Created: 12/20/2025
// Revision: 1.0  12/20/2025
// Units: mm
// **************************************************
//  ***** for STL output *****
//
//	Fixture();
//
// **************************************************
include<CommonStuffSAEmm.scad>

IDXtra=0.2;
Overlap=0.05;
$fn=90;


PCB_X=30;
PCB_Y=52.8;
PCB_Z=1.7;

nPCBsInX=4;
nPCBsInY=3;
PCB_Spacing_X=8;
PCB_Spacing_Y=8;
FixtueBorder_W=12;
Fixture_H=10;

module PCB_Hole(Xmod,Ymod){
	RoundRect(X=PCB_X+IDXtra*2+Xmod, Y=PCB_Y+IDXtra*2+Ymod, Z=Fixture_H+PCB_Z+Overlap*2, R=0.1);
} // PCB_Hole

// PCB_Hole();

module Fixture(nX=nPCBsInX, nY=nPCBsInY){
	difference(){
		RoundRect(X=(nX-1)*PCB_Spacing_X+nX*PCB_X+FixtueBorder_W*2,
					Y=(nY-1)*PCB_Spacing_Y+nY*PCB_Y+FixtueBorder_W*2, Z=Fixture_H+PCB_Z, R=FixtueBorder_W/2);
					
		for (j=[0:nX-1]) for (k=[0:nY-1])
			translate([-((nX-1)*PCB_Spacing_X+nX*PCB_X)/2+PCB_X/2, -((nY-1)*PCB_Spacing_Y+nY*PCB_Y)/2+PCB_Y/2, -Overlap])
				translate([j*(PCB_X+PCB_Spacing_X),k*(PCB_Y+PCB_Spacing_Y),0]){
					PCB_Hole(0,-2);
					translate([0,0,Fixture_H]) PCB_Hole(0,0);
				}
			
	} // difference
} // Fixture

// Fixture();
















































