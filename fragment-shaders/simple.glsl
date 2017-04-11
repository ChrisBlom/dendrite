#version 330
in vec4 c;
in vec3 p;
out vec4 fragColor;

void main(){

   float id = inversesqrt( (p.x * p.x) + (p.y * p.y) ) ;
   float d = sqrt( (p.x * p.x) + (p.y * p.y) ) ;
   float r = atan(p.x , p.y);
   float v = 1 + sin(r*7+id+c.a)*0.9;

   float edge = clamp(0.7-id*.09  , 0 , 0.3);
   float radial = clamp(v*id*0.1 , 0 , 1.0);
   float m = id*(c.a)*0.05  ;

   fragColor = vec4(c.b,
		    c.r,
		    c.g,

		    (edge + radial*c.a
		    )*m+0.2
		    //	    c.a*(edge/2*m + radial)*2

);

  //  fragColor = vec4(sin(c.x*8),c.y,c.z,c.w);
}
