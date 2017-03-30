#version 330
in vec4 c;
in vec3 p;
out vec4 fragColor;

void main(){

   float id = inversesqrt( (p.x * p.x) + (p.y * p.y) ) ;
   float r = atan(p.x , p.y);
   float v = 1 + sin(r*7)*0.5;

   float edge = clamp(0.4-id*.05  , 0 , 0.3);
   float radial = clamp(v*id*0.01 , 0 , 1.0);
   float m = 0.4+c.a*0;

   fragColor = vec4(c.b,
		    c.r,
		    c.g,
		    c.a*(edge/2*m + radial)*2

);

  //  fragColor = vec4(sin(c.x*8),c.y,c.z,c.w);
}
