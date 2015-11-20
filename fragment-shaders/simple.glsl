#version 330
in vec4 c;
in vec3 p;
out vec4 fragColor;

void main(){

   float id = inversesqrt( (p.x * p.x) + (p.y * p.y) ) ;
   float r = atan(p.x , p.y);
   float v = 1 + sin(r*3)*0.5;

   float edge = clamp(0.4-id*.05, 0 ,0.4);
   float radial = clamp(v*id*0.01 , 0 ,1);

   float m = 1+c.a;
   fragColor = vec4(c.b*m*.8,
		    c.r*m*.8,
		    c.g*m*.8,
		    //id*.015
		    //;
		    (edge + radial)

);

  //  fragColor = vec4(sin(c.x*8),c.y,c.z,c.w);
}
