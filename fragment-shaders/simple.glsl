#version 330
in vec4 c;
in vec3 p;
out vec4 fragColor;

void main(){

   float id = inversesqrt( (p.x * p.x) + (p.y * p.y) ) ;
   float r = atan(p.x , p.y);
   float v = (0.5+sin(r*2)*0.3);

   float m = 1+c.a;
   fragColor = vec4(c.b*m*9,
		    c.r*m,
		    c.g*m,
		    //id*.015
		    v*id*0.015
);

  //  fragColor = vec4(sin(c.x*8),c.y,c.z,c.w);
}
