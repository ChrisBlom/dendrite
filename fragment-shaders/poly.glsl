#version 330
in vec4 c;
in vec3 p;
out vec4 fragColor;

void main(){

  float id = max( (p.x * p.x) , (p.y * p.y) );

  float m = 0.5 + id * 0.5;

  fragColor = vec4(c.r*m+1,
		   c.g*m,
		   c.b*m,
		   0.8+(0.2*sin(20*m))
		   );
}
