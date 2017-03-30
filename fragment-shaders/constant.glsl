#version 330
in vec4 c;
in vec3 p;
out vec4 fragColor;

void main(){

  fragColor = vec4(c.r,
		   c.g,
		   c.b,
		   c.a
		   );
}
