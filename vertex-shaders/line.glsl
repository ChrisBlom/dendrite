#version 330
in vec2 position;
in vec3 color;

out vec4 c;
out vec3 p;

uniform mat4 MVP;
uniform float ENERGY;
uniform vec3 colormod;


void main(){
   gl_Position = MVP * vec4(position, 0.0, 1.0);
   c = vec4(colormod.r,
	    colormod.g,
	    colormod.b,
	    1);

   p = vec3(position.x, position.y,0);
}
