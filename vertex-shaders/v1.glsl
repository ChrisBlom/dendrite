#version 330
in vec2 position;
in vec3 color;

out vec4 c;
out vec3 p;

uniform mat4 MVP;
uniform float ENERGY;
uniform vec3 colormod;


void main(){
   gl_Position = MVP * vec4(position, 0.0, 0.6);
   //float d = sqrt( (position.x * position.x) + (position.y * position.y) ) ;
   //   float r = atan(position.x , position.y);
   //float v = (0.5+sin( (r*9.0) + ENERGY )*0.5);

   c = vec4(colormod.r,
	    colormod.g,
	    colormod.b,
	    ENERGY);

   p = vec3(position.x , position.y, atan(position.x,position.y) );

;

}
