#version 330
in vec2 position;
in vec3 color;
out vec4 c;

uniform mat4 MVP;
uniform float ENERGY;
uniform vec3 colormod;

// FOOBAR
void main(){
   gl_Position = MVP * vec4(position, 0.0, 1.0);
   float d =   sqrt((position.x * position.x) + (position.y * position.y) );
   float v = 0.5;
   float dd = max( sin(d*2+1)*.5+.5 ,0);
   c = vec4(colormod.x,colormod.y,colormod.z,0.3+ 0.1* sqrt(ENERGY));
}
