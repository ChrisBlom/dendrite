#version 330
in vec2 position;
in vec3 color;
out vec4 c;

uniform mat4 MVP;
uniform float ENERGY;

// FOOBAR
void main(){
   gl_Position = MVP * vec4(position, 0.0, 0.6);
   float d =   (position.x * position.x) + (position.y * position.y) ;
   float r = atan(position.x , position.y);
   float v = 1;
   c = vec4(v*2,v,v,1-d*20);
}
