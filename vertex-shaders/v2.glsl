#version 330
in vec2 position;
in vec3 color;
out vec4 c;

uniform mat4 MVP;
uniform float ENERGY;

void main(){
   gl_Position = MVP * vec4(position, 0.0, 1);
   float d = sqrt ( (position.x * position.x) + (position.y * position.y) ) ;
   float r = atan(position.x , position.y);
   float v = 1-(0.5+sin( (r*9.0 + ENERGY) /*+ color.x*/ )*0.5);
   c = vec4(v,v*2,v*2,1-d*d*30);
}
