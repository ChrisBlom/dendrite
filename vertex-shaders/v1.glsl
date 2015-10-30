#version 330
in vec2 position;
in vec3 color;
out vec4 c;
uniform mat4 MVP;

void main(){
   gl_Position = MVP * vec4(position, 0.0, 0.1);
   float d = sqrt ( (position.x * position.x) + (position.y * position.y) ) ;
   float r = atan(position.x , position.y);
   float v = (0.5+sin(r*3)*0.5)*4;
   c = vec4(v,v,v,1-d*4);
}
