#version 330
in vec4 c;
out vec4 fragColor;
void main(){
  fragColor = vec4(c.x,c.y,c.z,c.w);			;
}
