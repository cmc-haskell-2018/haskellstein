#version 130

uniform sampler2D texture;
uniform int color_constant;

#define COLOR_KOEF 0.5
#define COLOR_RED 1
#define COLOR_GREEN 2
#define COLOR_BLUE 4

void main()
{
	vec4 pixel = texture2D(texture, gl_TexCoord[0].xy);
	pixel.r += (color_constant & COLOR_RED) * COLOR_KOEF;
	pixel.g += (color_constant & COLOR_GREEN) * COLOR_KOEF;
	pixel.b += (color_constant & COLOR_BLUE) * COLOR_KOEF;
	gl_FragColor = pixel * gl_Color;
}
