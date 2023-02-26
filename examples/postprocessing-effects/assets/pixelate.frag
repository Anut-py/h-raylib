#version 330

in vec2 fragTexCoord;
in vec4 fragColor;

out vec4 finalColor;

uniform sampler2D texture0;
uniform vec4 colDiffuse;

const float numPixels = 200.0;

void main()
{
    vec2 pixelizedCoord = floor(fragTexCoord * numPixels) / numPixels;
    finalColor = texture(texture0, pixelizedCoord)*colDiffuse*fragColor;
}