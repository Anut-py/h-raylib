#version 330

in vec2 fragTexCoord;
in vec4 fragColor;

out vec4 finalColor;

uniform sampler2D texture0;
uniform vec4 colDiffuse;
uniform vec2 renderSize;

const float pixelSize = 5.0;

void main()
{
    vec2 pixelizedCoord = floor(fragTexCoord * renderSize / pixelSize) / renderSize * pixelSize;
    finalColor = texture(texture0, pixelizedCoord)*colDiffuse*fragColor;
}