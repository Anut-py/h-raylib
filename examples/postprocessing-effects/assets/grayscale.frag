#version 330

in vec2 fragTexCoord;
in vec4 fragColor;

out vec4 finalColor;

uniform sampler2D texture0;
uniform vec4 colDiffuse;

void main()
{
    vec4 texelColor = texture(texture0, fragTexCoord)*colDiffuse*fragColor;

    float gray = dot(texelColor.rgb, vec3(0.299, 0.587, 0.114));

    finalColor = vec4(gray, gray, gray, texelColor.a);
}