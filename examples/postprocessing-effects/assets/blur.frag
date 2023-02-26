#version 330

in vec2 fragTexCoord;
in vec4 fragColor;

out vec4 finalColor;

uniform sampler2D texture0;
uniform vec4 colDiffuse;
uniform vec2 renderSize;

const float dc = 1 / sqrt(2); // Diagonal constant (sin (pi/4))
const float blurRadius = 1.0;
const vec2 samples[8] = vec2[](
    vec2(0, 1),
    vec2(dc, dc),
    vec2(1, 0),
    vec2(dc, -dc),
    vec2(0, -1),
    vec2(-dc, -dc),
    vec2(-1, 0),
    vec2(-dc, dc)
);

const float weights[15] = float[](0.02, 0.04, 0.06, 0.08, 0.1, 0.14, 0.18, 0.08, 0.06, 0.05, 0.05, 0.04, 0.04, 0.04, 0.02); // I messed around with these until I got something I liked

void main()
{
    vec3 baseColor = texture(texture0, fragTexCoord).rgb*weights[0];

    for (int j = 1; j < 15; j ++) {
        for (int i = 0; i < 8; i ++) {
            vec2 offsetPixels = blurRadius*j*samples[i]; // Further out on each iteration
            vec2 offsetAdjusted = offsetPixels / renderSize; // Normalize offset to range from 0 to 1
            vec2 finalPosition = clamp(fragTexCoord + offsetAdjusted, vec2(0, 0), vec2(1, 1)); // Prevent wrapping over to the other side of the texture
            vec3 offsetColor = texture(texture0, finalPosition, 0.0).rgb;
            baseColor += offsetColor / 8 * weights[j];
        }
    }

    finalColor = vec4(baseColor, 10.0);
}