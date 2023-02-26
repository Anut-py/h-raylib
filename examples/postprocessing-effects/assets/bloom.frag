#version 330

in vec2 fragTexCoord;
in vec4 fragColor;

out vec4 finalColor;

uniform sampler2D texture0;
uniform vec4 colDiffuse;
uniform vec2 renderSize;

const float samples = 3.0;
const int range = 1;
const float quality = 5.0;

void main()
{
    vec4 sum = vec4(0);
    vec2 sizeFactor = vec2(1)/renderSize*quality;

    vec4 source = texture(texture0, fragTexCoord);

    for (int x = -range; x <= range; x++)
    {
        for (int y = -range; y <= range; y++)
        {
            sum += texture(texture0, fragTexCoord + vec2(x, y)*sizeFactor);
        }
    }

    finalColor = ((sum/(samples*samples)) + source)*colDiffuse;
}