#version 330

in vec2 fragTexCoord;
in vec4 fragColor;
in vec3 fragPosition;
in vec3 fragNormal;

out vec4 finalColor;

uniform float pointLightStrength;
uniform vec3 pointLightPosition;
uniform vec4 pointLightColor;
uniform float specularStrength;

uniform vec4 ambientLightColor;
uniform float ambientStrength;

uniform vec4 colDiffuse;
uniform vec3 viewPos;
uniform sampler2D texture0;

void main()
{
    vec4 texColor = texture(texture0, fragTexCoord);
    
    // Ambient lighting
    vec3 ambient = (ambientStrength * ambientLightColor).xyz;

    // Diffuse lighting
    vec3 normal = normalize(fragNormal);
    vec3 lightDir = normalize(pointLightPosition - fragPosition);
    float impact = max(dot(normal, lightDir), 0.0);
    vec3 diffuse = pointLightStrength * (impact * pointLightColor).xyz;

    // Specular highlights
    vec3 viewDir = normalize(viewPos - fragPosition);
    vec3 reflectDir = reflect(-lightDir, normal);
    float spec = pow(max(dot(viewDir, reflectDir), 0.0), 16);
    vec3 specular = specularStrength * spec * pointLightColor.xyz;

    finalColor = vec4(specular + diffuse + ambient, 1.0) * texColor * colDiffuse * fragColor;
}