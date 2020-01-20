#version 330 core

#define MAX_POINT_LIGHTS 10 // needed to make a static array

struct Material {
    sampler2D diffuse;
    vec3 diffuseColor;
    vec3 specularColor;
    float shininess;
};

struct DirectionalLight {
    vec3 direction;

    vec3 color;

    float ambientIntensity;
    float diffuseIntensity;
    float specularIntensity;
};

struct PointLight {
    vec3 position;

    vec3 color;

    float ambientIntensity;
    float diffuseIntensity;
    float specularIntensity;

    float constant;
    float linear;
    float quadratic;
};

in vec3 fragPos; // fragment position from vertex shader
in vec3 normal; // normal from vertex shader

in vec2 texCoords;


uniform int lightsNum; // current number of lights
uniform vec3 viewPos; // camera position
uniform DirectionalLight directionalLight;
uniform PointLight pointLights[MAX_POINT_LIGHTS]; // lights array
uniform Material material; // absorber material

uniform int mode;

out vec4 FragColor;

vec3 getDiffuseMaterial(Material material)
{
    if (mode == 0) 
    {
        return material.diffuseColor;
    }
    if (mode == 1)
    {
        return texture(material.diffuse, texCoords).rgb;
    }
}

vec3 getAmbient(Material material, vec3 lightColor, float ambientIntensity)
{
    return vec3(ambientIntensity) * lightColor * getDiffuseMaterial(material);
}

vec3 getDiffuse(Material material, vec3 lightColor, float diffuseIntensity, vec3 norm, vec3 lightDir)
{
    float diff = max(dot(norm, lightDir), 0.0); // cosine of angle between norm and lightDir

    return vec3(diffuseIntensity) * lightColor * diff * getDiffuseMaterial(material);
}

vec3 getSpecular(Material material, vec3 lightColor, float specularIntensity, vec3 norm, vec3 lightDir, vec3 viewDir)
{
    vec3 reflectDir = reflect(-lightDir, norm); // reflection direction
    float spec = pow(max(dot(viewDir, reflectDir), 0.0), material.shininess); // cosine of angle between viewDir and reflectDir to shininess power

    return vec3(specularIntensity) * lightColor * spec * material.specularColor;
}

vec3 getDirectionLighting(DirectionalLight light, vec3 norm, vec3 viewDir)
{
    vec3 lightDir = normalize(-light.direction);

    vec3 ambient = getAmbient(material, light.color, light.ambientIntensity);
    vec3 diffuse = getDiffuse(material, light.color, light.diffuseIntensity, norm, lightDir);
    vec3 specular = getSpecular(material, light.color, light.specularIntensity, norm, lightDir, viewDir);

    return (ambient + diffuse + specular);
}

vec3 getPointLighting(PointLight light, Material material, vec3 fragPos, vec3 norm, vec3 viewDir)
{
    vec3 lightDir = normalize(light.position - fragPos);

    vec3 ambient = getAmbient(material, light.color, light.ambientIntensity);
    vec3 diffuse = getDiffuse(material, light.color, light.diffuseIntensity, norm, lightDir);
    vec3 specular = getSpecular(material, light.color, light.specularIntensity, norm, lightDir, viewDir);

    // attenuation - light influence decreases with distance
    float distance = length(light.position - fragPos);
    float attenuation = 1.0 / (light.constant + light.linear * distance + light.quadratic * (distance * distance));

    ambient *= attenuation;
    diffuse *= attenuation;
    specular *= attenuation;

    return (ambient + diffuse + specular);
}

void main()
{
    vec3 norm = normalize(normal);
    vec3 viewDir = normalize(viewPos - fragPos);

    vec3 result = getDirectionLighting(directionalLight, norm, viewDir);

    for(int i = 0; i < lightsNum; i++)
        result += getPointLighting(pointLights[i], material, fragPos, norm, viewDir);

    FragColor = vec4(result, 1.0);
}