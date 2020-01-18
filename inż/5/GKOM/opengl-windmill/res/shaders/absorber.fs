#version 330 core

#define MAX_POINT_LIGHTS 10 // needed to make a static array

struct Material {
    vec3 diffuseColor;
    vec3 specularColor;
    float shininess;
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

uniform int lightsNum; // current number of lights
uniform vec3 viewPos; // camera position
uniform PointLight pointLights[MAX_POINT_LIGHTS]; // lights array
uniform Material material; // absorber material

out vec4 FragColor;

vec3 getAmbient(Material material, PointLight light)
{
    return vec3(light.ambientIntensity) * light.color * material.diffuseColor;
}

vec3 getDiffuse(Material material, PointLight light, vec3 norm, vec3 lightDir)
{
    float diff = max(dot(norm, lightDir), 0.0); // cosine of angle between norm and lightDir

    return vec3(light.diffuseIntensity) * light.color * diff * material.diffuseColor;
}

vec3 getSpecular(Material material, PointLight light, vec3 norm, vec3 lightDir, vec3 viewDir)
{
    vec3 reflectDir = reflect(-lightDir, norm); // reflection direction
    float spec = pow(max(dot(viewDir, reflectDir), 0.0), material.shininess); // cosine of angle between viewDir and reflectDir to shininess power

    return vec3(light.specularIntensity) * light.color * spec * material.specularColor;
}

void main()
{
    vec3 norm = normalize(normal);
    vec3 viewDir = normalize(viewPos - fragPos);

    vec3 result = vec3(0.0);

    for(int i = 0; i < lightsNum; i++)
    {
        vec3 lightDir = normalize(pointLights[i].position - fragPos);

        vec3 ambient = getAmbient(material, pointLights[i]);
        vec3 diffuse = getDiffuse(material, pointLights[i], norm, lightDir);
        vec3 specular = getSpecular(material, pointLights[i], norm, lightDir, viewDir);

        // attenuation - light influence decreases with distance
        float distance = length(pointLights[i].position - fragPos);
        float attenuation = 1.0 / (pointLights[i].constant + pointLights[i].linear * distance + pointLights[i].quadratic * (distance * distance));

        ambient *= attenuation;
        diffuse *= attenuation;
        specular *= attenuation;

        result += (ambient + diffuse + specular);
    }

    FragColor = vec4(result, 1.0);
}