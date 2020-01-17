#version 330 core

#define MAX_POINT_LIGHTS 10

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

in vec3 fragPos;
in vec3 normal;

uniform int lightsNum;
uniform vec3 viewPos;
uniform PointLight pointLights[MAX_POINT_LIGHTS];
uniform Material material;

out vec4 FragColor;

vec3 CalcPointLight(PointLight light, vec3 normal, vec3 fragPos, vec3 viewDir);

void main()
{
    // properties
    vec3 norm = normalize(normal);
    vec3 viewDir = normalize(viewPos - fragPos);

    vec3 result = vec3(0.0);

    for(int i = 0; i < lightsNum; i++)
    {
        vec3 lightDir = normalize(pointLights[i].position - fragPos);

        // diffuse shading
        float diff = max(dot(norm, lightDir), 0.0);

        // specular shading
        vec3 reflectDir = reflect(-lightDir, norm);
        float spec = pow(max(dot(viewDir, reflectDir), 0.0), material.shininess);

        // attenuation
        float distance = length(pointLights[i].position - fragPos);
        float attenuation = 1.0 / (pointLights[i].constant + pointLights[i].linear * distance + pointLights[i].quadratic * (distance * distance));

        // combine results
        vec3 ambient = vec3(pointLights[i].ambientIntensity) * pointLights[i].color * material.diffuseColor;
        vec3 diffuse = vec3(pointLights[i].diffuseIntensity) * pointLights[i].color * diff * material.diffuseColor;
        vec3 specular = vec3(pointLights[i].specularIntensity) * pointLights[i].color * spec * material.specularColor;
        ambient *= attenuation;
        diffuse *= attenuation;
        specular *= attenuation;

        result += (ambient + diffuse + specular);
    }

    FragColor = vec4(result, 1.0);
}