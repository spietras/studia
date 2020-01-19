#version 330 core

#define MAX_POINT_LIGHTS 10 // needed to make a static array

struct Material {
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
in vec4 fragPosLightSpace; // fragment position in directional light space

uniform int lightsNum; // current number of lights
uniform vec3 viewPos; // camera position
uniform DirectionalLight directionalLight;
uniform PointLight pointLights[MAX_POINT_LIGHTS]; // lights array
uniform Material material; // absorber material
uniform sampler2D shadowMap; // depth map

out vec4 FragColor;

float ShadowCalculation(vec4 fragPosLightSpace, vec3 normal, vec3 lightDir)
{
    // perform perspective divide (does nothing for orthogonal)
    vec3 projCoords = fragPosLightSpace.xyz / fragPosLightSpace.w;

    // transform NDC to [0,1] range
    projCoords = projCoords * 0.5 + 0.5;

    // get closest depth value from light's perspective (using [0,1] range fragPosLight as coords)
    float closestDepth = texture2D(shadowMap, projCoords.xy).r;

    // get depth of current fragment from light's perspective
    float currentDepth = projCoords.z;

    float BIAS_MIN = 0.01;
    float BIAS_MAX = 0.06;

    // bias to remove shadow acne
    float bias = max(BIAS_MAX * (1.0 - dot(normal, lightDir)), BIAS_MIN);

    // check whether current frag pos is in shadow
    float shadow = currentDepth - bias > closestDepth  ? 1.0 : 0.0;

    return shadow;
}

vec3 getAmbient(Material material, vec3 lightColor, float ambientIntensity)
{
    return vec3(ambientIntensity) * lightColor * material.diffuseColor;
}

vec3 getDiffuse(Material material, vec3 lightColor, float diffuseIntensity, vec3 norm, vec3 lightDir)
{
    float diff = max(dot(norm, lightDir), 0.0); // cosine of angle between norm and lightDir

    return vec3(diffuseIntensity) * lightColor * diff * material.diffuseColor;
}

vec3 getSpecular(Material material, vec3 lightColor, float specularIntensity, vec3 norm, vec3 lightDir, vec3 viewDir)
{
    vec3 reflectDir = reflect(-lightDir, norm); // reflection direction
    float spec = pow(max(dot(viewDir, reflectDir), 0.0), material.shininess); // cosine of angle between viewDir and reflectDir to shininess power

    return vec3(specularIntensity) * lightColor * spec * material.specularColor;
}

vec3 getDirectionLighting(DirectionalLight light, vec3 norm, vec3 viewDir, vec4 fragPosLightSpace)
{
    vec3 lightDir = normalize(-light.direction);

    vec3 ambient = getAmbient(material, light.color, light.ambientIntensity);
    vec3 diffuse = getDiffuse(material, light.color, light.diffuseIntensity, norm, lightDir);
    vec3 specular = getSpecular(material, light.color, light.specularIntensity, norm, lightDir, viewDir);

    float shadow = ShadowCalculation(fragPosLightSpace, norm, lightDir);
    //shadow = 1.0;

    return (ambient + (1.0 - shadow) * (diffuse + specular));
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

    vec3 result = getDirectionLighting(directionalLight, norm, viewDir, fragPosLightSpace);

    for(int i = 0; i < lightsNum; i++)
        result += getPointLighting(pointLights[i], material, fragPos, norm, viewDir);

    FragColor = vec4(result, 1.0);
}