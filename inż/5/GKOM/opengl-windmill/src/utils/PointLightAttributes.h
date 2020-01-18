#ifndef WIATRAK_POINTLIGHTATTRIBUTES_H
#define WIATRAK_POINTLIGHTATTRIBUTES_H


struct PointLightAttributes
{
    ColorFloat color; // light color

    // intensities of particular light components
    float ambientIntensity;
    float diffuseIntensity;
    float specularIntensity;

    // attenuation values
    float constant;
    float linear;
    float quadratic;

    PointLightAttributes(ColorFloat color,
                         float ambientIntensity,
                         float diffuseIntensity,
                         float specularIntensity,
                         float constant,
                         float linear,
                         float quadratic) : color(color),
                                            ambientIntensity(ambientIntensity),
                                            diffuseIntensity(diffuseIntensity),
                                            specularIntensity(specularIntensity),
                                            constant(constant),
                                            linear(linear),
                                            quadratic(quadratic)
    {}
};


#endif //WIATRAK_POINTLIGHTATTRIBUTES_H
