#ifndef WIATRAK_COLOR_H
#define WIATRAK_COLOR_H

#include <algorithm>
#include <cmath>
#include "glm/vec4.hpp"

static float convertIntToFloatColorValue(int colorValue)
{
    return float(colorValue) / 255.0f;
}

static int convertFloatToIntColorValue(float colorValue)
{
    return std::max(0, std::min(255, (int) std::floor(colorValue * 256.0f)));
}

struct ColorInt
{
    int red;
    int green;
    int blue;
    float alpha;

    ColorInt(int red, int green, int blue, float alpha = 1.0f) : red(red), green(green), blue(blue), alpha(alpha)
    {}
};

struct ColorFloat
{
    float red;
    float green;
    float blue;
    float alpha;

    ColorFloat(float red, float green, float blue, float alpha = 1.0f) : red(red), green(green), blue(blue),
                                                                         alpha(alpha)
    {}

    ColorFloat(ColorInt colorInt) : red(convertIntToFloatColorValue(colorInt.red)),
                                    green(convertIntToFloatColorValue(colorInt.green)),
                                    blue(convertIntToFloatColorValue(colorInt.blue)),
                                    alpha(colorInt.alpha) {}

    glm::vec4 getVec() const { return glm::vec4(red, green, blue, alpha); }
};

static ColorFloat convertColorIntToFloat(const ColorInt &c)
{
    return {convertIntToFloatColorValue(c.red),
            convertIntToFloatColorValue(c.green),
            convertIntToFloatColorValue(c.blue),
            c.alpha};
}

static ColorInt convertColorFloatToInt(const ColorFloat &c)
{
    return {convertFloatToIntColorValue(c.red),
            convertFloatToIntColorValue(c.green),
            convertFloatToIntColorValue(c.blue),
            c.alpha};
}

#endif //WIATRAK_COLOR_H
