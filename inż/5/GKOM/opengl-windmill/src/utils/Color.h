#ifndef WIATRAK_COLOR_H
#define WIATRAK_COLOR_H

#include <algorithm>
#include <cmath>

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
};

static float convertIntToFloatColorValue(int colorValue)
{
    return float(colorValue) / 255.0f;
}

static int convertFloatToIntColorValue(float colorValue)
{
    return std::max(0, std::min(255, (int) std::floor(colorValue * 256.0f)));
}


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
