#ifndef WIATRAK_TRIANGLE_H
#define WIATRAK_TRIANGLE_H

struct Triangle
{
    unsigned int v1_id;
    unsigned int v2_id;
    unsigned int v3_id;

    Triangle(unsigned int v1, unsigned int v2, unsigned int v3) : v1_id(v1), v2_id(v2), v3_id(v3)
    {}
};


#endif //WIATRAK_TRIANGLE_H
